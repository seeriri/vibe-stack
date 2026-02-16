;; Vibe Stack - Sentiment-Driven DAO Governance
;; A novel governance system using sentiment analysis and dynamic consensus

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-voted (err u102))
(define-constant err-proposal-closed (err u103))
(define-constant err-insufficient-stake (err u104))
(define-constant err-cooling-off (err u105))

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var min-stake-amount uint u1000000) ;; 1 STX in microSTX
(define-data-var cooling-off-period uint u144) ;; ~24 hours in blocks

;; Data Maps
(define-map proposals
    { proposal-id: uint }
    {
        creator: principal,
        title: (string-ascii 256),
        budget-requested: uint,
        vibe-score: int,
        total-sentiment: int,
        vote-count: uint,
        status: (string-ascii 20),
        created-at: uint,
        ends-at: uint,
        budget-released: uint
    }
)

(define-map votes
    { proposal-id: uint, voter: principal }
    {
        sentiment: int, ;; -100 to +100
        confidence: uint, ;; 0 to 100
        stake-amount: uint,
        voted-at: uint
    }
)

(define-map reputation-scores
    { voter: principal }
    {
        total-votes: uint,
        accuracy-score: uint, ;; 0 to 1000
        weighted-contribution: uint
    }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-reputation (voter principal))
    (default-to 
        { total-votes: u0, accuracy-score: u500, weighted-contribution: u0 }
        (map-get? reputation-scores { voter: voter })
    )
)

(define-read-only (calculate-vibe-score (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) (err u0)))
        (vote-count (get vote-count proposal))
        (total-sentiment (get total-sentiment proposal))
    )
        (if (> vote-count u0)
            (ok (/ total-sentiment (to-int vote-count)))
            (ok 0)
        )
    )
)

(define-read-only (get-dynamic-quorum (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) (err u0)))
        (vibe-score (get vibe-score proposal))
        (base-quorum u10)
    )
        ;; Lower quorum for high positive vibe, higher for negative
        (ok (if (> vibe-score 50)
            (- base-quorum u3)
            (if (< vibe-score -50)
                (+ base-quorum u5)
                base-quorum
            )
        ))
    )
)

;; Public functions
(define-public (create-proposal (title (string-ascii 256)) (budget-requested uint) (duration uint))
    (let (
        (proposal-id (+ (var-get proposal-nonce) u1))
        (current-height block-height)
    )
        (map-set proposals
            { proposal-id: proposal-id }
            {
                creator: tx-sender,
                title: title,
                budget-requested: budget-requested,
                vibe-score: 0,
                total-sentiment: 0,
                vote-count: u0,
                status: "active",
                created-at: current-height,
                ends-at: (+ current-height duration),
                budget-released: u0
            }
        )
        (var-set proposal-nonce proposal-id)
        (ok proposal-id)
    )
)

(define-public (cast-sentiment-vote 
    (proposal-id uint) 
    (sentiment int) 
    (confidence uint)
    (stake-amount uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) err-not-found))
        (voter-rep (get-reputation tx-sender))
        (current-height block-height)
    )
        ;; Validations
        (asserts! (is-none (get-vote proposal-id tx-sender)) err-already-voted)
        (asserts! (is-eq (get status proposal) "active") err-proposal-closed)
        (asserts! (>= stake-amount (var-get min-stake-amount)) err-insufficient-stake)
        (asserts! (< current-height (get ends-at proposal)) err-proposal-closed)
        (asserts! (and (>= sentiment -100) (<= sentiment 100)) (err u106))
        (asserts! (and (>= confidence u0) (<= confidence u100)) (err u107))
        
        ;; Calculate weighted sentiment
        (let (
            (reputation-weight (/ (get accuracy-score voter-rep) u100))
            (confidence-weight (/ confidence u100))
            (stake-weight (/ stake-amount (var-get min-stake-amount)))
            (weighted-sentiment (* sentiment (to-int (* (* reputation-weight confidence-weight) stake-weight))))
        )
            ;; Record vote
            (map-set votes
                { proposal-id: proposal-id, voter: tx-sender }
                {
                    sentiment: sentiment,
                    confidence: confidence,
                    stake-amount: stake-amount,
                    voted-at: current-height
                }
            )
            
            ;; Update proposal
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal {
                    total-sentiment: (+ (get total-sentiment proposal) weighted-sentiment),
                    vote-count: (+ (get vote-count proposal) u1),
                    vibe-score: (/ (+ (get total-sentiment proposal) weighted-sentiment) 
                                   (to-int (+ (get vote-count proposal) u1)))
                })
            )
            
            ;; Update voter reputation
            (map-set reputation-scores
                { voter: tx-sender }
                (merge voter-rep {
                    total-votes: (+ (get total-votes voter-rep) u1),
                    weighted-contribution: (+ (get weighted-contribution voter-rep) stake-amount)
                })
            )
            
            (ok true)
        )
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) err-not-found))
        (current-height block-height)
        (vibe-score (get vibe-score proposal))
        (cooling-off-end (+ (get ends-at proposal) (var-get cooling-off-period)))
    )
        (asserts! (is-eq (get status proposal) "active") err-proposal-closed)
        (asserts! (>= current-height (get ends-at proposal)) (err u108))
        (asserts! (>= current-height cooling-off-end) err-cooling-off)
        
        (let (
            (new-status (if (> vibe-score 0) "approved" "rejected"))
            ;; Graduated budget release based on vibe score
            (release-percentage (if (> vibe-score 75) u100
                                (if (> vibe-score 50) u75
                                (if (> vibe-score 25) u50
                                (if (> vibe-score 0) u25
                                u0)))))
            (budget-to-release (/ (* (get budget-requested proposal) release-percentage) u100))
        )
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal {
                    status: new-status,
                    budget-released: budget-to-release
                })
            )
            (ok { status: new-status, budget-released: budget-to-release })
        )
    )
)

(define-public (update-voter-accuracy (voter principal) (new-accuracy uint))
    (let (
        (current-rep (get-reputation voter))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-accuracy u1000) (err u109))
        
        (map-set reputation-scores
            { voter: voter }
            (merge current-rep {
                accuracy-score: new-accuracy
            })
        )
        (ok true)
    )
)

;; Administrative functions
(define-public (set-min-stake (new-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set min-stake-amount new-amount)
        (ok true)
    )
)

(define-public (set-cooling-off-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set cooling-off-period new-period)
        (ok true)
    )
)

;; Initialize
(begin
    (var-set proposal-nonce u0)
)