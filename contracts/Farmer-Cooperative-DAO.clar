(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-already-member (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-proposal-not-found (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-proposal-ended (err u106))

(define-data-var membership-fee uint u1000)
(define-data-var proposal-duration uint u144)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)

(define-map members principal bool)
(define-map member-contributions principal uint)

(define-map proposals uint {
    creator: principal,
    title: (string-ascii 50),
    description: (string-ascii 500),
    amount: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    end-block: uint
})

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-data-var proposal-count uint u0)

(define-public (join-cooperative)
    (let ((payment (stx-transfer? (var-get membership-fee) tx-sender (as-contract tx-sender))))
        (asserts! (is-ok payment) err-invalid-amount)
        (asserts! (not (default-to false (map-get? members tx-sender))) err-already-member)
        (map-set members tx-sender true)
        (var-set total-members (+ (var-get total-members) u1))
        (var-set treasury-balance (+ (var-get treasury-balance) (var-get membership-fee)))
        (ok true)))
(define-public (create-proposal (title (string-ascii 50)) (description (string-ascii 500)) (amount uint))
    (let ((proposal-id (+ (var-get proposal-count) u1)))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (map-set proposals proposal-id {
            creator: tx-sender,
            title: title,
            description: description,
            amount: amount,
            votes-for: u0,
            votes-against: u0,
            status: "active",
            end-block: (+ stacks-block-height (var-get proposal-duration))
        })
        (var-set proposal-count proposal-id)
        (ok proposal-id)))

(define-public (vote (proposal-id uint) (vote-for bool))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found)))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (asserts! (not (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))) err-already-voted)
        (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-ended)
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (if vote-for
            (map-set proposals proposal-id (merge proposal {votes-for: (+ (get votes-for proposal) u1)}))
            (map-set proposals proposal-id (merge proposal {votes-against: (+ (get votes-against proposal) u1)}))
        )
        (ok true)))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-member-status (address principal))
    (default-to false (map-get? members address)))

(define-read-only (get-dao-stats)
    (ok {
        total-members: (var-get total-members),
        treasury-balance: (var-get treasury-balance),
        total-proposals: (var-get proposal-count)
    }))
