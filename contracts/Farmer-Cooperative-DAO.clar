(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-already-member (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-proposal-not-found (err u104))
(define-constant err-already-voted (err u105))
(define-constant err-proposal-ended (err u106))
(define-constant err-insufficient-funds (err u107))
(define-constant err-proposal-not-approved (err u108))

(define-data-var membership-fee uint u1000)
(define-data-var proposal-duration uint u144)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)

(define-map members principal bool)
(define-map member-contributions principal uint)
(define-map member-reputation principal uint)

(define-map proposals uint {
    creator: principal,
    title: (string-ascii 50),
    description: (string-ascii 500),
    amount: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    end-block: uint,
    executed: bool
})

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-data-var proposal-count uint u0)

(define-private (get-voting-power (member principal))
    (let ((reputation (default-to u0 (map-get? member-reputation member))))
        (+ u1 (/ reputation u100))))

(define-private (award-reputation (member principal) (points uint))
    (let ((current-rep (default-to u0 (map-get? member-reputation member))))
        (map-set member-reputation member (+ current-rep points))))

(define-public (join-cooperative)
    (let ((payment (stx-transfer? (var-get membership-fee) tx-sender (as-contract tx-sender))))
        (asserts! (is-ok payment) err-invalid-amount)
        (asserts! (not (default-to false (map-get? members tx-sender))) err-already-member)
        (map-set members tx-sender true)
        (map-set member-reputation tx-sender u50)
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
            end-block: (+ stacks-block-height (var-get proposal-duration)),
            executed: false
        })
        (var-set proposal-count proposal-id)
        (award-reputation tx-sender u25)
        (ok proposal-id)))

(define-public (vote (proposal-id uint) (vote-for bool))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
          (voting-power (get-voting-power tx-sender)))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (asserts! (not (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender}))) err-already-voted)
        (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-ended)
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (if vote-for
            (map-set proposals proposal-id (merge proposal {votes-for: (+ (get votes-for proposal) voting-power)}))
            (map-set proposals proposal-id (merge proposal {votes-against: (+ (get votes-against proposal) voting-power)}))
        )
        (award-reputation tx-sender u10)
        (ok true)))

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found)))
        (asserts! (> stacks-block-height (get end-block proposal)) err-proposal-ended)
        (asserts! (not (get executed proposal)) err-proposal-not-approved)
        (asserts! (> (get votes-for proposal) (get votes-against proposal)) err-proposal-not-approved)
        (asserts! (>= (var-get treasury-balance) (get amount proposal)) err-insufficient-funds)
        
        (let ((transfer-result (as-contract (stx-transfer? (get amount proposal) tx-sender (get creator proposal)))))
            (asserts! (is-ok transfer-result) err-insufficient-funds)
            (var-set treasury-balance (- (var-get treasury-balance) (get amount proposal)))
            (map-set proposals proposal-id (merge proposal {executed: true, status: "executed"}))
            (ok true))))

(define-public (contribute-to-treasury (amount uint))
    (let ((payment (stx-transfer? amount tx-sender (as-contract tx-sender))))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (asserts! (is-ok payment) err-invalid-amount)
        (let ((current-contribution (default-to u0 (map-get? member-contributions tx-sender))))
            (map-set member-contributions tx-sender (+ current-contribution amount))
            (var-set treasury-balance (+ (var-get treasury-balance) amount))
            (award-reputation tx-sender (/ amount u10))
            (ok true))))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-member-status (address principal))
    (default-to false (map-get? members address)))

(define-read-only (get-member-reputation (address principal))
    (default-to u0 (map-get? member-reputation address)))

(define-read-only (get-member-voting-power (address principal))
    (if (default-to false (map-get? members address))
        (get-voting-power address)
        u0))

(define-read-only (get-dao-stats)
    (ok {
        total-members: (var-get total-members),
        treasury-balance: (var-get treasury-balance),
        total-proposals: (var-get proposal-count)
    }))

(define-read-only (can-execute-proposal (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and 
            (> stacks-block-height (get end-block proposal))
            (not (get executed proposal))
            (> (get votes-for proposal) (get votes-against proposal))
            (>= (var-get treasury-balance) (get amount proposal)))
        false))
