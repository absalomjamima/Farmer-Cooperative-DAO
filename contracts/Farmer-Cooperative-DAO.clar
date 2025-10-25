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
(define-constant err-resource-not-found (err u109))
(define-constant err-resource-claimed (err u110))
(define-constant err-cannot-claim-own-resource (err u111))
(define-constant err-not-guardian (err u112))
(define-constant err-multisig-not-found (err u113))
(define-constant err-already-signed (err u114))
(define-constant err-insufficient-signatures (err u115))
(define-constant err-multisig-expired (err u116))
(define-constant err-stake-not-found (err u117))
(define-constant err-stake-locked (err u118))
(define-constant err-no-rewards (err u119))

(define-data-var membership-fee uint u1000)
(define-data-var proposal-duration uint u144)
(define-data-var total-members uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var multisig-threshold uint u5000)
(define-data-var multisig-count uint u0)
(define-data-var multisig-duration uint u1008)
(define-data-var stake-count uint u0)
(define-data-var total-staked uint u0)
(define-data-var reward-pool uint u0)

(define-map members principal bool)
(define-map member-contributions principal uint)
(define-map member-reputation principal uint)
(define-map treasury-guardians principal bool)
(define-map multisig-transactions uint {
    initiator: principal,
    recipient: principal,
    amount: uint,
    purpose: (string-ascii 100),
    signatures: (list 10 principal),
    signatures-count: uint,
    executed: bool,
    expires-at: uint
})
(define-map multisig-signatures {tx-id: uint, signer: principal} bool)
(define-map stakes uint {
    staker: principal,
    amount: uint,
    locked-until: uint,
    created-at: uint,
    active: bool
})
(define-map staker-stakes principal (list 20 uint))

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

(define-map shared-resources uint {
    provider: principal,
    resource-type: (string-ascii 30),
    description: (string-ascii 200),
    quantity: uint,
    claimed: bool,
    claimer: (optional principal),
    created-at: uint
})

(define-data-var proposal-count uint u0)
(define-data-var resource-count uint u0)

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

(define-public (share-resource (resource-type (string-ascii 30)) (description (string-ascii 200)) (quantity uint))
    (let ((resource-id (+ (var-get resource-count) u1)))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (asserts! (> quantity u0) err-invalid-amount)
        (map-set shared-resources resource-id {
            provider: tx-sender,
            resource-type: resource-type,
            description: description,
            quantity: quantity,
            claimed: false,
            claimer: none,
            created-at: stacks-block-height
        })
        (var-set resource-count resource-id)
        (award-reputation tx-sender u15)
        (ok resource-id)))

(define-public (claim-resource (resource-id uint))
    (let ((resource (unwrap! (map-get? shared-resources resource-id) err-resource-not-found)))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (asserts! (not (get claimed resource)) err-resource-claimed)
        (asserts! (not (is-eq tx-sender (get provider resource))) err-cannot-claim-own-resource)
        (map-set shared-resources resource-id (merge resource {
            claimed: true,
            claimer: (some tx-sender)
        }))
        (award-reputation tx-sender u5)
        (award-reputation (get provider resource) u10)
        (ok true)))

(define-read-only (get-resource (resource-id uint))
    (map-get? shared-resources resource-id))

(define-read-only (get-available-resources)
    (ok {
        total-resources: (var-get resource-count),
        active-resources: u0
    }))

(define-read-only (is-resource-available (resource-id uint))
    (match (map-get? shared-resources resource-id)
        resource (not (get claimed resource))
        false))

(define-public (appoint-treasury-guardian (guardian principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (default-to false (map-get? members guardian)) err-not-member)
        (map-set treasury-guardians guardian true)
        (ok true)))

(define-public (revoke-treasury-guardian (guardian principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-delete treasury-guardians guardian)
        (ok true)))

(define-public (create-multisig-transaction (recipient principal) (amount uint) (purpose (string-ascii 100)))
    (let ((tx-id (+ (var-get multisig-count) u1)))
        (asserts! (default-to false (map-get? treasury-guardians tx-sender)) err-not-guardian)
        (asserts! (>= amount (var-get multisig-threshold)) err-invalid-amount)
        (asserts! (>= (var-get treasury-balance) amount) err-insufficient-funds)
        (map-set multisig-transactions tx-id {
            initiator: tx-sender,
            recipient: recipient,
            amount: amount,
            purpose: purpose,
            signatures: (list tx-sender),
            signatures-count: u1,
            executed: false,
            expires-at: (+ stacks-block-height (var-get multisig-duration))
        })
        (map-set multisig-signatures {tx-id: tx-id, signer: tx-sender} true)
        (var-set multisig-count tx-id)
        (ok tx-id)))

(define-public (sign-multisig-transaction (tx-id uint))
    (let ((transaction (unwrap! (map-get? multisig-transactions tx-id) err-multisig-not-found)))
        (asserts! (default-to false (map-get? treasury-guardians tx-sender)) err-not-guardian)
        (asserts! (not (get executed transaction)) err-multisig-expired)
        (asserts! (<= stacks-block-height (get expires-at transaction)) err-multisig-expired)
        (asserts! (not (default-to false (map-get? multisig-signatures {tx-id: tx-id, signer: tx-sender}))) err-already-signed)
        (let ((updated-signatures (unwrap! (as-max-len? (append (get signatures transaction) tx-sender) u10) err-invalid-amount))
              (new-count (+ (get signatures-count transaction) u1)))
            (map-set multisig-transactions tx-id (merge transaction {
                signatures: updated-signatures,
                signatures-count: new-count
            }))
            (map-set multisig-signatures {tx-id: tx-id, signer: tx-sender} true)
            (ok true))))

(define-public (execute-multisig-transaction (tx-id uint))
    (let ((transaction (unwrap! (map-get? multisig-transactions tx-id) err-multisig-not-found)))
        (asserts! (not (get executed transaction)) err-multisig-expired)
        (asserts! (<= stacks-block-height (get expires-at transaction)) err-multisig-expired)
        (asserts! (>= (get signatures-count transaction) u3) err-insufficient-signatures)
        (asserts! (>= (var-get treasury-balance) (get amount transaction)) err-insufficient-funds)
        (let ((transfer-result (as-contract (stx-transfer? (get amount transaction) tx-sender (get recipient transaction)))))
            (asserts! (is-ok transfer-result) err-insufficient-funds)
            (var-set treasury-balance (- (var-get treasury-balance) (get amount transaction)))
            (map-set multisig-transactions tx-id (merge transaction {executed: true}))
            (ok true))))

(define-read-only (get-multisig-transaction (tx-id uint))
    (map-get? multisig-transactions tx-id))

(define-read-only (is-treasury-guardian (address principal))
    (default-to false (map-get? treasury-guardians address)))

(define-read-only (get-multisig-threshold)
    (var-get multisig-threshold))

(define-read-only (can-execute-multisig (tx-id uint))
    (match (map-get? multisig-transactions tx-id)
        transaction (and
            (not (get executed transaction))
            (<= stacks-block-height (get expires-at transaction))
            (>= (get signatures-count transaction) u3)
            (>= (var-get treasury-balance) (get amount transaction)))
        false))

(define-public (stake-tokens (amount uint) (lock-duration uint))
    (let ((stake-id (+ (var-get stake-count) u1))
          (locked-until (+ stacks-block-height lock-duration)))
        (asserts! (default-to false (map-get? members tx-sender)) err-not-member)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= lock-duration u144) err-invalid-amount)
        (let ((payment (stx-transfer? amount tx-sender (as-contract tx-sender))))
            (asserts! (is-ok payment) err-invalid-amount)
            (map-set stakes stake-id {
                staker: tx-sender,
                amount: amount,
                locked-until: locked-until,
                created-at: stacks-block-height,
                active: true
            })
            (let ((current-stakes (default-to (list) (map-get? staker-stakes tx-sender))))
                (map-set staker-stakes tx-sender (unwrap! (as-max-len? (append current-stakes stake-id) u20) err-invalid-amount)))
            (var-set stake-count stake-id)
            (var-set total-staked (+ (var-get total-staked) amount))
            (let ((reputation-reward (/ (* amount lock-duration) u1000)))
                (award-reputation tx-sender reputation-reward))
            (ok stake-id))))

(define-public (unstake-tokens (stake-id uint))
    (let ((stake (unwrap! (map-get? stakes stake-id) err-stake-not-found)))
        (asserts! (is-eq tx-sender (get staker stake)) err-not-member)
        (asserts! (get active stake) err-stake-not-found)
        (asserts! (> stacks-block-height (get locked-until stake)) err-stake-locked)
        (let ((amount (get amount stake))
              (duration (- (get locked-until stake) (get created-at stake)))
              (reward (calculate-stake-reward amount duration)))
            (try! (as-contract (stx-transfer? amount tx-sender (get staker stake))))
            (if (> reward u0)
                (begin
                    (asserts! (>= (var-get reward-pool) reward) err-insufficient-funds)
                    (try! (as-contract (stx-transfer? reward tx-sender (get staker stake))))
                    (var-set reward-pool (- (var-get reward-pool) reward)))
                true)
            (map-set stakes stake-id (merge stake {active: false}))
            (var-set total-staked (- (var-get total-staked) amount))
            (ok true))))

(define-private (calculate-stake-reward (amount uint) (duration uint))
    (let ((base-reward (/ (* amount duration) u100000)))
        (if (> (var-get reward-pool) u0)
            (if (> base-reward (var-get reward-pool))
                (var-get reward-pool)
                base-reward)
            u0)))

(define-public (fund-reward-pool (amount uint))
    (let ((payment (stx-transfer? amount tx-sender (as-contract tx-sender))))
        (asserts! (is-ok payment) err-invalid-amount)
        (var-set reward-pool (+ (var-get reward-pool) amount))
        (ok true)))

(define-read-only (get-stake (stake-id uint))
    (map-get? stakes stake-id))

(define-read-only (get-staker-stakes (staker principal))
    (default-to (list) (map-get? staker-stakes staker)))

(define-read-only (get-staking-stats)
    (ok {
        total-staked: (var-get total-staked),
        total-stakes: (var-get stake-count),
        reward-pool: (var-get reward-pool)
    }))

(define-read-only (can-unstake (stake-id uint))
    (match (map-get? stakes stake-id)
        stake (and
            (get active stake)
            (> stacks-block-height (get locked-until stake)))
        false))
