# 🌾 Farmer Cooperative DAO

A decentralized autonomous organization (DAO) for farming communities to make collective decisions and manage shared resources.

## 🎯 Features

- 👥 Membership management
- 📜 Proposal creation and voting
- 💰 Treasury management
- 🗳️ Democratic decision-making

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks wallet

### Usage

1. Join the cooperative by calling `join-cooperative` and paying the membership fee
2. Create proposals using `create-proposal`
3. Vote on active proposals with `vote`
4. Check proposal status with `get-proposal`

## 📊 Contract Functions

### Public Functions

- `join-cooperative`: Join the DAO by paying membership fee
- `create-proposal`: Create a new proposal
- `vote`: Vote on an active proposal

### Read-Only Functions

- `get-proposal`: Get proposal details
- `get-member-status`: Check membership status
- `get-dao-stats`: Get DAO statistics

## 💡 Example Usage

```clarity
;; Join the cooperative
(contract-call? .farmer-cooperative-dao join-cooperative)

;; Create a proposal
(contract-call? .farmer-cooperative-dao create-proposal "Bulk Fertilizer Purchase" "Purchase 1000kg of organic fertilizer" u50000)

;; Vote on proposal
(contract-call? .farmer-cooperative-dao vote u1 true)
```

## 🔒 Security

- Only members can create proposals and vote
- Voting period is time-limited
- One vote per member per proposal
```

