# 🏠 Rent Payment Smart Contract

A decentralized rent payment system built on Stacks blockchain using Clarity.

## 🌟 Features

- ✅ Property registration and management
- 💰 Security deposit handling
- 📅 Automated rent payment tracking
- ⏱️ Late payment detection with fees
- 📝 Eviction process management
- 🔒 Secure on-chain transactions

## 📋 Contract Overview

This smart contract enables landlords and tenants to manage rental agreements on the blockchain with the following capabilities:

- **Property owners** can register properties, set rent amounts, and manage tenants
- **Tenants** can pay security deposits and monthly rent
- **Automatic late fee** calculation for overdue payments
- **Eviction process** with proper documentation and security deposit handling

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic understanding of Stacks blockchain and Clarity

### Installation

1. Create a new Clarinet project:

```bash
clarinet new rent-payment-contract
```

2. Navigate to the project directory:

```bash
cd rent-payment-contract
```

3. Replace the contents of `contracts/rent.clar` with the contract code

## 📖 Usage Guide

### For Property Owners

1. **Register a property**:
   - Property ID: Unique identifier for the property
   - Rent amount: Monthly rent in STX
   - Security deposit: Required deposit in STX
   - Payment day: Day of the month when rent is due (1-28)

2. **Register a tenant**:
   - Assign a tenant to a specific property

3. **Manage evictions** (if necessary):
   - Initiate eviction process for non-payment
   - Cancel eviction if payment is received
   - Complete eviction to remove tenant from property

4. **Return security deposit** when tenant moves out

### For Tenants

1. **Pay security deposit** when moving in
2. **Pay monthly rent** before the due date to avoid late fees
3. **View payment history** and current status

## 🔍 Contract Functions

### Property Management
- `register-property`: Register a new property with rent details
- `register-tenant`: Assign a tenant to a property

### Payments
- `pay-security-deposit`: Pay the required security deposit
- `pay-rent`: Pay the monthly rent (with automatic late fee calculation)

### Eviction Process
- `initiate-eviction`: Start the eviction process for non-payment
- `cancel-eviction`: Cancel an ongoing eviction
- `complete-eviction`: Finalize the eviction process
- `return-security-deposit`: Return the security deposit to the tenant

### Read-Only Functions
- `get-property`: Get property details
- `get-tenant-info`: Get tenant information
- `is-payment-late`: Check if a tenant's payment is late
- `calculate-late-fee`: Calculate the late fee for a payment

## ⚠️ Important Notes

- All payments are made in STX
- Late fees are calculated as a percentage of the rent amount
- Grace period for late payments is 5 days
- Security deposits are held by the contract until returned

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📜 License

This project is licensed under the MIT License.

