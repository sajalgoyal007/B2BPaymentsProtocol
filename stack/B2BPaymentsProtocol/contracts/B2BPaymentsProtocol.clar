;; B2BPayments Protocol
;; Cross-border business payments with reduced fees and faster settlement times
;; A decentralized protocol for efficient B2B international transactions

;; Define the payment token (can be customized for different currencies)
(define-fungible-token b2b-payment-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-invalid-amount (err u101))
(define-constant err-insufficient-balance (err u102))
(define-constant err-payment-not-found (err u103))
(define-constant err-payment-already-settled (err u104))
(define-constant err-invalid-business (err u105))

;; Protocol fee (0.5% = 50 basis points out of 10000)
(define-constant protocol-fee-basis-points u50)
(define-constant basis-points-denominator u10000)

;; Data structures
(define-map business-registrations 
  principal 
  {
    business-name: (string-ascii 64),
    country-code: (string-ascii 3),
    registered-at: uint,
    is-verified: bool
  })

(define-map cross-border-payments
  uint
  {
    payment-id: uint,
    sender: principal,
    recipient: principal,
    amount: uint,
    fee: uint,
    currency-pair: (string-ascii 10),
    status: (string-ascii 20),
    created-at: uint,
    settled-at: (optional uint)
  })

;; State variables
(define-data-var next-payment-id uint u1)
(define-data-var total-volume uint u0)
(define-data-var protocol-treasury uint u0)

;; Function 1: Initialize Cross-Border Payment
;; Creates a new payment request with automatic fee calculation and escrow
(define-public (initiate-cross-border-payment 
    (recipient principal) 
    (amount uint) 
    (currency-pair (string-ascii 10)))
  (let
    (
      (payment-id (var-get next-payment-id))
      (fee (/ (* amount protocol-fee-basis-points) basis-points-denominator))
      (total-amount (+ amount fee))
    )
    ;; Validations
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (not (is-eq tx-sender recipient)) err-invalid-business)
    
    ;; Check sender is registered business
    (asserts! (is-some (map-get? business-registrations tx-sender)) err-unauthorized)
    
    ;; Check recipient is registered business
    (asserts! (is-some (map-get? business-registrations recipient)) err-unauthorized)
    
    ;; Check sufficient balance (assuming STX for now, can be extended for other tokens)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) err-insufficient-balance)
    
    ;; Transfer funds to contract (escrow)
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    ;; Store payment details
    (map-set cross-border-payments payment-id
      {
        payment-id: payment-id,
        sender: tx-sender,
        recipient: recipient,
        amount: amount,
        fee: fee,
        currency-pair: currency-pair,
        status: "pending",
        created-at: stacks-block-height,
        settled-at: none
      })
    
    ;; Update state
    (var-set next-payment-id (+ payment-id u1))
    (var-set total-volume (+ (var-get total-volume) amount))
    (var-set protocol-treasury (+ (var-get protocol-treasury) fee))
    
    ;; Print payment initiated event
    (print {
      event: "payment-initiated",
      payment-id: payment-id,
      sender: tx-sender,
      recipient: recipient,
      amount: amount,
      fee: fee,
      currency-pair: currency-pair
    })
    
    (ok payment-id)))

;; Function 2: Settle Payment
;; Completes the payment by releasing funds to recipient and updating status
(define-public (settle-payment (payment-id uint))
  (let
    (
      (payment-data (unwrap! (map-get? cross-border-payments payment-id) err-payment-not-found))
      (amount (get amount payment-data))
      (recipient (get recipient payment-data))
      (status (get status payment-data))
    )
    ;; Validations
    (asserts! (is-eq status "pending") err-payment-already-settled)
    
    ;; Only recipient can settle the payment (in real implementation, this could be automated)
    (asserts! (is-eq tx-sender recipient) err-unauthorized)
    
    ;; Transfer amount to recipient (fee stays in contract)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update payment status
    (map-set cross-border-payments payment-id
      (merge payment-data {
        status: "settled",
        settled-at: (some stacks-block-height)
      }))
    
    ;; Print settlement event
    (print {
      event: "payment-settled",
      payment-id: payment-id,
      recipient: recipient,
      amount: amount,
      settled-at: stacks-block-height
    })
    
    (ok true)))

;; Read-only functions for querying payment data
(define-read-only (get-payment-details (payment-id uint))
  (map-get? cross-border-payments payment-id))

(define-read-only (get-business-info (business principal))
  (map-get? business-registrations business))

(define-read-only (get-protocol-stats)
  (ok {
    total-volume: (var-get total-volume),
    protocol-treasury: (var-get protocol-treasury),
    next-payment-id: (var-get next-payment-id)
  }))

;; Business registration helper (can be called by businesses to register)
(define-public (register-business (business-name (string-ascii 64)) (country-code (string-ascii 3)))
  (begin
    (map-set business-registrations tx-sender
      {
        business-name: business-name,
        country-code: country-code,
        registered-at: stacks-block-height,
        is-verified: false
      })
    (ok true)))