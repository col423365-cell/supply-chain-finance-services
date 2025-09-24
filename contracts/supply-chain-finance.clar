;; Supply Chain Finance Platform
;; Working capital platform with invoice financing, payment optimization, risk assessment, and cash flow management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-company-not-found (err u101))
(define-constant err-invoice-not-found (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-already-financed (err u105))

;; Data Variables
(define-data-var next-company-id uint u1)
(define-data-var next-invoice-id uint u1)
(define-data-var total-liquidity uint u0)

;; Data Maps
(define-map companies
  uint
  {
    name: (string-utf8 100),
    credit-rating: uint,
    payment-terms: uint,
    risk-score: uint,
    total-financed: uint
  }
)

(define-map invoices
  uint
  {
    company-id: uint,
    amount: uint,
    due-date: uint,
    status: (string-ascii 20),
    financing-rate: uint,
    advance-amount: uint
  }
)

(define-map payment-terms
  { company-id: uint, supplier-id: uint }
  {
    standard-terms: uint,
    early-discount: uint,
    extended-terms: uint,
    optimization-score: uint
  }
)

(define-map risk-assessments
  uint
  {
    company-id: uint,
    assessment-date: uint,
    financial-stability: uint,
    payment-history: uint,
    market-conditions: uint,
    overall-risk: uint
  }
)

(define-map cash-flows
  { company-id: uint, period: uint }
  {
    inflows: uint,
    outflows: uint,
    net-position: uint,
    financing-needs: uint
  }
)

;; Public Functions

;; Company Registration
(define-public (register-company (name (string-utf8 100)) (credit-rating uint) (payment-days uint))
  (let ((company-id (var-get next-company-id)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (<= credit-rating u100) err-invalid-amount)
    (map-set companies company-id {
      name: name,
      credit-rating: credit-rating,
      payment-terms: payment-days,
      risk-score: u0,
      total-financed: u0
    })
    (var-set next-company-id (+ company-id u1))
    (ok company-id)
  )
)

;; Invoice Registration
(define-public (submit-invoice (company-id uint) (amount uint) (due-date uint) (financing-rate uint))
  (let ((invoice-id (var-get next-invoice-id)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-some (map-get? companies company-id)) err-company-not-found)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set invoices invoice-id {
      company-id: company-id,
      amount: amount,
      due-date: due-date,
      status: "pending",
      financing-rate: financing-rate,
      advance-amount: u0
    })
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)
  )
)

;; Invoice Financing
(define-public (finance-invoice (invoice-id uint) (advance-percentage uint))
  (let 
    (
      (invoice (unwrap! (map-get? invoices invoice-id) err-invoice-not-found))
      (advance-amount (/ (* (get amount invoice) advance-percentage) u100))
    )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-eq (get status invoice) "pending") err-already-financed)
    (asserts! (<= advance-percentage u90) err-invalid-amount)
    (asserts! (>= (var-get total-liquidity) advance-amount) err-insufficient-funds)
    
    ;; Update invoice status and advance amount
    (map-set invoices invoice-id
      (merge invoice { 
        status: "financed",
        advance-amount: advance-amount
      })
    )
    
    ;; Update company total financed
    (let ((company (unwrap-panic (map-get? companies (get company-id invoice)))))
      (map-set companies (get company-id invoice)
        (merge company { total-financed: (+ (get total-financed company) advance-amount) })
      )
    )
    
    ;; Reduce liquidity pool
    (var-set total-liquidity (- (var-get total-liquidity) advance-amount))
    (ok advance-amount)
  )
)

;; Payment Terms Optimization
(define-public (optimize-payment-terms (company-id uint) (supplier-id uint) (standard-terms uint) (early-discount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-some (map-get? companies company-id)) err-company-not-found)
    (let ((optimization-score (+ early-discount (/ standard-terms u10))))
      (map-set payment-terms { company-id: company-id, supplier-id: supplier-id } {
        standard-terms: standard-terms,
        early-discount: early-discount,
        extended-terms: (+ standard-terms u30),
        optimization-score: optimization-score
      })
    )
    (ok true)
  )
)

;; Risk Assessment
(define-public (assess-risk (company-id uint) (financial-stability uint) (payment-history uint) (market-conditions uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-some (map-get? companies company-id)) err-company-not-found)
    (asserts! (<= financial-stability u100) err-invalid-amount)
    (asserts! (<= payment-history u100) err-invalid-amount)
    (asserts! (<= market-conditions u100) err-invalid-amount)
    
    (let 
      (
        (overall-risk (/ (+ financial-stability payment-history market-conditions) u3))
        (assessment-id (var-get next-company-id))
      )
      (map-set risk-assessments assessment-id {
        company-id: company-id,
        assessment-date: burn-block-height,
        financial-stability: financial-stability,
        payment-history: payment-history,
        market-conditions: market-conditions,
        overall-risk: overall-risk
      })
      
      ;; Update company risk score
      (let ((company (unwrap-panic (map-get? companies company-id))))
        (map-set companies company-id
          (merge company { risk-score: overall-risk })
        )
      )
      (ok overall-risk)
    )
  )
)

;; Cash Flow Management
(define-public (record-cash-flow (company-id uint) (period uint) (inflows uint) (outflows uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-some (map-get? companies company-id)) err-company-not-found)
    
    (let 
      (
        (net-position (if (>= inflows outflows) (- inflows outflows) u0))
        (financing-needs (if (> outflows inflows) (- outflows inflows) u0))
      )
      (map-set cash-flows { company-id: company-id, period: period } {
        inflows: inflows,
        outflows: outflows,
        net-position: net-position,
        financing-needs: financing-needs
      })
      (ok net-position)
    )
  )
)

;; Add Liquidity
(define-public (add-liquidity (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (> amount u0) err-invalid-amount)
    (var-set total-liquidity (+ (var-get total-liquidity) amount))
    (ok (var-get total-liquidity))
  )
)

;; Read-only Functions

(define-read-only (get-company (company-id uint))
  (map-get? companies company-id)
)

(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices invoice-id)
)

(define-read-only (get-payment-terms (company-id uint) (supplier-id uint))
  (map-get? payment-terms { company-id: company-id, supplier-id: supplier-id })
)

(define-read-only (get-risk-assessment (assessment-id uint))
  (map-get? risk-assessments assessment-id)
)

(define-read-only (get-cash-flow (company-id uint) (period uint))
  (map-get? cash-flows { company-id: company-id, period: period })
)

(define-read-only (get-total-liquidity)
  (var-get total-liquidity)
)

(define-read-only (calculate-financing-cost (invoice-id uint))
  (match (map-get? invoices invoice-id)
    invoice (let ((cost (/ (* (get advance-amount invoice) (get financing-rate invoice)) u100)))
              (ok cost))
    (err u404)
  )
)


;; title: supply-chain-finance
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

