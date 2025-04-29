
;; title: rent
;; version: 1.0


(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_REGISTERED (err u101))
(define-constant ERR_NOT_REGISTERED (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_ALREADY_PAID (err u104))
(define-constant ERR_LATE_PAYMENT (err u105))
(define-constant ERR_EVICTION_IN_PROGRESS (err u106))
(define-constant ERR_NO_EVICTION (err u107))
(define-constant ERR_GRACE_PERIOD_ACTIVE (err u108))
(define-constant ERR_INSUFFICIENT_FUNDS (err u109))

(define-constant SECONDS_IN_DAY u86400)
(define-constant GRACE_PERIOD_DAYS u5)
(define-constant LATE_FEE_PERCENTAGE u10)

(define-data-var contract-owner principal tx-sender)

(define-map properties
  { property-id: uint }
  {
    owner: principal,
    rent-amount: uint,
    security-deposit: uint,
    payment-day: uint,
    active: bool
  }
)

(define-map tenants
  { tenant-id: principal }
  {
    property-id: uint,
    rent-paid-until: uint,
    last-payment: uint,
    security-deposit-paid: uint,
    eviction-status: bool
  }
)

(define-map property-tenants
  { property-id: uint }
  { tenant: (optional principal) }
)

(define-map rent-payments
  { tenant: principal, month: uint, year: uint }
  {
    amount: uint,
    paid-on-time: bool,
    late-fee: uint,
    timestamp: uint
  }
)

(define-read-only (get-owner)
  (var-get contract-owner)
)

(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-tenant-info (tenant principal))
  (map-get? tenants { tenant-id: tenant })
)

(define-read-only (get-property-tenant (property-id uint))
  (map-get? property-tenants { property-id: property-id })
)

(define-read-only (get-payment-record (tenant principal) (month uint) (year uint))
  (map-get? rent-payments { tenant: tenant, month: month, year: year })
)

(define-read-only (calculate-late-fee (rent-amount uint))
  (/ (* rent-amount LATE_FEE_PERCENTAGE) u100)
)

(define-read-only (is-payment-late (tenant principal))
  (let (
    (tenant-info (unwrap! (get-tenant-info tenant) false))
    (current-time stacks-block-height)
    (grace-period-end (+ (get rent-paid-until tenant-info) (* GRACE_PERIOD_DAYS SECONDS_IN_DAY)))
  )
    (> current-time grace-period-end)
  )
)

(define-read-only (get-current-month-year)
  (let (
    (current-time stacks-block-height)
    (month (+ u1 (mod (/ current-time (* u30 SECONDS_IN_DAY)) u12)))
    (year (+ u2023 (/ current-time (* u365 SECONDS_IN_DAY))))
  )
    {month: month, year: year}
  )
)

(define-public (register-property (property-id uint) (rent-amount uint) (security-deposit uint) (payment-day uint))
  (let (
    (caller tx-sender)
  )
    (asserts! (or (is-eq caller (var-get contract-owner)) (is-eq caller tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-none (get-property property-id)) ERR_ALREADY_REGISTERED)
    (asserts! (> rent-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (and (>= payment-day u1) (<= payment-day u28)) ERR_INVALID_AMOUNT)
    
    (map-set properties
      { property-id: property-id }
      {
        owner: caller,
        rent-amount: rent-amount,
        security-deposit: security-deposit,
        payment-day: payment-day,
        active: true
      }
    )
    
    (map-set property-tenants
      { property-id: property-id }
      { tenant: none }
    )
    
    (ok true)
  )
)

(define-public (register-tenant (tenant principal) (property-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (current-tenant-record (get-property-tenant property-id))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-none (get tenant current-tenant-record)) ERR_ALREADY_REGISTERED)
    
    (map-set tenants
      { tenant-id: tenant }
      {
        property-id: property-id,
        rent-paid-until: stacks-block-height,
        last-payment: u0,
        security-deposit-paid: u0,
        eviction-status: false
      }
    )
    
    (map-set property-tenants
      { property-id: property-id }
      { tenant: (some tenant) }
    )
    
    (ok true)
  )
)

(define-public (pay-security-deposit (property-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (tenant-info (unwrap! (get-tenant-info caller) ERR_NOT_REGISTERED))
    (deposit-amount (get security-deposit property))
  )
    (asserts! (is-eq (get property-id tenant-info) property-id) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get security-deposit-paid tenant-info) u0) ERR_ALREADY_PAID)
    
    (try! (stx-transfer? deposit-amount caller (get owner property)))
    
    (map-set tenants
      { tenant-id: caller }
      (merge tenant-info { security-deposit-paid: deposit-amount })
    )
    
    (ok true)
  )
)

(define-public (pay-rent)
  (let (
    (caller tx-sender)
    (tenant-info (unwrap! (get-tenant-info caller) ERR_NOT_REGISTERED))
    (property-id (get property-id tenant-info))
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (rent-amount (get rent-amount property))
    (is-late (is-payment-late caller))
    (late-fee (if is-late (calculate-late-fee rent-amount) u0))
    (total-payment (+ rent-amount late-fee))
    (date-info (get-current-month-year))
    (month (get month date-info))
    (year (get year date-info))
  )
    (asserts! (not (get eviction-status tenant-info)) ERR_EVICTION_IN_PROGRESS)
    (asserts! (is-none (get-payment-record caller month year)) ERR_ALREADY_PAID)
    
    (try! (stx-transfer? total-payment caller (get owner property)))
    
    (map-set rent-payments
      { tenant: caller, month: month, year: year }
      {
        amount: total-payment,
        paid-on-time: (not is-late),
        late-fee: late-fee,
        timestamp: stacks-block-height
      }
    )
    
    (map-set tenants
      { tenant-id: caller }
      (merge tenant-info {
        rent-paid-until: (+ stacks-block-height (* u30 SECONDS_IN_DAY)),
        last-payment: stacks-block-height
      })
    )
    
    (ok total-payment)
  )
)

(define-public (initiate-eviction (tenant principal))
  (let (
    (caller tx-sender)
    (tenant-info (unwrap! (get-tenant-info tenant) ERR_NOT_REGISTERED))
    (property-id (get property-id tenant-info))
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (grace-period-end (+ (get rent-paid-until tenant-info) (* GRACE_PERIOD_DAYS SECONDS_IN_DAY)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (> stacks-block-height grace-period-end) ERR_GRACE_PERIOD_ACTIVE)
    (asserts! (not (get eviction-status tenant-info)) ERR_ALREADY_REGISTERED)
    
    (map-set tenants
      { tenant-id: tenant }
      (merge tenant-info { eviction-status: true })
    )
    
    (ok true)
  )
)

(define-public (cancel-eviction (tenant principal))
  (let (
    (caller tx-sender)
    (tenant-info (unwrap! (get-tenant-info tenant) ERR_NOT_REGISTERED))
    (property-id (get property-id tenant-info))
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (get eviction-status tenant-info) ERR_NO_EVICTION)
    
    (map-set tenants
      { tenant-id: tenant }
      (merge tenant-info { eviction-status: false })
    )
    
    (ok true)
  )
)

(define-public (complete-eviction (tenant principal))
  (let (
    (caller tx-sender)
    (tenant-info (unwrap! (get-tenant-info tenant) ERR_NOT_REGISTERED))
    (property-id (get property-id tenant-info))
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (get eviction-status tenant-info) ERR_NO_EVICTION)
    
    (map-set property-tenants
      { property-id: property-id }
      { tenant: none }
    )
    
    (ok true)
  )
)

(define-public (return-security-deposit (tenant principal))
  (let (
    (caller tx-sender)
    (tenant-info (unwrap! (get-tenant-info tenant) ERR_NOT_REGISTERED))
    (property-id (get property-id tenant-info))
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (deposit-amount (get security-deposit-paid tenant-info))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (> deposit-amount u0) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? deposit-amount caller tenant))
    
    (ok true)
  )
)

(define-public (transfer-contract-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)