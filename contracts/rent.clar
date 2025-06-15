
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

(define-constant ERR_INVALID_ESCALATION (err u112))
(define-constant ERR_ESCALATION_EXISTS (err u113))
(define-constant ERR_NO_ESCALATION (err u114))

(define-map rent-escalations
  { property-id: uint }
  {
    escalation-type: (string-ascii 10),
    escalation-value: uint,
    effective-date: uint,
    frequency-months: uint,
    last-applied: uint,
    active: bool
  }
)

(define-map rent-history
  { property-id: uint, effective-date: uint }
  {
    old-rent: uint,
    new-rent: uint,
    escalation-applied: uint,
    timestamp: uint
  }
)

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


(define-read-only (get-tenant-payment-history (tenant principal))
  (let (
    (tenant-info (unwrap! (get-tenant-info tenant) ERR_NOT_REGISTERED))
    (current-date (get-current-month-year))
    (current-year (get year current-date))
    (payments (list))
  )
    (ok (map get-payment-record 
      (list tenant tenant tenant tenant tenant tenant)
      (list u1 u2 u3 u4 u5 u6)
      (list current-year current-year current-year current-year current-year current-year)
    ))
  )
)



(define-constant ERR_NO_MAINTENANCE_REQUEST (err u110))
(define-constant ERR_REQUEST_ALREADY_EXISTS (err u111))

(define-map maintenance-requests
  { request-id: uint, property-id: uint }
  {
    tenant: principal,
    description: (string-ascii 256),
    status: (string-ascii 20),
    timestamp: uint,
    resolution-timestamp: (optional uint)
  }
)

(define-data-var request-counter uint u0)

(define-public (submit-maintenance-request (property-id uint) (description (string-ascii 256)))
  (let (
    (tenant tx-sender)
    (tenant-info (unwrap! (get-tenant-info tenant) ERR_NOT_REGISTERED))
    (request-id (+ (var-get request-counter) u1))
  )
    (asserts! (is-eq (get property-id tenant-info) property-id) ERR_UNAUTHORIZED)
    
    (map-set maintenance-requests
      { request-id: request-id, property-id: property-id }
      {
        tenant: tenant,
        description: description,
        status: "pending",
        timestamp: stacks-block-height,
        resolution-timestamp: none
      }
    )
    
    (var-set request-counter request-id)
    (ok request-id)
  )
)

(define-public (resolve-maintenance-request (request-id uint) (property-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (request (unwrap! (map-get? maintenance-requests { request-id: request-id, property-id: property-id }) ERR_NO_MAINTENANCE_REQUEST))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    
    (map-set maintenance-requests
      { request-id: request-id, property-id: property-id }
      (merge request {
        status: "resolved",
        resolution-timestamp: (some stacks-block-height)
      })
    )
    (ok true)
  )
)




(define-read-only (get-rent-escalation (property-id uint))
  (map-get? rent-escalations { property-id: property-id })
)

(define-read-only (get-rent-history (property-id uint) (effective-date uint))
  (map-get? rent-history { property-id: property-id, effective-date: effective-date })
)

(define-read-only (calculate-new-rent (property-id uint))
  (let (
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (escalation (unwrap! (get-rent-escalation property-id) ERR_NO_ESCALATION))
    (current-rent (get rent-amount property))
    (escalation-type (get escalation-type escalation))
    (escalation-value (get escalation-value escalation))
  )
    (ok (if (is-eq escalation-type "percentage")
      (+ current-rent (/ (* current-rent escalation-value) u100))
      (+ current-rent escalation-value)
    ))
  )
)

(define-read-only (is-escalation-due (property-id uint))
  (let (
    (escalation (unwrap! (get-rent-escalation property-id) false))
    (effective-date (get effective-date escalation))
    (frequency-months (get frequency-months escalation))
    (last-applied (get last-applied escalation))
    (next-due-date (+ last-applied (* frequency-months u30 SECONDS_IN_DAY)))
  )
    (and 
      (get active escalation)
      (>= stacks-block-height effective-date)
      (>= stacks-block-height next-due-date)
    )
  )
)

(define-public (set-rent-escalation (property-id uint) (escalation-type (string-ascii 10)) (escalation-value uint) (effective-date uint) (frequency-months uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq escalation-type "percentage") (is-eq escalation-type "fixed")) ERR_INVALID_ESCALATION)
    (asserts! (> escalation-value u0) ERR_INVALID_ESCALATION)
    (asserts! (> frequency-months u0) ERR_INVALID_ESCALATION)
    (asserts! (>= effective-date stacks-block-height) ERR_INVALID_ESCALATION)
    
    (map-set rent-escalations
      { property-id: property-id }
      {
        escalation-type: escalation-type,
        escalation-value: escalation-value,
        effective-date: effective-date,
        frequency-months: frequency-months,
        last-applied: stacks-block-height,
        active: true
      }
    )
    (ok true)
  )
)

(define-public (apply-rent-escalation (property-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (escalation (unwrap! (get-rent-escalation property-id) ERR_NO_ESCALATION))
    (old-rent (get rent-amount property))
    (new-rent (unwrap-panic (calculate-new-rent property-id)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-escalation-due property-id) ERR_INVALID_ESCALATION)
    
    (map-set properties
      { property-id: property-id }
      (merge property { rent-amount: new-rent })
    )
    
    (map-set rent-escalations
      { property-id: property-id }
      (merge escalation { last-applied: stacks-block-height })
    )
    
    (map-set rent-history
      { property-id: property-id, effective-date: stacks-block-height }
      {
        old-rent: old-rent,
        new-rent: new-rent,
        escalation-applied: (get escalation-value escalation),
        timestamp: stacks-block-height
      }
    )
    
    (ok new-rent)
  )
)

(define-public (disable-rent-escalation (property-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (escalation (unwrap! (get-rent-escalation property-id) ERR_NO_ESCALATION))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    
    (map-set rent-escalations
      { property-id: property-id }
      (merge escalation { active: false })
    )
    (ok true)
  )
)

(define-public (get-future-rent (property-id uint) (months-ahead uint))
  (let (
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (escalation (unwrap! (get-rent-escalation property-id) ERR_NO_ESCALATION))
    (current-rent (get rent-amount property))
    (frequency (get frequency-months escalation))
    (escalations-count (/ months-ahead frequency))
    (escalation-value (get escalation-value escalation))
    (escalation-type (get escalation-type escalation))
  )
    (if (is-eq escalation-type "percentage")
      (ok (fold apply-percentage-increase (list escalations-count) current-rent))
      (ok (+ current-rent (* escalations-count escalation-value)))
    )
  )
)

(define-private (apply-percentage-increase (count uint) (current-amount uint))
  (+ current-amount (/ (* current-amount u5) u100))
)