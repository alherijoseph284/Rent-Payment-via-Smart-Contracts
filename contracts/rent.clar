
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

(define-constant ERR_LEASE_NOT_FOUND (err u115))
(define-constant ERR_LEASE_EXPIRED (err u116))
(define-constant ERR_LEASE_ALREADY_EXISTS (err u117))
(define-constant ERR_INVALID_LEASE_TERM (err u118))
(define-constant ERR_SCREENING_INCOMPLETE (err u119))
(define-constant ERR_TENANT_REJECTED (err u120))
(define-constant ERR_AUTO_RENEWAL_DISABLED (err u121))
(define-constant ERR_EARLY_TERMINATION_NOT_ALLOWED (err u122))
(define-constant ERR_POLICY_NOT_FOUND (err u123))
(define-constant ERR_POLICY_EXPIRED (err u124))
(define-constant ERR_CLAIM_NOT_FOUND (err u125))
(define-constant ERR_CLAIM_ALREADY_EXISTS (err u126))
(define-constant ERR_INVALID_POLICY_TERM (err u127))
(define-constant ERR_INSUFFICIENT_COVERAGE (err u128))

(define-map lease-agreements
  { property-id: uint, lease-id: uint }
  {
    tenant: principal,
    start-date: uint,
    end-date: uint,
    lease-term-months: uint,
    security-deposit: uint,
    monthly-rent: uint,
    auto-renewal: bool,
    renewal-notice-period: uint,
    early-termination-allowed: bool,
    early-termination-fee: uint,
    lease-status: (string-ascii 20),
    created-timestamp: uint,
    last-renewal-date: (optional uint)
  }
)

(define-map tenant-screening
  { tenant: principal, property-id: uint, screening-id: uint }
  {
    credit-score: uint,
    income-verified: bool,
    employment-verified: bool,
    references-checked: bool,
    background-check-passed: bool,
    screening-status: (string-ascii 20),
    screening-date: uint,
    rejection-reason: (optional (string-ascii 256)),
    landlord-notes: (optional (string-ascii 256))
  }
)

(define-map lease-renewals
  { property-id: uint, lease-id: uint, renewal-id: uint }
  {
    old-lease-id: uint,
    new-lease-id: uint,
    renewal-date: uint,
    new-end-date: uint,
    rent-change: uint,
    auto-renewed: bool,
    tenant-agreed: bool,
    renewal-terms: (string-ascii 256)
  }
)

(define-map lease-terminations
  { property-id: uint, lease-id: uint }
  {
    termination-date: uint,
    termination-reason: (string-ascii 256),
    early-termination: bool,
    fees-charged: uint,
    notice-period: uint,
    initiated-by: principal,
    security-deposit-returned: bool
  }
)

(define-data-var lease-counter uint u0)
(define-data-var screening-counter uint u0)
(define-data-var renewal-counter uint u0)

;; Insurance Management System
(define-map insurance-policies
  { property-id: uint, policy-id: uint }
  {
    insurance-company: (string-ascii 100),
    policy-number: (string-ascii 50),
    policy-type: (string-ascii 30),
    coverage-amount: uint,
    deductible: uint,
    annual-premium: uint,
    start-date: uint,
    end-date: uint,
    auto-renewal: bool,
    policy-status: (string-ascii 20),
    last-premium-payment: uint,
    created-timestamp: uint
  }
)

(define-map insurance-claims
  { property-id: uint, policy-id: uint, claim-id: uint }
  {
    claim-type: (string-ascii 50),
    claim-amount: uint,
    damage-description: (string-ascii 500),
    claim-date: uint,
    claim-status: (string-ascii 20),
    approved-amount: uint,
    settlement-date: (optional uint),
    adjuster-notes: (optional (string-ascii 300)),
    submitted-by: principal
  }
)

(define-map premium-payments
  { property-id: uint, policy-id: uint, payment-year: uint }
  {
    amount-paid: uint,
    payment-date: uint,
    paid-on-time: bool,
    late-fee: uint,
    payment-method: (string-ascii 20)
  }
)

(define-map coverage-details
  { property-id: uint, policy-id: uint }
  {
    dwelling-coverage: uint,
    personal-property: uint,
    liability-coverage: uint,
    loss-of-use: uint,
    additional-coverages: (string-ascii 200),
    exclusions: (string-ascii 300)
  }
)

(define-data-var policy-counter uint u0)
(define-data-var claim-counter uint u0)

(define-read-only (get-lease-agreement (property-id uint) (lease-id uint))
  (map-get? lease-agreements { property-id: property-id, lease-id: lease-id })
)

(define-read-only (get-tenant-screening (tenant principal) (property-id uint) (screening-id uint))
  (map-get? tenant-screening { tenant: tenant, property-id: property-id, screening-id: screening-id })
)

(define-read-only (get-lease-renewal (property-id uint) (lease-id uint) (renewal-id uint))
  (map-get? lease-renewals { property-id: property-id, lease-id: lease-id, renewal-id: renewal-id })
)

(define-read-only (get-lease-termination (property-id uint) (lease-id uint))
  (map-get? lease-terminations { property-id: property-id, lease-id: lease-id })
)

;; Insurance read-only functions
(define-read-only (get-insurance-policy (property-id uint) (policy-id uint))
  (map-get? insurance-policies { property-id: property-id, policy-id: policy-id })
)

(define-read-only (get-insurance-claim (property-id uint) (policy-id uint) (claim-id uint))
  (map-get? insurance-claims { property-id: property-id, policy-id: policy-id, claim-id: claim-id })
)

(define-read-only (get-premium-payment (property-id uint) (policy-id uint) (payment-year uint))
  (map-get? premium-payments { property-id: property-id, policy-id: policy-id, payment-year: payment-year })
)

(define-read-only (get-coverage-details (property-id uint) (policy-id uint))
  (map-get? coverage-details { property-id: property-id, policy-id: policy-id })
)

(define-read-only (is-policy-active (property-id uint) (policy-id uint))
  (let (
    (policy (unwrap! (get-insurance-policy property-id policy-id) false))
    (current-time stacks-block-height)
  )
    (and 
      (>= current-time (get start-date policy))
      (<= current-time (get end-date policy))
      (is-eq (get policy-status policy) "active")
    )
  )
)

(define-read-only (is-premium-due (property-id uint) (policy-id uint))
  (let (
    (policy (unwrap! (get-insurance-policy property-id policy-id) false))
    (current-time stacks-block-height)
    (current-year (+ u2023 (/ current-time (* u365 SECONDS_IN_DAY))))
    (last-payment (get-premium-payment property-id policy-id current-year))
  )
    (and 
      (is-policy-active property-id policy-id)
      (is-none last-payment)
    )
  )
)

(define-read-only (is-lease-active (property-id uint) (lease-id uint))
  (let (
    (lease (unwrap! (get-lease-agreement property-id lease-id) false))
    (current-time stacks-block-height)
  )
    (and 
      (>= current-time (get start-date lease))
      (<= current-time (get end-date lease))
      (is-eq (get lease-status lease) "active")
    )
  )
)

(define-read-only (is-lease-renewal-due (property-id uint) (lease-id uint))
  (let (
    (lease (unwrap! (get-lease-agreement property-id lease-id) false))
    (current-time stacks-block-height)
    (notice-period (get renewal-notice-period lease))
    (renewal-deadline (- (get end-date lease) (* notice-period SECONDS_IN_DAY)))
  )
    (and 
      (get auto-renewal lease)
      (>= current-time renewal-deadline)
      (is-eq (get lease-status lease) "active")
    )
  )
)

(define-public (create-lease-agreement (property-id uint) (tenant principal) (lease-term-months uint) (auto-renewal bool) (renewal-notice-period uint) (early-termination-allowed bool) (early-termination-fee uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (lease-id (+ (var-get lease-counter) u1))
    (current-time stacks-block-height)
    (lease-end-date (+ current-time (* lease-term-months u30 SECONDS_IN_DAY)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (> lease-term-months u0) ERR_INVALID_LEASE_TERM)
    (asserts! (<= lease-term-months u24) ERR_INVALID_LEASE_TERM)
    (asserts! (> renewal-notice-period u0) ERR_INVALID_LEASE_TERM)
    (asserts! (<= renewal-notice-period u90) ERR_INVALID_LEASE_TERM)
    
    (map-set lease-agreements
      { property-id: property-id, lease-id: lease-id }
      {
        tenant: tenant,
        start-date: current-time,
        end-date: lease-end-date,
        lease-term-months: lease-term-months,
        security-deposit: (get security-deposit property),
        monthly-rent: (get rent-amount property),
        auto-renewal: auto-renewal,
        renewal-notice-period: renewal-notice-period,
        early-termination-allowed: early-termination-allowed,
        early-termination-fee: early-termination-fee,
        lease-status: "active",
        created-timestamp: current-time,
        last-renewal-date: none
      }
    )
    
    (var-set lease-counter lease-id)
    (ok lease-id)
  )
)

(define-public (conduct-tenant-screening (tenant principal) (property-id uint) (credit-score uint) (income-verified bool) (employment-verified bool) (references-checked bool) (background-check-passed bool) (landlord-notes (optional (string-ascii 256))))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (screening-id (+ (var-get screening-counter) u1))
    (current-time stacks-block-height)
    (screening-passed (and income-verified employment-verified references-checked background-check-passed (>= credit-score u600)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (>= credit-score u300) ERR_INVALID_AMOUNT)
    (asserts! (<= credit-score u850) ERR_INVALID_AMOUNT)
    
    (map-set tenant-screening
      { tenant: tenant, property-id: property-id, screening-id: screening-id }
      {
        credit-score: credit-score,
        income-verified: income-verified,
        employment-verified: employment-verified,
        references-checked: references-checked,
        background-check-passed: background-check-passed,
        screening-status: (if screening-passed "approved" "rejected"),
        screening-date: current-time,
        rejection-reason: (if screening-passed none (some "Failed screening criteria")),
        landlord-notes: landlord-notes
      }
    )
    
    (var-set screening-counter screening-id)
    (ok screening-id)
  )
)

(define-public (renew-lease (property-id uint) (lease-id uint) (new-term-months uint) (rent-change uint) (tenant-agreed bool))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (lease (unwrap! (get-lease-agreement property-id lease-id) ERR_LEASE_NOT_FOUND))
    (renewal-id (+ (var-get renewal-counter) u1))
    (new-lease-id (+ (var-get lease-counter) u1))
    (current-time stacks-block-height)
    (new-end-date (+ (get end-date lease) (* new-term-months u30 SECONDS_IN_DAY)))
    (new-rent (+ (get monthly-rent lease) rent-change))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-lease-renewal-due property-id lease-id) ERR_INVALID_LEASE_TERM)
    (asserts! (> new-term-months u0) ERR_INVALID_LEASE_TERM)
    (asserts! (<= new-term-months u24) ERR_INVALID_LEASE_TERM)
    (asserts! tenant-agreed ERR_UNAUTHORIZED)
    
    (map-set lease-renewals
      { property-id: property-id, lease-id: lease-id, renewal-id: renewal-id }
      {
        old-lease-id: lease-id,
        new-lease-id: new-lease-id,
        renewal-date: current-time,
        new-end-date: new-end-date,
        rent-change: rent-change,
        auto-renewed: (get auto-renewal lease),
        tenant-agreed: tenant-agreed,
        renewal-terms: "Standard lease renewal"
      }
    )
    
    (map-set lease-agreements
      { property-id: property-id, lease-id: new-lease-id }
      (merge lease {
        end-date: new-end-date,
        lease-term-months: new-term-months,
        monthly-rent: new-rent,
        last-renewal-date: (some current-time)
      })
    )
    
    (map-set lease-agreements
      { property-id: property-id, lease-id: lease-id }
      (merge lease { lease-status: "renewed" })
    )
    
    (var-set renewal-counter renewal-id)
    (var-set lease-counter new-lease-id)
    (ok new-lease-id)
  )
)

(define-public (terminate-lease (property-id uint) (lease-id uint) (termination-reason (string-ascii 256)) (early-termination bool) (notice-period uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (lease (unwrap! (get-lease-agreement property-id lease-id) ERR_LEASE_NOT_FOUND))
    (tenant (get tenant lease))
    (current-time stacks-block-height)
    (fees-charged (if early-termination (get early-termination-fee lease) u0))
  )
    (asserts! (or (is-eq (get owner property) caller) (is-eq tenant caller)) ERR_UNAUTHORIZED)
    (asserts! (is-lease-active property-id lease-id) ERR_LEASE_EXPIRED)
    (asserts! (or (not early-termination) (get early-termination-allowed lease)) ERR_EARLY_TERMINATION_NOT_ALLOWED)
    
    (map-set lease-terminations
      { property-id: property-id, lease-id: lease-id }
      {
        termination-date: current-time,
        termination-reason: termination-reason,
        early-termination: early-termination,
        fees-charged: fees-charged,
        notice-period: notice-period,
        initiated-by: caller,
        security-deposit-returned: false
      }
    )
    
    (map-set lease-agreements
      { property-id: property-id, lease-id: lease-id }
      (merge lease { lease-status: "terminated" })
    )
    
    (if early-termination
      (try! (stx-transfer? fees-charged tenant (get owner property)))
      true
    )
    (ok true)
  )
)

(define-public (auto-renew-lease (property-id uint) (lease-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (lease (unwrap! (get-lease-agreement property-id lease-id) ERR_LEASE_NOT_FOUND))
    (new-lease-id (+ (var-get lease-counter) u1))
    (current-time stacks-block-height)
    (new-end-date (+ (get end-date lease) (* (get lease-term-months lease) u30 SECONDS_IN_DAY)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (get auto-renewal lease) ERR_AUTO_RENEWAL_DISABLED)
    (asserts! (is-lease-renewal-due property-id lease-id) ERR_INVALID_LEASE_TERM)
    
    (map-set lease-agreements
      { property-id: property-id, lease-id: new-lease-id }
      (merge lease {
        end-date: new-end-date,
        last-renewal-date: (some current-time)
      })
    )
    
    (map-set lease-agreements
      { property-id: property-id, lease-id: lease-id }
      (merge lease { lease-status: "auto-renewed" })
    )
    
    (var-set lease-counter new-lease-id)
    (ok new-lease-id)
  )
)

;; Insurance Management Functions

(define-public (register-insurance-policy (property-id uint) (insurance-company (string-ascii 100)) (policy-number (string-ascii 50)) (policy-type (string-ascii 30)) (coverage-amount uint) (deductible uint) (annual-premium uint) (policy-term-months uint) (auto-renewal bool))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy-id (+ (var-get policy-counter) u1))
    (current-time stacks-block-height)
    (policy-end-date (+ current-time (* policy-term-months u30 SECONDS_IN_DAY)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> annual-premium u0) ERR_INVALID_AMOUNT)
    (asserts! (> policy-term-months u0) ERR_INVALID_POLICY_TERM)
    (asserts! (<= policy-term-months u60) ERR_INVALID_POLICY_TERM)
    
    (map-set insurance-policies
      { property-id: property-id, policy-id: policy-id }
      {
        insurance-company: insurance-company,
        policy-number: policy-number,
        policy-type: policy-type,
        coverage-amount: coverage-amount,
        deductible: deductible,
        annual-premium: annual-premium,
        start-date: current-time,
        end-date: policy-end-date,
        auto-renewal: auto-renewal,
        policy-status: "active",
        last-premium-payment: current-time,
        created-timestamp: current-time
      }
    )
    
    (var-set policy-counter policy-id)
    (ok policy-id)
  )
)

(define-public (set-coverage-details (property-id uint) (policy-id uint) (dwelling-coverage uint) (personal-property uint) (liability-coverage uint) (loss-of-use uint) (additional-coverages (string-ascii 200)) (exclusions (string-ascii 300)))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy (unwrap! (get-insurance-policy property-id policy-id) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-policy-active property-id policy-id) ERR_POLICY_EXPIRED)
    
    (map-set coverage-details
      { property-id: property-id, policy-id: policy-id }
      {
        dwelling-coverage: dwelling-coverage,
        personal-property: personal-property,
        liability-coverage: liability-coverage,
        loss-of-use: loss-of-use,
        additional-coverages: additional-coverages,
        exclusions: exclusions
      }
    )
    (ok true)
  )
)

(define-public (pay-insurance-premium (property-id uint) (policy-id uint) (payment-method (string-ascii 20)))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy (unwrap! (get-insurance-policy property-id policy-id) ERR_POLICY_NOT_FOUND))
    (current-time stacks-block-height)
    (current-year (+ u2023 (/ current-time (* u365 SECONDS_IN_DAY))))
    (premium-amount (get annual-premium policy))
    (existing-payment (get-premium-payment property-id policy-id current-year))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-policy-active property-id policy-id) ERR_POLICY_EXPIRED)
    (asserts! (is-none existing-payment) ERR_ALREADY_PAID)
    
    (map-set premium-payments
      { property-id: property-id, policy-id: policy-id, payment-year: current-year }
      {
        amount-paid: premium-amount,
        payment-date: current-time,
        paid-on-time: true,
        late-fee: u0,
        payment-method: payment-method
      }
    )
    
    (map-set insurance-policies
      { property-id: property-id, policy-id: policy-id }
      (merge policy { last-premium-payment: current-time })
    )
    
    (ok premium-amount)
  )
)

(define-public (file-insurance-claim (property-id uint) (policy-id uint) (claim-type (string-ascii 50)) (claim-amount uint) (damage-description (string-ascii 500)))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy (unwrap! (get-insurance-policy property-id policy-id) ERR_POLICY_NOT_FOUND))
    (claim-id (+ (var-get claim-counter) u1))
    (current-time stacks-block-height)
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-policy-active property-id policy-id) ERR_POLICY_EXPIRED)
    (asserts! (> claim-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= claim-amount (get coverage-amount policy)) ERR_INSUFFICIENT_COVERAGE)
    
    (map-set insurance-claims
      { property-id: property-id, policy-id: policy-id, claim-id: claim-id }
      {
        claim-type: claim-type,
        claim-amount: claim-amount,
        damage-description: damage-description,
        claim-date: current-time,
        claim-status: "submitted",
        approved-amount: u0,
        settlement-date: none,
        adjuster-notes: none,
        submitted-by: caller
      }
    )
    
    (var-set claim-counter claim-id)
    (ok claim-id)
  )
)

(define-public (process-insurance-claim (property-id uint) (policy-id uint) (claim-id uint) (approved-amount uint) (claim-status (string-ascii 20)) (adjuster-notes (optional (string-ascii 300))))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy (unwrap! (get-insurance-policy property-id policy-id) ERR_POLICY_NOT_FOUND))
    (claim (unwrap! (get-insurance-claim property-id policy-id claim-id) ERR_CLAIM_NOT_FOUND))
    (current-time stacks-block-height)
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get claim-status claim) "submitted") ERR_INVALID_AMOUNT)
    (asserts! (<= approved-amount (get claim-amount claim)) ERR_INSUFFICIENT_COVERAGE)
    
    (map-set insurance-claims
      { property-id: property-id, policy-id: policy-id, claim-id: claim-id }
      (merge claim {
        claim-status: claim-status,
        approved-amount: approved-amount,
        settlement-date: (if (is-eq claim-status "approved") (some current-time) none),
        adjuster-notes: adjuster-notes
      })
    )
    (ok true)
  )
)

(define-public (renew-insurance-policy (property-id uint) (policy-id uint) (new-coverage-amount uint) (new-premium uint) (new-term-months uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy (unwrap! (get-insurance-policy property-id policy-id) ERR_POLICY_NOT_FOUND))
    (new-policy-id (+ (var-get policy-counter) u1))
    (current-time stacks-block-height)
    (new-end-date (+ current-time (* new-term-months u30 SECONDS_IN_DAY)))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (get auto-renewal policy) ERR_AUTO_RENEWAL_DISABLED)
    (asserts! (> new-coverage-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> new-premium u0) ERR_INVALID_AMOUNT)
    (asserts! (> new-term-months u0) ERR_INVALID_POLICY_TERM)
    
    (map-set insurance-policies
      { property-id: property-id, policy-id: new-policy-id }
      (merge policy {
        coverage-amount: new-coverage-amount,
        annual-premium: new-premium,
        start-date: current-time,
        end-date: new-end-date,
        last-premium-payment: current-time,
        created-timestamp: current-time
      })
    )
    
    (map-set insurance-policies
      { property-id: property-id, policy-id: policy-id }
      (merge policy { policy-status: "renewed" })
    )
    
    (var-set policy-counter new-policy-id)
    (ok new-policy-id)
  )
)

(define-public (cancel-insurance-policy (property-id uint) (policy-id uint))
  (let (
    (caller tx-sender)
    (property (unwrap! (get-property property-id) ERR_NOT_REGISTERED))
    (policy (unwrap! (get-insurance-policy property-id policy-id) ERR_POLICY_NOT_FOUND))
  )
    (asserts! (is-eq (get owner property) caller) ERR_UNAUTHORIZED)
    (asserts! (is-policy-active property-id policy-id) ERR_POLICY_EXPIRED)
    
    (map-set insurance-policies
      { property-id: property-id, policy-id: policy-id }
      (merge policy { policy-status: "cancelled" })
    )
    (ok true)
  )
)


