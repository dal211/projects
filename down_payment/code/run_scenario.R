# Loan
purchase_price <- 700000 # cost of the home
mortgage_rate <- 0.05 # annual interest rate on the loan
loan_term_years <- 15 # length of mortgage in years
invest_return <- 0.07 # your opportunity cost (annual)

# Down payments
dp_pct <- .43
base_dp_pct <- .29

# Household factors
cash <- 200000
annual_hh_income_after_tax <- 130000

# 2a. Calculate the fixed annual payment for an n‐year loan at rate r
calc_annual_payment <- function(principal, rate, term_years) {
  # Uses: A = [r P (1+r)^n] / [(1+r)^n - 1]
  (rate * principal * (1 + rate)^term_years) /
    ((1 + rate)^term_years - 1)
}

# 2b. Present‐value of an ordinary annuity of A for n years at rate r
npv_annuity <- function(annual_payment, rate, term_years) {
  # Uses: PV = A * [1 − (1+r)^(-n)] / r
  annual_payment * (1 - (1 + rate)^(-term_years)) / rate
}

# ─────────────────────────────────────────────────────────────────────────────
# Function to run ONE scenario (given a down‐payment pct)
# ─────────────────────────────────────────────────────────────────────────────
run_scenario <- function(dp_pct, base_dp_pct) {
  # dp_pct      = scenario’s down‐payment_pct
  # base_dp_pct = the “other” DP you compare to (e.g. small or large)
  
  # 3a. Cash‐zero: times 0…n
  times <- 0:loan_term_years
  
  # 3b. Compute DP and loan principal
  dp <- dp_pct * purchase_price
  base_dp <- base_dp_pct * purchase_price
  loan_amt <- purchase_price - dp
  base_loan <- purchase_price - base_dp
  
  # 3c. Annual mortgage payments
  pmt <- calc_annual_payment(loan_amt, mortgage_rate, loan_term_years)
  base_pmt <- calc_annual_payment(base_loan, mortgage_rate, loan_term_years)
  
  # 3d. Cash flows
  #    t=0  : you keep (base_dp – dp) in your pocket to invest
  invest0 <- base_dp - dp
  #    t=1:n: each year you “free up” -(pmt) vs. -(base_pmt), so
  #            you net + (base_pmt – pmt) to invest
  ann_save <- base_pmt - pmt
  invest_flows <- c(invest0, rep(ann_save, loan_term_years))
  mortgage_flows <- c(-dp, rep(-pmt, loan_term_years))
  
  # 3e. Discount both streams at invest_return → PVs
  pv_invest <- sum(invest_flows / (1 + invest_return)^times)
  pv_mortgage <- sum(mortgage_flows / (1 + invest_return)^times)
  
  # 3f. Net NPV = PV(invest) + PV(mortgage)
  tibble(
    `Home price` = purchase_price,
    `Down payment %` = dp_pct,
    `Down payment amt`   = dp,
    `Loan Term` = loan_term_years,
    `Mortgage rate` = mortgage_rate,
    `Total mortgage` = loan_amt,
    `Annual mortgage pmts`  = pmt,
    `Monthly pmt` = pmt / 12
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. Run and compare scenarios
# ─────────────────────────────────────────────────────────────────────────────
results <- bind_rows(
  # Compare: large DP vs. small DP
  run_scenario(dp_pct, base_dp_pct),
  #         medium DP vs. small DP
  run_scenario(base_dp_pct, dp_pct)
)
