options(scipen = 9999)
source("code/libraries.R")

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
run_scenario <- function(dp_pct) {
  # dp_pct      = scenario’s down‐payment_pct
  # base_dp_pct = the “other” DP you compare to (e.g. small or large)
  
  # 3a. Cash‐zero: times 0…n
  times <- 0:loan_term_years
  
  # 3b. Compute DP and loan principal
  dp <- dp_pct * purchase_price
  loan_amt <- purchase_price - dp
  
  # 3c. Annual mortgage payments
  pmt <- calc_annual_payment(loan_amt, mortgage_rate, loan_term_years)
  
  # 3f. Net NPV = PV(invest) + PV(mortgage)
  tibble(
    `Home price` = purchase_price,
    `Down payment %` = dp_pct,
    `Down payment amt`   = dp,
    `Loan Term` = loan_term_years,
    `Mortgage rate` = mortgage_rate,
    `Total mortgage` = loan_amt,
    `Annual mortgage pmts`  = round(pmt),
    `Monthly pmts` = round(pmt / 12)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. Run and compare scenarios
# ─────────────────────────────────────────────────────────────────────────────
results <- bind_rows(
  run_scenario(dp_pct),
  run_scenario(base_dp_pct)
)

kable(results)
