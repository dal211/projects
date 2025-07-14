# ─────────────────────────────────────────────────────────────────────────────
# 0. Load tidyverse (for tibble & nice printing)
# ─────────────────────────────────────────────────────────────────────────────
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Define inputs
# ─────────────────────────────────────────────────────────────────────────────
purchase_price <- 700000 # cost of the home
mortgage_rate <- 0.05 # annual interest rate on the loan
loan_term_years <- 15 # length of mortgage in years
invest_return <- 0.07 # your opportunity cost (annual)

# three down‐payment rates to compare
down_payment_pct_small <- 0.14 # 14% DP → large loan
down_payment_pct_med <- 0.30 # 30% DP → medium loan
down_payment_pct_large <- 0.43 # 43% DP → small loan

# ─────────────────────────────────────────────────────────────────────────────
# 2. Helper functions
# ─────────────────────────────────────────────────────────────────────────────
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
# 3. Function to run ONE scenario (given a down‐payment pct)
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
    dp_pct      = dp_pct,
    dp_amount   = dp,
    loan_amount = loan_amt,
    annual_pmt  = pmt,
    pv_invest   = pv_invest,
    pv_mortgage = pv_mortgage,
    net_npv     = pv_invest + pv_mortgage
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 4. Run and compare scenarios
# ─────────────────────────────────────────────────────────────────────────────
results <- bind_rows(
  # Compare: large DP vs. small DP
  run_scenario(down_payment_pct_large, down_payment_pct_small),
  #         medium DP vs. small DP
  run_scenario(down_payment_pct_med, down_payment_pct_small)
)

# 5. Show results
results %>%
  mutate(
    dp_label = case_when(
      dp_pct == down_payment_pct_large ~ "Large DP",
      dp_pct == down_payment_pct_med ~ "Medium DP",
      TRUE ~ "Other"
    )
  ) %>%
  select(dp_label, dp_amount, loan_amount, annual_pmt, pv_invest, pv_mortgage, net_npv) %>%
  knitr::kable(digits = 0, caption = "NPV Comparison of Down‐Payment Scenarios")

# ─────────────────────────────────────────────────────────────────────────────
