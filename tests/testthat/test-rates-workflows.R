rate_workflow_projection <- function() {
  dates <- as.Date("2025-01-01") + c(31, 59, 90)
  yields <- data.frame(
    scenario_id = rep(c("base", "hike"), each = 3),
    version = "test-v1",
    step = rep(1:3, 2),
    date = rep(dates, 2),
    DGS3MO = c(rep(4, 3), 4.5, 5, 5.5),
    DGS2 = c(rep(4, 3), 5, 6, 7),
    DGS5 = rep(4, 6),
    DGS10 = rep(4, 6),
    check.names = FALSE
  )
  structure(
    list(yields = yields,
         tenors = c(DGS3MO = 0.25, DGS2 = 2, DGS5 = 5, DGS10 = 10),
         synthetic = TRUE),
    class = "ek_yield_curve_projection"
  )
}

test_that("matched-tenor FTP calculates asset and liability contributions", {
  curves <- rate_workflow_projection()
  portfolio <- data.frame(
    position = c("loan", "deposit"),
    side = c("asset", "liability"),
    balance = c(1000, 500),
    rate = c(0.07, 0.02),
    tenor = c("DGS2", "DGS3MO"),
    funding_spread_bps = c(100, 50),
    stringsAsFactors = FALSE
  )
  result <- ek_ftp_analysis(curves, portfolio)

  expect_equal(result$model_version, "matched-tenor-ftp-v1")
  expect_equal(result$portfolio_source, "provided")
  base <- result$positions[result$positions$scenario_id == "base", ]
  expect_equal(base$transfer_rate[base$position == "loan"], rep(0.05, 3))
  expect_equal(base$margin_bps[base$position == "loan"], rep(200, 3))
  expect_equal(base$margin_bps[base$position == "deposit"], rep(250, 3))
  expect_equal(
    result$paths$monthly_contribution[result$paths$scenario_id == "base"],
    rep((1000 * 0.02 + 500 * 0.025) / 12, 3)
  )
  expect_equal(nrow(result$summary), 2L)
})

test_that("DCF and DCM inputs combine scenario curves and supplied spreads", {
  curves <- rate_workflow_projection()
  dcf <- data.frame(
    project = "project",
    tenor = "DGS5",
    beta = 1,
    equity_risk_premium_bps = 500,
    debt_spread_bps = 200,
    equity_weight = 0.8,
    debt_weight = 0.2,
    tax_rate = 0.25
  )
  dcm <- data.frame(issue = "issue", tenor = "DGS2", spread_bps = 125)
  result <- ek_curve_pricing_inputs(curves, dcf, dcm)

  expect_equal(result$model_version, "curve-pricing-inputs-v1")
  expect_equal(result$dcf_source, "provided")
  expect_equal(result$dcm_source, "provided")
  expect_equal(result$dcf$risk_free_rate, rep(0.04, 6))
  expect_equal(result$dcf$cost_of_equity, rep(0.09, 6))
  expect_equal(result$dcf$cost_of_debt, rep(0.06, 6))
  expect_equal(result$dcf$wacc, rep(0.081, 6))
  expect_equal(result$dcm$indicative_yield, c(0.0525, 0.0525, 0.0525,
                                              0.0625, 0.0725, 0.0825))
  expect_true(all(ek_curve_pricing_inputs(curves)$dcf_source == "synthetic_sample"))
})

test_that("FTP and pricing assumptions reject invalid inputs and curves", {
  curves <- rate_workflow_projection()
  ftp <- data.frame(
    position = "loan", side = "asset", balance = 100, rate = 0.05,
    tenor = "DGS2", funding_spread_bps = -1
  )
  expect_error(ek_ftp_analysis(curves, ftp), "invalid sides")
  expect_error(ek_ftp_analysis(curves, transform(ftp, tenor = "missing")),
               "invalid sides")

  dcf <- data.frame(
    project = "project", tenor = "DGS5", beta = 1,
    equity_risk_premium_bps = 500, debt_spread_bps = 200,
    equity_weight = 0.8, debt_weight = 0.3, tax_rate = 0.25
  )
  expect_error(ek_curve_pricing_inputs(curves, dcf), "weights")
  expect_error(
    ek_curve_pricing_inputs(curves, dcm = data.frame(
      issue = "issue", tenor = "DGS2", spread_bps = 6000
    )),
    "between 0 and 5000"
  )
  expect_error(ek_curve_pricing_inputs(list()), "valid ek_yield_curve_projection")
})
