alm_projection <- function() {
  dates <- as.Date("2025-01-01") + c(31, 59, 90)
  yields <- data.frame(
    scenario_id = rep(c("base", "hike"), each = 3),
    version = "test-v1",
    step = rep(1:3, 2),
    date = rep(dates, 2),
    DGS3MO = c(rep(4, 3), 4.5, 5, 5.5),
    DGS2 = c(rep(4, 3), 5, 6, 7),
    DGS5 = rep(4, 6),
    check.names = FALSE
  )
  structure(
    list(yields = yields, tenors = c(DGS3MO = 0.25, DGS2 = 2, DGS5 = 5),
         synthetic = TRUE),
    class = "ek_yield_curve_projection"
  )
}

alm_portfolio <- function(...) {
  defaults <- data.frame(
    position = c("floating_loan", "core_deposits"),
    side = c("asset", "liability"),
    balance = c(120, 100),
    rate = c(0.06, 0.02),
    tenor = c("DGS2", "DGS3MO"),
    repricing = c("floating", "deposit"),
    duration = c(1.5, 1),
    reset_months = c(2, 1),
    deposit_beta = c(0, 0.5),
    stringsAsFactors = FALSE
  )
  overrides <- list(...)
  for (name in names(overrides)) defaults[[name]] <- overrides[[name]]
  defaults
}

test_that("ALM scenario results apply reset lags, deposit beta and duration", {
  result <- ek_alm_scenario_analysis(
    alm_projection(), alm_portfolio(), horizon_months = 3
  )
  base <- result$summary[result$summary$scenario_id == "base", ]
  hike <- result$summary[result$summary$scenario_id == "hike", ]
  expect_equal(base$nii_change, 0)
  expect_equal(base$terminal_eve_change, 0)
  expect_equal(hike$base_nii, (120 * 0.06 - 100 * 0.02) / 12 * 3)
  expect_equal(hike$nii_change, 0.375)
  expect_equal(hike$terminal_eve_change, -3.9)

  path <- result$paths[result$paths$scenario_id == "hike", ]
  expect_equal(path$nii_change, c(0.0791666666666667, 0.0583333333333333, 0.2375))
  expect_equal(path$eve_change, c(-1.3, -2.6, -3.9))
  deposit_rows <- result$positions$scenario_id == "hike" &
    result$positions$position == "core_deposits"
  deposit <- result$positions[deposit_rows, ]
  expect_equal(deposit$rate_change, c(0.0025, 0.005, 0.0075))
})

test_that("ALM inputs require valid curve paths and balance-sheet assumptions", {
  curves <- alm_projection()
  expect_error(ek_alm_scenario_analysis(curves, horizon_months = 4), "cover")
  expect_error(
    ek_alm_scenario_analysis(
      curves, alm_portfolio(), reference_scenario = "missing", horizon_months = 3
    ),
    "Reference scenario"
  )
  expect_error(
    ek_alm_scenario_analysis(
      curves, alm_portfolio(side = c("asset", "other")), horizon_months = 3
    ),
    "invalid sides"
  )
  expect_error(
    ek_alm_scenario_analysis(
      curves, alm_portfolio(tenor = c("DGS10", "DGS3MO")), horizon_months = 3
    ),
    "invalid sides"
  )
  expect_error(
    ek_alm_scenario_analysis(
      curves, alm_portfolio(deposit_beta = c(0, 1.1)), horizon_months = 3
    ),
    "invalid sides"
  )
})

test_that("ALM API exposes sample and BVAR scenarios with lineage", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  router <- ek_test_router()
  result <- ek_test_call(router, "/v1/banking/alm", query = "horizon_months=6")

  expect_equal(result$status, 200L)
  expect_equal(result$body$meta$model_version, "alm-static-repricing-v1")
  expect_true(result$body$meta$scenario_synthetic)
  expect_equal(result$body$meta$horizon_months, 6L)
  expect_equal(nrow(result$body$summary), 3L)
  expect_equal(nrow(result$body$paths), 18L)
  expect_equal(result$body$meta$run_id, ek_read_runs(log)$run_id)
  expect_equal(ek_read_runs(log)$model_id, "banking_alm_scenario_analysis")

  bvar <- paste0(
    '{"vars":["FEDFUNDS","UNRATE","NFCI"],"p":1,"h":12,"draws":5,',
    '"scenarios":{"base":{},"downturn":{"UNRATE":[10.5,11,11.5,12]}}}'
  )
  conditional <- ek_test_call(router, "/v1/banking/alm", body = bvar)
  expect_equal(conditional$status, 200L)
  expect_equal(conditional$body$meta$scenario_version, "bvar-v1")
  expect_equal(nrow(conditional$body$summary), 2L)

  invalid <- ek_test_call(router, "/v1/banking/alm", body = '{"h":6}')
  expect_equal(invalid$status, 400L)
  expect_match(invalid$body$error, "require `scenarios`")
  expect_equal(ek_test_call(router, "/v1/banking/alm", query = "horizon_months=61")$status,
               400L)
})
