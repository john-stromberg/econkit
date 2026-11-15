test_that("FTP API runs sample scenarios and records model lineage", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  result <- ek_test_call(ek_test_router(), "/v1/rates/ftp")

  expect_equal(result$status, 200L)
  expect_equal(result$body$meta$model_version, "matched-tenor-ftp-v1")
  expect_true(result$body$meta$scenario_synthetic)
  expect_equal(result$body$meta$portfolio_source, "synthetic_sample")
  expect_equal(nrow(result$body$summary), 3L)
  expect_equal(nrow(result$body$positions), 36L * 5L)
  runs <- ek_read_runs(log)
  expect_equal(runs$model_id, "matched_tenor_ftp")
  expect_equal(runs$run_id, result$body$meta$run_id)

  body <- paste0(
    '{"portfolio":[{"position":"loan","side":"asset","balance":1000,',
    '"rate":0.07,"tenor":"DGS5","funding_spread_bps":100}]}'
  )
  custom <- ek_test_call(ek_test_router(), "/v1/rates/ftp", body = body)
  expect_equal(custom$status, 200L)
  expect_equal(custom$body$meta$portfolio_source, "provided")
  expect_equal(nrow(custom$body$positions), 36L)
})

test_that("pricing-input API returns DCF and DCM inputs, not valuations", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  router <- ek_test_router()
  sample <- ek_test_call(router, "/v1/rates/pricing-inputs")

  expect_equal(sample$status, 200L)
  expect_equal(sample$body$meta$model_version, "curve-pricing-inputs-v1")
  expect_equal(sample$body$meta$dcf_source, "synthetic_sample")
  expect_equal(nrow(sample$body$dcf), 72L)
  expect_equal(nrow(sample$body$dcm), 72L)
  expect_true(all(sample$body$dcf$wacc > 0))
  expect_true(all(sample$body$dcm$indicative_yield > sample$body$dcm$benchmark_rate))
  expect_equal(ek_read_runs(log)$model_id, "curve_pricing_inputs")
  expect_equal(ek_read_runs(log)$run_id, sample$body$meta$run_id)

  body <- paste0(
    '{"dcf":[{"project":"test","tenor":"DGS5","beta":1,',
    '"equity_risk_premium_bps":500,"debt_spread_bps":200,',
    '"equity_weight":0.8,"debt_weight":0.2,"tax_rate":0.25}],',
    '"dcm":[{"issue":"test_issue","tenor":"DGS2","spread_bps":125}]}'
  )
  custom <- ek_test_call(router, "/v1/rates/pricing-inputs", body = body)
  expect_equal(custom$status, 200L)
  expect_equal(custom$body$meta$dcf_source, "provided")
  expect_equal(custom$body$meta$dcm_source, "provided")
  expect_equal(
    custom$body$dcf$wacc,
    custom$body$dcf$equity_weight * custom$body$dcf$cost_of_equity +
      custom$body$dcf$debt_weight * custom$body$dcf$cost_of_debt *
        (1 - custom$body$dcf$tax_rate),
    tolerance = 1e-3
  )
  expect_equal(ek_test_call(router, "/v1/rates/pricing-inputs", body = '{"h":6}')$status,
               400L)
})
