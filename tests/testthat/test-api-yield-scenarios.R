test_that("rates API projects sample curves and records lineage", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  router <- ek_test_router()
  result <- ek_test_call(
    router, "/v1/rates/scenarios",
    body = '{"ar":true,"curve_lambda":0.6}'
  )

  expect_equal(result$status, 200L)
  expect_equal(result$body$meta$model_version, "curve-satellite-v1")
  expect_true(result$body$meta$synthetic)
  expect_equal(result$body$meta$drivers, c("UNRATE", "NFCI"))
  expect_equal(length(result$body$meta$scenarios), 3L)
  expect_equal(nrow(result$body$factors), 36L)
  expect_equal(nrow(result$body$yields), 36L)
  expect_true(all(c("DGS3MO", "DGS2", "DGS5", "DGS10", "DGS30") %in%
                    names(result$body$yields)))
  expect_type(result$body$meta$run_id, "character")
  runs <- ek_read_runs(log)
  expect_equal(runs$model_id, "yield_curve_satellite")
  expect_equal(runs$run_id, result$body$meta$run_id)
  expect_match(runs$metrics, "r_squared")

  invalid <- ek_test_call(
    router, "/v1/rates/scenarios", body = '{"ar":"yes"}'
  )
  expect_equal(invalid$status, 400L)
  expect_match(invalid$body$error, "`ar`")
  no_scenarios <- ek_test_call(
    router, "/v1/rates/scenarios", body = '{"h":6}'
  )
  expect_equal(no_scenarios$status, 400L)
  expect_match(no_scenarios$body$error, "require `scenarios`")
})

test_that("rates API builds conditional BVAR rate scenarios", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = "")
  body <- paste0(
    '{"vars":["FEDFUNDS","UNRATE","NFCI"],"p":1,"h":12,"draws":5,',
    '"scenarios":{"base":{},"downturn":{"UNRATE":[10.5,11,11.5,12]}}}'
  )
  result <- ek_test_call(ek_test_router(), "/v1/rates/scenarios", body = body)

  expect_equal(result$status, 200L)
  expect_equal(result$body$meta$scenario_version, "bvar-v1")
  expect_equal(result$body$meta$scenarios, c("base", "downturn"))
  expect_equal(nrow(result$body$yields), 24L)

  missing_driver <- sub(',"NFCI"', "", body)
  invalid <- ek_test_call(ek_test_router(), "/v1/rates/scenarios", body = missing_driver)
  expect_equal(invalid$status, 400L)
  expect_match(invalid$body$error, "must include curve drivers")
})
