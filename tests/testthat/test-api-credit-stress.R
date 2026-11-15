test_that("credit stress API accepts portfolio JSON and rejects invalid inputs", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = "")
  router <- ek_test_router()
  post <- function(body) ek_test_call(router, "/v1/banking/credit-stress", body = body)
  valid <- paste0(
    '{"portfolio":[{"segment":"CRE","ead":1000000,',
    '"pd_12m":0.025,"lgd":0.4,"unemployment_beta":0.35},',
    '{"segment":"consumer","ead":500000,"pd_12m":0.04,',
    '"lgd":0.6,"unemployment_beta":0.45}]}'
  )
  result <- post(valid)
  expect_equal(result$status, 200L)
  expect_equal(nrow(result$body$summary), 3L)
  expect_equal(nrow(result$body$segments), 6L)
  expect_equal(result$body$summary$expected_loss[result$body$summary$scenario_id == "base"],
               22000)
  expect_identical(result$body$portfolio_source, "provided")
  expect_identical(result$body$scenario_version, "sample-v1")
  expect_null(result$body$run_id)
  expect_identical(post("{}")$body$portfolio_source, "synthetic_sample")

  invalid_segment <- sub('"CRE"', "1", valid)
  result <- post(invalid_segment)
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "`segment` must be a string")

  result <- post(sub('"pd_12m":0.025', '"pd_12m":1', valid))
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "PDs")

  result <- post(sub(',"unemployment_beta":0.35', "", valid))
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "portfolio")

  result <- post(sub("^\\{", "{\"scenario\":\"custom\",", valid))
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "Unknown field")

  result <- post(sub("^\\{", "{\"h\":6,", valid))
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "require `scenarios`")
})

test_that("credit stress API runs BVAR scenarios and records lineage", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  router <- ek_test_router()
  body <- paste0(
    '{"vars":["FEDFUNDS","UNRATE"],"p":1,"h":12,"draws":5,"version":"rates-v1",',
    '"scenarios":{"base":{},"hike":{"FEDFUNDS":[7,7,7,7,7,7]}}}'
  )
  result <- ek_test_call(router, "/v1/banking/credit-stress", body = body)
  expect_equal(result$status, 200L)
  expect_equal(sort(result$body$summary$scenario_id), c("base", "hike"))
  expect_equal(result$body$summary$horizon_months, c(12L, 12L))
  expect_identical(result$body$scenario_version, "rates-v1")
  expect_true(result$body$scenario_synthetic)
  expect_type(result$body$run_id, "character")

  runs <- ek_read_runs(log)
  expect_equal(runs$model_id, "credit_scenario_stress")
  expect_equal(runs$run_id, result$body$run_id)
  expect_equal(runs$sources, "sample")
  expect_match(runs$metrics, "loss_rate")
  expect_equal(jsonlite::fromJSON(runs$params)$reference_scenario, "base")

  no_unrate <- sub('"UNRATE"', '"INDPRO"', body)
  result <- ek_test_call(router, "/v1/banking/credit-stress", body = no_unrate)
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "UNRATE")
  no_base <- sub('"base":\\{\\},', "", body)
  result <- ek_test_call(router, "/v1/banking/credit-stress", body = no_base)
  expect_equal(result$status, 400L)
  expect_match(result$body$error, "Reference scenario")
})

test_that("PD sensitivity API returns sample calibration", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "")
  router <- ek_test_router()
  result <- ek_test_call(router, "/v1/banking/pd-sensitivity")
  expect_equal(result$status, 200L)
  expect_true(result$body$meta$synthetic)
  unrate <- result$body$coefficients[result$body$coefficients$term == "UNRATE", ]
  expect_equal(nrow(unrate), 3L)
  expect_true(all(unrate$estimate > 0))
  expect_equal(nrow(result$body$fit), 3L)
  expect_equal(ek_test_call(router, "/v1/banking/pd-sensitivity", "drivers=NOPE")$status,
               400L)
})
