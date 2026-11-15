test_that("ECL API serves sample and supplied portfolios", {
  skip_if_not_installed("plumber")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = "")
  router <- ek_test_router()

  sample <- ek_test_call(router, "/v1/banking/ecl")
  expect_equal(sample$status, 200L)
  expect_equal(sample$body$meta$standard, "cecl")
  expect_false(sample$body$meta$discount)
  expect_equal(nrow(sample$body$summary), 3L)
  expect_null(sample$body$weighted)
  expect_null(sample$body$paths)
  expect_equal(sample$body$summary$ecl, ek_credit_lifetime_ecl()$summary$ecl, tolerance = 1e-6)
  expect_equal(ek_test_call(router, "/v1/banking/ecl", "standard=gaap")$status, 400L)

  body <- paste0(
    '{"standard":"ifrs9","include_paths":true,',
    '"weights":{"base":0.6,"adverse":0.3,"severe":0.1},',
    '"portfolio":[',
    '{"segment":"cards","ead":1000,"pd_12m":0.05,"lgd":0.8,"unemployment_beta":0.4,',
    '"term_months":36,"rate":0.18,"amortization":"linear","stage":2},',
    '{"segment":"cre","ead":5000,"pd_12m":0.02,"lgd":0.4,"unemployment_beta":0.3,',
    '"term_months":60,"rate":0.06,"amortization":"bullet","stage":1}]}'
  )
  result <- ek_test_call(router, "/v1/banking/ecl", body = body)
  expect_equal(result$status, 200L)
  portfolio <- data.frame(segment = c("cards", "cre"), ead = c(1000, 5000),
                          pd_12m = c(0.05, 0.02), lgd = c(0.8, 0.4),
                          unemployment_beta = c(0.4, 0.3), term_months = c(36, 60),
                          rate = c(0.18, 0.06), amortization = c("linear", "bullet"),
                          stage = c(2, 1))
  expected <- ek_credit_lifetime_ecl(portfolio = portfolio, standard = "ifrs9",
                                     weights = c(base = 0.6, adverse = 0.3, severe = 0.1))
  expect_equal(result$body$weighted$ecl, expected$weighted$ecl, tolerance = 1e-6)
  expect_equal(nrow(result$body$weighted_segments), 2L)
  expect_true(result$body$meta$discount)
  expect_equal(result$body$meta$weights$base, 0.6)
  expect_equal(nrow(result$body$paths), 3L * 36L + 3L * 12L)
  expect_type(result$body$paths$date, "character")
  expect_null(result$body$meta$run_id)

  partial <- sub(',"amortization":"bullet","stage":1', "", body)
  expect_match(ek_test_call(router, "/v1/banking/ecl", body = partial)$body$error,
               "portfolio")
  wrong_type <- sub('"amortization":"linear"', '"amortization":1', body)
  expect_match(ek_test_call(router, "/v1/banking/ecl", body = wrong_type)$body$error,
               "`amortization` must be a string")
  no_severe <- sub(',"severe":0.1', "", body)
  expect_match(ek_test_call(router, "/v1/banking/ecl", body = no_severe)$body$error,
               "exactly the scenarios")
  flag <- sub('"include_paths":true', '"include_paths":"yes"', body)
  expect_equal(ek_test_call(router, "/v1/banking/ecl", body = flag)$status, 400L)
  expect_match(ek_test_call(router, "/v1/banking/ecl", body = '{"h":6}')$body$error,
               "require `scenarios`")
})

test_that("ECL API runs BVAR scenarios and records lineage", {
  skip_if_not_installed("plumber")
  log <- withr::local_tempfile(fileext = ".jsonl")
  withr::local_envvar(ECONKIT_API_KEYS = "", ECONKIT_RUN_LOG = log)
  router <- ek_test_router()
  body <- paste0(
    '{"vars":["FEDFUNDS","UNRATE"],"p":1,"h":12,"draws":5,"anchor":5.5,',
    '"weights":{"base":0.7,"downturn":0.3},',
    '"scenarios":{"base":{},"downturn":{"UNRATE":[10.5,11,11.5,12]}}}'
  )
  result <- ek_test_call(router, "/v1/banking/ecl", body = body)
  expect_equal(result$status, 200L)
  expect_equal(result$body$meta$anchor, 5.5)
  expect_equal(result$body$meta$scenario_version, "bvar-v1")
  losses <- stats::setNames(result$body$summary$ecl, result$body$summary$scenario_id)
  expect_gt(losses[["downturn"]], losses[["base"]])
  runs <- ek_read_runs(log)
  expect_equal(runs$model_id, "credit_lifetime_ecl")
  expect_equal(runs$run_id, result$body$meta$run_id)
  expect_match(runs$metrics, "ecl_rate")
})
