test_that("sample scenario library is versioned, monthly and synthetic", {
  scenarios <- ek_scenario_library(
    data = macro_monthly,
    as_of = as.Date("2018-12-01"),
    version = "test-v1"
  )

  expect_s3_class(scenarios, "ek_scenario_library")
  expect_true(scenarios$synthetic)
  expect_setequal(unique(scenarios$paths$scenario_id),
                  c("base", "adverse", "severe"))
  expect_equal(unique(scenarios$paths$version), "test-v1")
  expect_true(all(scenarios$paths$data_vintage == as.Date("2018-12-01")))
  expect_true(all(scenarios$paths$date > scenarios$paths$as_of))
  expect_equal(nrow(scenarios$paths), 36L)
  expect_equal(nrow(scenarios$asset_returns), 24L)
  expect_invisible(ek_validate_scenario_library(scenarios))

  custom <- ek_scenario_library(
    scenarios$paths, scenarios$asset_returns, synthetic = TRUE
  )
  expect_true(custom$synthetic)
})

test_that("SAA scenario stress applies weights to cumulative asset shocks", {
  scenarios <- ek_scenario_library()
  assets <- sort(unique(scenarios$asset_returns$asset_class))
  weights <- stats::setNames(rep(1 / length(assets), length(assets)), assets)
  stress <- ek_saa_scenario_stress(scenarios, weights)

  expect_true(stress$synthetic)
  expect_equal(nrow(stress$summary), 3L)
  expect_lt(
    stress$summary$portfolio_return[stress$summary$scenario_id == "severe"],
    stress$summary$portfolio_return[stress$summary$scenario_id == "adverse"]
  )
  expect_lt(
    stress$summary$portfolio_return[stress$summary$scenario_id == "adverse"],
    stress$summary$portfolio_return[stress$summary$scenario_id == "base"]
  )
  contributions <- aggregate(
    contribution ~ scenario_id, stress$contributions, sum
  )
  expect_equal(
    stress$summary$portfolio_return[
      match(contributions$scenario_id, stress$summary$scenario_id)
    ],
    contributions$contribution
  )
  expect_error(
    ek_saa_scenario_stress(scenarios, c(global_equity = 1)),
    "exactly the assets"
  )
})

test_that("scenario validation rejects broken monthly paths and shock keys", {
  scenarios <- ek_scenario_library()
  broken_path <- scenarios
  broken_path$paths$date[1] <- broken_path$paths$date[1] + 1
  expect_error(
    ek_validate_scenario_library(broken_path),
    "contiguous monthly steps"
  )

  broken_shocks <- scenarios
  broken_shocks$asset_returns <- broken_shocks$asset_returns[-1, ]
  expect_error(
    ek_validate_scenario_library(broken_shocks),
    "same asset classes"
  )
})
