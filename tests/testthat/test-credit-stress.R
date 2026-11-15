test_that("sample credit stress reconciles segment and scenario losses", {
  scenarios <- ek_scenario_library()
  result <- ek_credit_scenario_stress(scenarios)
  base <- result$segments[result$segments$scenario_id == "base", ]

  expect_identical(result$portfolio_source, "synthetic_sample")
  expect_true(result$scenario_synthetic)
  expect_equal(nrow(result$summary), 3L)
  expect_equal(nrow(result$segments), 9L)
  expect_equal(base$cumulative_pd, base$pd_12m)
  expect_equal(base$expected_loss, base$ead * base$lgd * base$pd_12m)
  by_scenario <- aggregate(expected_loss ~ scenario_id, result$segments, sum)
  expect_equal(
    result$summary$expected_loss[match(by_scenario$scenario_id, result$summary$scenario_id)],
    by_scenario$expected_loss
  )
  expect_equal(
    result$summary$loss_rate,
    result$summary$expected_loss / result$summary$portfolio_ead
  )
  losses <- stats::setNames(result$summary$expected_loss, result$summary$scenario_id)
  expect_lt(losses[["base"]], losses[["adverse"]])
  expect_lt(losses[["adverse"]], losses[["severe"]])
})

test_that("credit stress uses the complete unemployment path and supplied parameters", {
  scenarios <- ek_scenario_library()
  portfolio <- data.frame(
    segment = c("sensitive", "flat", "zero"),
    ead = c(100, 50, 5),
    pd_12m = c(0.12, 0.20, 0),
    lgd = c(0.5, 0.4, 0.7),
    unemployment_beta = c(0.5, 0, 1)
  )
  result <- ek_credit_scenario_stress(scenarios, portfolio)
  severe <- result$segments[result$segments$scenario_id == "severe", ]
  base_path <- scenarios$paths[scenarios$paths$scenario_id == "base", ]
  severe_path <- scenarios$paths[scenarios$paths$scenario_id == "severe", ]
  expected_pd <- -expm1(
    log1p(-0.12) / 12 *
      sum(exp(0.5 * (severe_path$UNRATE - base_path$UNRATE)))
  )

  expect_identical(result$portfolio_source, "provided")
  expect_equal(severe$cumulative_pd[severe$segment == "sensitive"], expected_pd)
  expect_equal(severe$cumulative_pd[severe$segment == "flat"], 0.20)
  expect_equal(severe$cumulative_pd[severe$segment == "zero"], 0)
  scenarios$paths$UNRATE[scenarios$paths$scenario_id == "severe"] <-
    severe_path$UNRATE + 1
  changed <- ek_credit_scenario_stress(scenarios, portfolio)
  changed_severe <- changed$segments[changed$segments$scenario_id == "severe", ]
  expect_gt(
    changed_severe$cumulative_pd[changed_severe$segment == "sensitive"],
    expected_pd
  )
  expect_equal(changed_severe$cumulative_pd[changed_severe$segment == "flat"], 0.20)
})

test_that("credit stress rejects invalid portfolios and non-finite hazards", {
  portfolio <- data.frame(
    segment = c("a", "b"), ead = c(100, 50), pd_12m = c(0.02, 0.03),
    lgd = c(0.4, 0.5), unemployment_beta = c(0.3, 0.4)
  )
  expect_error(ek_credit_scenario_stress(portfolio = portfolio["segment"]),
               "required")
  invalid <- portfolio
  invalid$maturity <- 12
  expect_error(ek_credit_scenario_stress(portfolio = invalid), "exactly")
  invalid <- portfolio
  invalid$segment[2] <- "a"
  expect_error(ek_credit_scenario_stress(portfolio = invalid), "unique")
  invalid <- portfolio
  invalid$ead <- c(0, 0)
  expect_error(ek_credit_scenario_stress(portfolio = invalid), "positive finite")
  invalid <- portfolio
  invalid$pd_12m[1] <- 1
  expect_error(ek_credit_scenario_stress(portfolio = invalid), "PDs")
  invalid <- portfolio
  invalid$lgd[1] <- Inf
  expect_error(ek_credit_scenario_stress(portfolio = invalid), "finite numeric")
  invalid <- portfolio
  invalid$unemployment_beta[1] <- 1000
  expect_error(ek_credit_scenario_stress(portfolio = invalid), "non-finite hazards")
})

test_that("credit stress requires aligned reference paths for every version", {
  scenarios <- ek_scenario_library()
  expect_error(
    ek_credit_scenario_stress(scenarios, reference_scenario = "missing"),
    "Reference scenario is missing"
  )
  misaligned <- scenarios
  adverse <- misaligned$paths$scenario_id == "adverse"
  misaligned$paths$data_vintage[adverse] <- misaligned$paths$data_vintage[adverse] - 1
  expect_error(ek_credit_scenario_stress(misaligned), "aligned")

  shorter <- scenarios
  drop_adverse_last <- shorter$paths$scenario_id == "adverse" &
    shorter$paths$step == 12
  shorter$paths <- shorter$paths[!drop_adverse_last, , drop = FALSE]
  shorter$asset_returns$horizon_months[
    shorter$asset_returns$scenario_id == "adverse"
  ] <- 11
  expect_error(ek_credit_scenario_stress(shorter), "aligned")

  second <- ek_scenario_library(version = "sample-v2")
  second$paths$UNRATE[second$paths$scenario_id == "base"] <-
    second$paths$UNRATE[second$paths$scenario_id == "base"] + 1
  combined <- ek_scenario_library(
    rbind(scenarios$paths, second$paths),
    rbind(scenarios$asset_returns, second$asset_returns),
    synthetic = TRUE
  )
  summary <- ek_credit_scenario_stress(combined)$summary
  expect_equal(nrow(summary), 6L)
  severe <- summary[summary$scenario_id == "severe", ]
  expect_lt(severe$expected_loss[severe$version == "sample-v2"],
            severe$expected_loss[severe$version == "sample-v1"])
})
