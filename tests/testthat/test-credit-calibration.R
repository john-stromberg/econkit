test_that("sample calibration recovers the generating sensitivities", {
  sens <- ek_credit_pd_sensitivity()
  unrate <- sens$coefficients[sens$coefficients$term == "UNRATE", ]
  expect_equal(unrate$segment,
               c("commercial_industrial", "commercial_real_estate", "consumer"))
  expect_equal(unrate$estimate, c(0.15, 0.20, 0.25), tolerance = 0.15)
  expect_true(all(unrate$std_error > 0))
  expect_true(all(sens$fit$r_squared > 0.3))
  expect_equal(sens$fit$n_obs, rep(224L, 3))
  expect_equal(sens$fit$start, rep(as.Date("1970-01-01"), 3))
  expect_true(sens$synthetic)
  expect_s3_class(sens$models$consumer, "ek_satellite")

  portfolio <- data.frame(segment = unrate$segment, ead = c(200, 250, 100),
                          pd_12m = c(0.02, 0.025, 0.04), lgd = c(0.45, 0.4, 0.6),
                          unemployment_beta = unrate$estimate)
  losses <- ek_credit_scenario_stress(portfolio = portfolio)$summary
  expect_lt(losses$expected_loss[losses$scenario_id == "base"],
            losses$expected_loss[losses$scenario_id == "severe"])
})

test_that("calibration is exact on noiseless custom histories", {
  macro <- data.frame(date = seq(as.Date("2000-01-01"), by = "month", length.out = 60),
                      UNRATE = 4 + 3 * sin(seq_len(60) / 6), NFCI = cos(seq_len(60) / 4))
  hazard <- 0.03 * exp(0.3 * (macro$UNRATE - 5) + 0.5 * macro$NFCI)
  history <- data.frame(date = macro$date, segment = "cards", default_rate = -expm1(-hazard))
  history$default_rate[5] <- NA
  sens <- ek_credit_pd_sensitivity(history, macro, drivers = c("UNRATE", "NFCI"))
  est <- stats::setNames(sens$coefficients$estimate, sens$coefficients$term)
  expect_equal(est[["UNRATE"]], 0.3)
  expect_equal(est[["NFCI"]], 0.5)
  expect_equal(est[["const"]], log(0.03) - 1.5)
  expect_equal(sens$fit$n_obs, 59L)
  expect_equal(sens$models$cards$freq, 12L)
  expect_false(sens$synthetic)
})

test_that("calibration rejects invalid histories", {
  history <- ek_load_dataset("credit_default_rates")
  bad <- history
  bad$default_rate[1] <- 1
  expect_error(ek_credit_pd_sensitivity(bad), "\\[0, 1\\)")
  expect_error(ek_credit_pd_sensitivity(rbind(history, history[1, ])), "one row per")
  expect_error(ek_credit_pd_sensitivity(history[c("date", "segment")]), "default_rate")
  expect_error(ek_credit_pd_sensitivity(drivers = c("UNRATE", "UNRATE")), "distinct")
  macro <- data.frame(date = as.Date("1900-01-01"), UNRATE = 5)
  expect_error(ek_credit_pd_sensitivity(history, macro), "No complete")
  expect_error(ek_credit_pd_sensitivity(history, macro[c(1, 1), ]), "one row per date")
})
