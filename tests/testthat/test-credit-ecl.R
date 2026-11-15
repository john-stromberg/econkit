ecl_portfolio <- function(...) {
  base <- list(segment = "loans", ead = 1000, pd_12m = 0.05, lgd = 0.4,
               unemployment_beta = 0.3, term_months = 12, rate = 0.06)
  extra <- list(...)
  base[names(extra)] <- extra
  as.data.frame(base, stringsAsFactors = FALSE)
}

test_that("lifetime ECL matches closed forms on the reference path", {
  scenarios <- ek_scenario_library()
  one_year <- ek_credit_lifetime_ecl(scenarios, ecl_portfolio())
  base <- one_year$segments[one_year$segments$scenario_id == "base", ]
  expect_equal(base$ecl, 1000 * 0.4 * 0.05)
  expect_equal(base$horizon_pd, 0.05)
  expect_false(one_year$discount)
  expect_null(one_year$weighted)

  two_year <- ek_credit_lifetime_ecl(scenarios, ecl_portfolio(term_months = 24))
  base <- two_year$segments[two_year$segments$scenario_id == "base", ]
  expect_equal(base$horizon_pd, 1 - 0.95^2)
  expect_equal(base$ecl, 1000 * 0.4 * (1 - 0.95^2))

  survival <- 0.95^((0:24) / 12)
  marginal <- -diff(survival)
  linear <- ek_credit_lifetime_ecl(
    scenarios, ecl_portfolio(term_months = 24, amortization = "linear", stage = 2),
    standard = "ifrs9"
  )
  base <- linear$segments[linear$segments$scenario_id == "base", ]
  expected <- sum(marginal * 0.4 * 1000 * (24:1) / 24 * 1.06^(-(1:24) / 12))
  expect_equal(base$ecl, expected)
  expect_true(linear$discount)
  paths <- linear$paths[linear$paths$scenario_id == "base", ]
  expect_equal(nrow(paths), 24L)
  expect_equal(paths$date[1], as.Date("2026-01-01"))
  expect_equal(sum(paths$expected_loss), base$ecl)
})

test_that("one-year bullet ECL equals the credit stress loss", {
  scenarios <- ek_scenario_library()
  portfolio <- ecl_portfolio(segment = c("a", "b"), ead = c(100, 50),
                             pd_12m = c(0.02, 0.08), unemployment_beta = c(0.5, 0.1))
  ecl <- ek_credit_lifetime_ecl(scenarios, portfolio)
  stress <- ek_credit_scenario_stress(scenarios, portfolio[1:5])
  expect_equal(ecl$summary$ecl, stress$summary$expected_loss)
  expect_equal(ecl$segments$horizon_pd, stress$segments$cumulative_pd)
})

test_that("adjustments revert after the scenario horizon", {
  scenarios <- ek_scenario_library()
  result <- ek_credit_lifetime_ecl(scenarios, ecl_portfolio(term_months = 36),
                                   reversion_months = 6)
  severe <- result$paths[result$paths$scenario_id == "severe", ]
  gap <- scenarios$paths$UNRATE[scenarios$paths$scenario_id == "severe"] -
    scenarios$paths$UNRATE[scenarios$paths$scenario_id == "base"]
  expect_equal(severe$unemployment_gap[1:12], gap)
  expect_equal(severe$unemployment_gap[13:18], gap[12] * (1 - (1:6) / 6))
  expect_equal(severe$hazard_multiplier[19:36], rep(1, 18))
  expect_equal(severe$hazard_multiplier[13], exp(0.3 * gap[12] * 5 / 6))
  instant <- ek_credit_lifetime_ecl(scenarios, ecl_portfolio(term_months = 36),
                                    reversion_months = 0)
  expect_equal(instant$paths$hazard_multiplier[instant$paths$month > 12], rep(1, 72))
  losses <- stats::setNames(result$summary$ecl, result$summary$scenario_id)
  expect_lt(losses[["base"]], losses[["adverse"]])
  expect_lt(losses[["adverse"]], losses[["severe"]])
  expect_lt(instant$summary$ecl[instant$summary$scenario_id == "severe"], losses[["severe"]])

  anchored <- ek_credit_lifetime_ecl(scenarios, ecl_portfolio(), anchor = 5)
  expect_gt(anchored$summary$ecl[anchored$summary$scenario_id == "base"],
            1000 * 0.4 * 0.05)
  expect_true(is.na(anchored$reference_scenario))
  expect_equal(anchored$anchor, 5)
})

test_that("IFRS 9 stages and scenario weights", {
  scenarios <- ek_scenario_library()
  portfolio <- ecl_portfolio(segment = c("s1", "s2", "s3"), ead = c(100, 100, 100),
                             term_months = 60, stage = c(1, 2, 3))
  ifrs9 <- ek_credit_lifetime_ecl(scenarios, portfolio, standard = "ifrs9", discount = FALSE)
  base <- ifrs9$segments[ifrs9$segments$scenario_id == "base", ]
  expect_equal(base$horizon_months, c(12L, 60L, 0L))
  expect_equal(base$ecl[1], 100 * 0.4 * 0.05)
  expect_equal(base$ecl[3], 100 * 0.4)
  expect_equal(base$horizon_pd[3], 1)
  expect_false(any(ifrs9$paths$segment == "s3"))
  cecl <- ek_credit_lifetime_ecl(scenarios, portfolio)
  expect_equal(cecl$segments$horizon_months[cecl$segments$segment == "s1"], rep(60L, 3))

  weights <- c(severe = 0.1, base = 0.6, adverse = 0.3)
  weighted <- ek_credit_lifetime_ecl(scenarios, portfolio, weights = weights)
  by_scenario <- stats::setNames(weighted$summary$ecl, weighted$summary$scenario_id)
  expect_equal(weighted$weighted$ecl, sum(by_scenario[names(weights)] * weights))
  expect_equal(weighted$weighted$ecl_rate, weighted$weighted$ecl / 300)
  expect_equal(sum(weighted$weighted_segments$ecl), weighted$weighted$ecl)
  expect_equal(weighted$weighted_segments$ecl[weighted$weighted_segments$segment == "s3"], 40)
  expect_error(ek_credit_lifetime_ecl(scenarios, portfolio, weights = weights[1:2]),
               "exactly the scenarios")
  expect_error(ek_credit_lifetime_ecl(scenarios, portfolio, weights = weights * 2), "sum to 1")
  expect_error(ek_credit_lifetime_ecl(scenarios, portfolio, weights = c(0.5, 0.5)), "named")
})

test_that("lifetime ECL rejects invalid inputs", {
  bad <- function(...) ek_credit_lifetime_ecl(portfolio = ecl_portfolio(...))
  expect_error(bad(term_months = 0), "term_months")
  expect_error(bad(term_months = 12.5), "term_months")
  expect_error(bad(rate = 1), "rate")
  expect_error(bad(amortization = "balloon"), "amortization")
  expect_error(bad(stage = 4), "stage")
  expect_error(bad(pd_12m = 1), "PDs")
  expect_error(ek_credit_lifetime_ecl(portfolio = ecl_portfolio()[-6]), "term_months")
  expect_error(ek_credit_lifetime_ecl(portfolio = ecl_portfolio(extra = 1)), "optionally")
  expect_error(ek_credit_lifetime_ecl(reversion_months = -1), "reversion_months")
  expect_error(ek_credit_lifetime_ecl(anchor = NA_real_), "anchor")
  expect_error(ek_credit_lifetime_ecl(discount = NA), "discount")
  expect_error(ek_credit_lifetime_ecl(standard = "gaap"))
  expect_error(ek_credit_lifetime_ecl(reference_scenario = "missing"), "Reference scenario")
  sample <- ek_credit_lifetime_ecl()
  expect_identical(sample$portfolio_source, "synthetic_sample")
  expect_equal(nrow(sample$segments), 9L)
})
