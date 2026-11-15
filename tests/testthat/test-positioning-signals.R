test_that("sample asset returns are labelled synthetic and aligned forward", {
  expect_true(isTRUE(attr(asset_returns, "synthetic")))
  expect_named(asset_returns, c(
    "date", "global_equity", "government_bonds", "investment_grade_credit",
    "high_yield_credit", "commodities", "gold", "us_dollar", "cash"
  ))

  regimes <- data.frame(
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    regime = c("Recovery", "Recovery", "Stagflation")
  )
  returns <- data.frame(
    date = regimes$date,
    a = c(0, 0.1, 0.2),
    b = c(0, -0.1, -0.2)
  )
  pb <- ek_positioning_playbook(regimes, returns, min_obs = 3)
  expect_equal(pb$mean_return[pb$regime == "Recovery" & pb$asset_class == "a"], 0.15)
  expect_equal(pb$n_obs[pb$regime == "Recovery" & pb$asset_class == "a"], 2L)
  expect_false(pb$eligible[pb$regime == "Recovery" & pb$asset_class == "a"])
  expect_equal(pb$mean_return[pb$regime == "All" & pb$asset_class == "a"], 0.15)
})

test_that("tilts respect neutrality, turnover, benchmark and evidence constraints", {
  pb <- data.frame(
    regime = rep(c("All", "Recovery"), each = 3),
    asset_class = rep(c("a", "b", "c"), 2),
    n_obs = c(100, 100, 100, 60, 60, 10),
    mean_return = c(0.01, 0.01, 0.01, 0.03, -0.02, 0.50),
    monthly_volatility = rep(0.05, 6),
    eligible = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  benchmark <- c(a = 0.01, b = 0.09, c = 0.9)
  tilts <- ek_positioning_tilts(pb, "Recovery", benchmark,
                                max_tilt = 0.1, max_turnover = 0.04)
  expect_equal(sum(tilts$active_tilt), 0, tolerance = 1e-8)
  expect_lte(sum(abs(tilts$active_tilt)) / 2, 0.04 + 1e-8)
  expect_lte(max(abs(tilts$active_tilt)), 0.1 + 1e-8)
  expect_true(all(tilts$suggested_weight >= 0))
  expect_equal(sum(tilts$suggested_weight), 1, tolerance = 1e-8)
  expect_equal(tilts$active_tilt[tilts$asset_class == "c"], 0)
  expect_error(ek_positioning_tilts(pb, "Recovery", c(a = 0.5, b = 0.4, c = 0.2)),
               "summing to one")
})

test_that("positioning gap compares current book with suggested weights", {
  pb <- data.frame(
    regime = rep(c("All", "Recovery"), each = 2),
    asset_class = rep(c("a", "b"), 2),
    n_obs = rep(40, 4),
    mean_return = c(0.01, 0.01, 0.03, -0.01),
    monthly_volatility = rep(0.05, 4),
    eligible = rep(TRUE, 4)
  )
  tilts <- ek_positioning_tilts(pb, "Recovery", c(a = 0.6, b = 0.4))
  gap <- ek_positioning_gap(tilts, c(a = 0.5, b = 0.5))
  expect_equal(gap$gap, gap$suggested_weight - gap$current_weight)
  expect_equal(sum(gap$gap), 0, tolerance = 1e-8)
  expect_error(ek_positioning_gap(tilts, c(a = 0.4, b = 0.4)), "sum to one")
})

test_that("signal builder is point-in-time with delayed recession labels", {
  data <- macro_monthly
  cutoff <- as.Date("2018-12-01")
  signals <- ek_build_signals(data, as_of = cutoff)
  expect_s3_class(signals, "ek_signals")
  expect_equal(unique(signals$as_of), cutoff)
  expect_equal(unique(signals$data_vintage), cutoff)
  expect_true(all(c("signal", "value", "label", "direction", "percentile",
                    "confidence", "n_obs", "model_version", "git_sha",
                    "change_vs_prior", "override", "override_reason", "user") %in%
                    names(signals)))
  expect_true(all(signals$n_obs > 0))
  expect_true(all(signals$percentile >= 0 & signals$percentile <= 1, na.rm = TRUE))

  changed <- data
  i <- max(which(data$date <= cutoff))
  last_label <- i - 12 - 6
  unknown_start <- last_label + 12 + 1
  changed$USREC[unknown_start:i] <- 1L - changed$USREC[unknown_start:i]
  signals_changed <- ek_build_signals(changed, as_of = cutoff)
  p1 <- signals$value[signals$signal == "recession_probability"]
  p2 <- signals_changed$value[signals_changed$signal == "recession_probability"]
  expect_equal(p1, p2)
  expect_error(ek_build_signals(data, as_of = "1969-01-01"), "No data")
})

test_that("signal store is append-only, idempotent and supports as-of reads", {
  data <- macro_monthly
  old <- ek_build_signals(data, as_of = as.Date("2018-12-01"))
  new <- ek_build_signals(data, as_of = as.Date("2019-01-01"))
  path <- normalizePath(tempfile(fileext = ".rds"), winslash = "/", mustWork = FALSE)
  on.exit(unlink(path), add = TRUE)

  expect_equal(ek_store_signals(old, path), path)
  expect_equal(ek_store_signals(old, path), path)
  ek_store_signals(new, path)
  history <- readRDS(path)
  expect_equal(nrow(history), nrow(old) + nrow(new))
  current <- ek_read_signals(path)
  expect_true(all(current$as_of == as.Date("2019-01-01")))
  prior <- ek_read_signals(path, as_of = "2018-12-15")
  expect_true(all(prior$as_of == as.Date("2018-12-01")))
  delta <- current$value[current$signal == "term_spread"] -
    prior$value[prior$signal == "term_spread"]
  expect_equal(current$change_vs_prior[current$signal == "term_spread"], delta)

  changed <- old
  changed$value[changed$signal == "term_spread"] <- 999
  expect_error(ek_store_signals(changed, path), "immutable")
})
