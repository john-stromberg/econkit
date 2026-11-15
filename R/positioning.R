#' Regime-conditioned asset-class return summary
#'
#' Estimates forward one-month asset-class return and risk statistics for
#' each growth/inflation regime. The regime at month `t` is matched to returns
#' in month `t + 1`; this timing avoids using the return that occurred before
#' the signal was observable. The `"All"` rows are the unconditional
#' benchmark. The default bundled returns are synthetic and for workflow
#' demonstration only.
#'
#' @section Use and limitations:
#' This is descriptive historical analysis, not a forecast or investment
#' recommendation. Regime samples can be small and the synthetic sample has
#' deliberately regime-dependent returns. `eligible` flags whether a row has
#' at least `min_obs` observations; the sample size and confidence interval
#' should be shown with any result. Sharpe ratios use zero as the reference
#' return and annualise monthly arithmetic mean and standard deviation.
#'
#' @param data Monthly macro data with `date`, `INDPRO` and `CPIAUCSL`
#'   columns, or `date` and an existing `regime` column.
#' @param returns Monthly asset returns with `date` and one numeric column per
#'   asset class. Returns are decimal, not percent.
#' @param min_obs Minimum observations for a playbook row to be eligible for
#'   tilt calculations.
#' @param window Momentum window in months if regimes must be derived.
#' @return A data frame with `regime`, `asset_class`, `n_obs`, monthly mean
#'   and volatility, annualised mean and volatility, Sharpe ratio, confidence
#'   interval for the monthly mean, and `eligible`.
#' @export
#' @examples
#' playbook <- ek_positioning_playbook()
#' subset(playbook, regime == "Stagflation")
ek_positioning_playbook <- function(data = NULL, returns = NULL,
                                    min_obs = 24L, window = 3L) {
  if (is.null(data)) data <- ek_load_dataset("macro_monthly")
  if (is.null(returns)) returns <- ek_load_dataset("asset_returns")
  if (!all(c("date") %in% names(data)) || !"date" %in% names(returns)) {
    stop("`data` and `returns` must have a `date` column.", call. = FALSE)
  }
  if (!"regime" %in% names(data)) {
    need <- c("INDPRO", "CPIAUCSL")
    missing <- setdiff(need, names(data))
    if (length(missing)) {
      stop("`data` needs `regime` or columns: ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    data$regime <- ek_growth_inflation_regime(
      ek_transform(data$INDPRO, "yoy"),
      ek_transform(data$CPIAUCSL, "yoy"),
      window = window
    )
  }
  if (min_obs < 2) stop("`min_obs` must be at least 2.", call. = FALSE)
  if (anyDuplicated(data$date) || anyDuplicated(returns$date)) {
    stop("`date` values must be unique in `data` and `returns`.", call. = FALSE)
  }
  assets <- setdiff(names(returns), "date")
  if (!length(assets) || !all(vapply(returns[assets], is.numeric, logical(1)))) {
    stop("`returns` must contain at least one numeric asset return column.", call. = FALSE)
  }
  if (!inherits(data$date, "Date") || !inherits(returns$date, "Date")) {
    stop("Both `date` columns must have class Date.", call. = FALSE)
  }

  outcome_date <- ek_add_months(data$date, 1)
  outcome_row <- match(outcome_date, returns$date)
  aligned <- data.frame(
    regime = as.character(data$regime),
    signal_date = data$date,
    return_date = outcome_date,
    returns[assets][outcome_row, , drop = FALSE],
    check.names = FALSE
  )
  aligned <- aligned[!is.na(aligned$regime) & !is.na(outcome_row), , drop = FALSE]
  all_rows <- aligned
  all_rows$regime <- "All"
  aligned <- rbind(aligned, all_rows)
  regime_names <- unique(c("All", sort(unique(aligned$regime[aligned$regime != "All"]))))

  rows <- lapply(regime_names, function(regime_name) {
    part <- aligned[aligned$regime == regime_name, , drop = FALSE]
    lapply(assets, function(asset) {
      x <- part[[asset]]
      x <- x[is.finite(x)]
      n <- length(x)
      avg <- if (n) mean(x) else NA_real_
      vol <- if (n > 1) stats::sd(x) else NA_real_
      half_width <- if (n > 1) {
        stats::qt(0.975, df = n - 1) * vol / sqrt(n)
      } else {
        NA_real_
      }
      data.frame(
        regime = regime_name,
        asset_class = asset,
        n_obs = n,
        mean_return = avg,
        monthly_volatility = vol,
        annualized_mean = 12 * avg,
        annualized_volatility = sqrt(12) * vol,
        sharpe = if (is.finite(vol) && vol > 0) sqrt(12) * avg / vol else NA_real_,
        ci_lower = avg - half_width,
        ci_upper = avg + half_width,
        eligible = n >= min_obs
      )
    })
  })
  out <- do.call(rbind, unlist(rows, recursive = FALSE))
  rownames(out) <- NULL
  attr(out, "synthetic") <- isTRUE(attr(returns, "synthetic"))
  out
}

#' Produce constrained, regime-conditioned portfolio tilts
#'
#' Converts differences between a regime's historical mean returns and
#' unconditional mean returns into small, dollar-neutral active weights.
#' Scores are volatility-scaled and shrunk toward zero for smaller samples.
#' This transparent heuristic is not an optimiser or a forecast; it is
#' intended as a review input, not an instruction to trade.
#'
#' @param playbook Output of [ek_positioning_playbook()].
#' @param regime The current regime name in `playbook`.
#' @param benchmark Named numeric vector of strategic weights summing to one.
#' @param max_tilt Maximum absolute active weight per asset, expressed as a
#'   fraction of portfolio value.
#' @param max_turnover Maximum one-way turnover (half the sum of absolute
#'   active weights).
#' @param shrinkage Number of observations controlling shrinkage of the
#'   historical excess-return score.
#' @return A data frame with benchmark weight, active tilt and suggested
#'   weight by `asset_class`, plus evidence and explanatory fields.
#' @export
#' @examples
#' pb <- ek_positioning_playbook()
#' b <- c(global_equity = 0.4, government_bonds = 0.25,
#'        investment_grade_credit = 0.1, high_yield_credit = 0.05,
#'        commodities = 0.08, gold = 0.05, us_dollar = 0.02, cash = 0.05)
#' ek_positioning_tilts(pb, "Recovery", b)
ek_positioning_tilts <- function(playbook, regime, benchmark, max_tilt = 0.05,
                                 max_turnover = 0.10, shrinkage = 24) {
  if (!is.character(regime) || length(regime) != 1L || is.na(regime) ||
        !nzchar(regime)) {
    stop("`regime` must be one non-empty regime name.", call. = FALSE)
  }
  if (!is.numeric(benchmark) || is.null(names(benchmark)) ||
        anyDuplicated(names(benchmark)) || any(!is.finite(benchmark)) ||
        any(benchmark < 0) || abs(sum(benchmark) - 1) > 1e-8) {
    stop("`benchmark` must be a named, non-negative vector summing to one.",
         call. = FALSE)
  }
  if (length(max_tilt) != 1L || !is.finite(max_tilt) || max_tilt < 0 ||
        max_tilt > 1 || length(max_turnover) != 1L || !is.finite(max_turnover) ||
        max_turnover < 0 || max_turnover > 1 || length(shrinkage) != 1L ||
        !is.finite(shrinkage) || shrinkage < 0) {
    stop("`max_tilt`, `max_turnover` and `shrinkage` are outside valid ranges.",
         call. = FALSE)
  }
  needed <- c("regime", "asset_class", "n_obs", "mean_return",
              "monthly_volatility", "eligible")
  if (!all(needed %in% names(playbook))) {
    stop("`playbook` is missing required columns.", call. = FALSE)
  }
  current <- playbook[playbook$regime == regime, , drop = FALSE]
  base <- playbook[playbook$regime == "All", , drop = FALSE]
  if (!nrow(current) || !nrow(base)) {
    stop("`regime` and unconditional `All` rows must be present in `playbook`.",
         call. = FALSE)
  }
  if (anyDuplicated(current$asset_class) || anyDuplicated(base$asset_class)) {
    stop("There must be one playbook row per regime and asset class.", call. = FALSE)
  }
  missing <- union(
    setdiff(names(benchmark), current$asset_class),
    setdiff(names(benchmark), base$asset_class)
  )
  if (length(missing)) {
    stop("Assets missing from playbook: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  current <- current[match(names(benchmark), current$asset_class), , drop = FALSE]
  base <- base[match(names(benchmark), base$asset_class), , drop = FALSE]
  eligible <- current$eligible & is.finite(current$mean_return) &
    base$eligible & is.finite(base$mean_return) & is.finite(base$monthly_volatility) &
    base$monthly_volatility > 0
  alpha <- current$mean_return - base$mean_return
  score <- rep(0, length(benchmark))
  score[eligible] <- alpha[eligible] / base$monthly_volatility[eligible] *
    current$n_obs[eligible] / (current$n_obs[eligible] + shrinkage)
  if (any(eligible)) score[eligible] <- score[eligible] - mean(score[eligible])
  if (max(abs(score)) > 0 && max_tilt > 0) {
    raw_tilt <- score * max_tilt / max(abs(score))
    lower <- ifelse(eligible, pmax(-as.numeric(benchmark), -max_tilt), 0)
    upper <- ifelse(eligible, max_tilt, 0)
    lo <- min(raw_tilt - upper) - 1
    hi <- max(raw_tilt - lower) + 1
    for (i in seq_len(100)) {
      offset <- (lo + hi) / 2
      if (sum(pmax(lower, pmin(upper, raw_tilt - offset))) > 0) {
        lo <- offset
      } else {
        hi <- offset
      }
    }
    tilt <- pmax(lower, pmin(upper, raw_tilt - (lo + hi) / 2))
    turnover <- sum(abs(tilt)) / 2
    if (turnover > max_turnover && turnover > 0) tilt <- tilt * max_turnover / turnover
  } else {
    tilt <- rep(0, length(benchmark))
  }

  data.frame(
    asset_class = names(benchmark),
    benchmark_weight = as.numeric(benchmark),
    active_tilt = tilt,
    suggested_weight = as.numeric(benchmark) + tilt,
    regime = regime,
    n_obs = current$n_obs,
    eligible = eligible,
    excess_return = alpha,
    stringsAsFactors = FALSE
  )
}

#' Compare current portfolio weights with suggested positioning
#'
#' @param tilts Output of [ek_positioning_tilts()].
#' @param current_weights Named numeric vector of current portfolio weights.
#' @return A data frame with current and suggested weights and the gap
#'   (`suggested_weight - current_weight`).
#' @export
#' @examples
#' pb <- ek_positioning_playbook()
#' b <- c(global_equity = 0.4, government_bonds = 0.25,
#'        investment_grade_credit = 0.1, high_yield_credit = 0.05,
#'        commodities = 0.08, gold = 0.05, us_dollar = 0.02, cash = 0.05)
#' t <- ek_positioning_tilts(pb, "Recovery", b)
#' ek_positioning_gap(t, b)
ek_positioning_gap <- function(tilts, current_weights) {
  if (!is.numeric(current_weights) || is.null(names(current_weights)) ||
        anyDuplicated(names(current_weights)) || any(!is.finite(current_weights)) ||
        any(current_weights < 0) || abs(sum(current_weights) - 1) > 1e-8) {
    stop("`current_weights` must be named, non-negative and sum to one.",
         call. = FALSE)
  }
  if (!all(c("asset_class", "suggested_weight", "benchmark_weight") %in% names(tilts))) {
    stop("`tilts` must be the output of `ek_positioning_tilts()`.", call. = FALSE)
  }
  if (!setequal(tilts$asset_class, names(current_weights))) {
    stop("`current_weights` and `tilts` must contain the same asset classes.",
         call. = FALSE)
  }
  idx <- match(tilts$asset_class, names(current_weights))
  data.frame(
    asset_class = tilts$asset_class,
    benchmark_weight = tilts$benchmark_weight,
    suggested_weight = tilts$suggested_weight,
    current_weight = unname(current_weights[idx]),
    gap = tilts$suggested_weight - unname(current_weights[idx]),
    regime = tilts$regime,
    n_obs = tilts$n_obs,
    eligible = tilts$eligible,
    stringsAsFactors = FALSE
  )
}
