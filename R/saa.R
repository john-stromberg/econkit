#' Estimate strategic asset-allocation inputs from monthly returns
#'
#' Calculates annualized historical arithmetic returns, geometric returns,
#' volatility and a covariance matrix for use as strategic asset-allocation
#' inputs. These are descriptive estimates, not forward-looking capital-market
#' forecasts. The bundled returns are synthetic and unsuitable for investment
#' decisions.
#'
#' @param returns Monthly decimal asset returns with a `date` column and one
#'   numeric column per asset. Defaults to bundled synthetic returns.
#' @param as_of Latest date to include. Defaults to the latest return date.
#' @param lookback_years Number of years of monthly observations to use.
#' @param min_obs Minimum complete monthly observations required.
#' @return An `ek_saa_assumptions` list with an `assumptions` data frame,
#'   annualized `covariance` matrix, dates, lookback and synthetic-data flag.
#' @export
#' @examples
#' inputs <- ek_saa_assumptions()
#' inputs$assumptions
ek_saa_assumptions <- function(returns = NULL, as_of = NULL,
                               lookback_years = 10L, min_obs = 60L) {
  if (is.null(returns)) returns <- ek_load_dataset("asset_returns")
  if (!is.data.frame(returns) || !"date" %in% names(returns) ||
        !inherits(returns$date, "Date") || anyNA(returns$date) ||
        anyDuplicated(returns$date) || is.unsorted(returns$date, strictly = TRUE)) {
    stop("`returns` must have unique, strictly increasing Date values.", call. = FALSE)
  }
  assets <- setdiff(names(returns), "date")
  if (!length(assets) ||
        !all(vapply(returns[assets], is.numeric, logical(1)))) {
    stop("`returns` must contain at least one numeric asset return column.",
         call. = FALSE)
  }
  if (length(lookback_years) != 1L || !is.finite(lookback_years) ||
        lookback_years < 1 || lookback_years != as.integer(lookback_years) ||
        length(min_obs) != 1L || !is.finite(min_obs) || min_obs < 2 ||
        min_obs != as.integer(min_obs)) {
    stop("`lookback_years` and `min_obs` must be positive whole numbers.",
         call. = FALSE)
  }
  if (is.null(as_of)) as_of <- max(returns$date)
  as_of <- as.Date(as_of)
  if (length(as_of) != 1L || is.na(as_of)) {
    stop("`as_of` must be one valid date.", call. = FALSE)
  }

  available <- returns[returns$date <= as_of, , drop = FALSE]
  if (!nrow(available)) {
    stop("No returns are available on or before `as_of`.", call. = FALSE)
  }
  available <- utils::tail(available, as.integer(lookback_years) * 12L)
  complete <- stats::complete.cases(available[, assets, drop = FALSE])
  sample <- available[complete, , drop = FALSE]
  if (nrow(sample) < min_obs) {
    stop("Need at least ", as.integer(min_obs),
         " complete monthly observations; found ", nrow(sample), ".",
         call. = FALSE)
  }
  values <- as.matrix(sample[, assets, drop = FALSE])
  if (any(!is.finite(values)) || any(values <= -1)) {
    stop("Complete returns must be finite and greater than -100%.",
         call. = FALSE)
  }

  means <- colMeans(values) * 12
  vols <- apply(values, 2, stats::sd) * sqrt(12)
  geometric <- exp(colMeans(log1p(values)) * 12) - 1
  assumptions <- data.frame(
    asset_class = assets,
    n_obs = nrow(sample),
    annualized_arithmetic_return = unname(means),
    annualized_geometric_return = unname(geometric),
    annualized_volatility = unname(vols),
    stringsAsFactors = FALSE
  )
  covariance <- stats::cov(values) * 12
  dimnames(covariance) <- list(assets, assets)
  out <- list(
    assumptions = assumptions,
    covariance = covariance,
    as_of = max(sample$date),
    sample_start = min(sample$date),
    sample_end = max(sample$date),
    lookback_years = as.integer(lookback_years),
    synthetic = isTRUE(attr(returns, "synthetic"))
  )
  class(out) <- "ek_saa_assumptions"
  out
}

#' Summarize a strategic policy portfolio
#'
#' Applies supplied strategic weights to [ek_saa_assumptions()] and reports
#' the historical arithmetic return and covariance-based volatility of the
#' policy mix. It does not optimize weights or provide an allocation
#' recommendation.
#'
#' @param assumptions Output of [ek_saa_assumptions()].
#' @param weights Named, non-negative strategic weights summing to one.
#' @return A list with the policy weights and historical portfolio return and
#'   volatility estimates.
#' @export
#' @examples
#' inputs <- ek_saa_assumptions()
#' weights <- stats::setNames(rep(1 / nrow(inputs$assumptions),
#'                                nrow(inputs$assumptions)),
#'                            inputs$assumptions$asset_class)
#' ek_saa_portfolio(inputs, weights)
ek_saa_portfolio <- function(assumptions, weights) {
  if (!inherits(assumptions, "ek_saa_assumptions")) {
    stop("`assumptions` must be returned by `ek_saa_assumptions()`.",
         call. = FALSE)
  }
  assets <- assumptions$assumptions$asset_class
  weights <- ek_validate_saa_weights(weights, assets)
  expected_return <- sum(
    weights * assumptions$assumptions$annualized_arithmetic_return
  )
  volatility <- sqrt(drop(t(weights) %*% assumptions$covariance %*% weights))
  list(
    weights = data.frame(asset_class = assets, weight = unname(weights)),
    annualized_arithmetic_return = expected_return,
    annualized_volatility = volatility,
    as_of = assumptions$as_of,
    sample_start = assumptions$sample_start,
    sample_end = assumptions$sample_end,
    synthetic = assumptions$synthetic
  )
}

ek_validate_saa_weights <- function(weights, assets) {
  if (!is.numeric(weights) || is.null(names(weights)) ||
        anyNA(weights) || anyDuplicated(names(weights)) ||
        any(!is.finite(weights)) || any(weights < 0) ||
        abs(sum(weights) - 1) > 1e-8) {
    stop("`weights` must be named, non-negative and sum to one.",
         call. = FALSE)
  }
  if (!setequal(names(weights), assets)) {
    stop("`weights` must contain exactly the assets in the scenario or assumptions.",
         call. = FALSE)
  }
  weights[assets]
}

#' Apply scenario asset shocks to a strategic policy mix
#'
#' Computes weighted cumulative returns on starting policy weights, without
#' rebalancing during the scenario. The sample shocks are stylized one-year
#' stress inputs and do not estimate a forecast or likelihood.
#'
#' @param scenarios An `ek_scenario_library` object.
#' @param weights Named, non-negative strategic weights summing to one.
#' @return A list with scenario-level `summary`, asset-level `contributions`
#'   and the `synthetic` flag.
#' @export
#' @examples
#' scenarios <- ek_scenario_library()
#' weights <- stats::setNames(rep(1 / 8, 8),
#'                            scenarios$asset_returns$asset_class[1:8])
#' ek_saa_scenario_stress(scenarios, weights)
ek_saa_scenario_stress <- function(scenarios, weights) {
  ek_validate_scenario_library(scenarios) # nolint: object_usage_linter
  if (!nrow(scenarios$asset_returns)) {
    stop("SAA stress requires a scenario library with asset-return shocks.",
         call. = FALSE)
  }
  assets <- sort(unique(scenarios$asset_returns$asset_class))
  weights <- ek_validate_saa_weights(weights, assets)
  path_key <- paste(
    scenarios$paths$scenario_id, scenarios$paths$version, sep = "\r"
  )
  shock_key <- paste(
    scenarios$asset_returns$scenario_id,
    scenarios$asset_returns$version,
    sep = "\r"
  )
  scenario_keys <- unique(path_key)
  summary <- lapply(scenario_keys, function(key) {
    path <- scenarios$paths[path_key == key, , drop = FALSE]
    shocks <- scenarios$asset_returns[shock_key == key, , drop = FALSE]
    shocks <- shocks[match(names(weights), shocks$asset_class), , drop = FALSE]
    contribution <- as.numeric(weights) * shocks$scenario_return
    data.frame(
      scenario_id = path$scenario_id[1],
      version = path$version[1],
      as_of = path$as_of[1],
      data_vintage = path$data_vintage[1],
      horizon_months = nrow(path),
      portfolio_return = sum(contribution),
      synthetic = scenarios$synthetic,
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, summary)
  contributions <- do.call(rbind, lapply(scenario_keys, function(key) {
    path <- scenarios$paths[path_key == key, , drop = FALSE]
    shocks <- scenarios$asset_returns[shock_key == key, , drop = FALSE]
    shocks <- shocks[match(names(weights), shocks$asset_class), , drop = FALSE]
    data.frame(
      scenario_id = path$scenario_id[1],
      version = path$version[1],
      asset_class = names(weights),
      weight = unname(weights),
      scenario_return = shocks$scenario_return,
      contribution = unname(weights) * shocks$scenario_return,
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary) <- NULL
  rownames(contributions) <- NULL
  list(
    summary = summary,
    contributions = contributions,
    synthetic = scenarios$synthetic
  )
}
