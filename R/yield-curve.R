#' Fit Nelson-Siegel yield curves
#'
#' Fits a cross-sectional Nelson-Siegel curve to constant-maturity Treasury
#' yields for each date. The bundled curve data are synthetic. FRED yields
#' are par yields, not zero-coupon discount rates; these descriptive fits are
#' not suitable for pricing or valuation.
#'
#' @section Model card:
#' **Purpose:** summarize the level, slope and curvature of a government yield
#' curve. **Assumptions:** a fixed-decay Nelson-Siegel form is adequate for the
#' selected maturities; each date is fit independently by ordinary least
#' squares. **Limitations:** no arbitrage constraints, uncertainty estimates,
#' or fitted discount factors; the factors are descriptive, not forecasts.
#'
#' @param data Wide data frame with `date` and one percent-yield column for
#'   each name in `tenors`. Defaults to the bundled synthetic curve.
#' @param tenors Named numeric vector of maturities in years. Names identify
#'   the corresponding columns in `data`; at least three distinct maturities
#'   are required.
#' @param lambda Positive fixed Nelson-Siegel decay parameter.
#' @return An `ek_yield_curve` object with date-level `factors` and long-format
#'   observed and fitted yields, along with tenors, lambda, source metadata
#'   and model version.
#' @export
#' @examples
#' curve <- ek_yield_curve_fit()
#' head(curve$factors)
ek_yield_curve_fit <- function(
  data = NULL,
  tenors = c(DGS3MO = 0.25, DGS2 = 2, DGS5 = 5, DGS10 = 10, DGS30 = 30),
  lambda = 0.6) {
  if (!is.numeric(tenors) || length(tenors) < 3L || is.null(names(tenors)) ||
        anyNA(names(tenors)) || any(!nzchar(names(tenors))) ||
        anyDuplicated(names(tenors)) || any(!is.finite(tenors)) ||
        any(tenors <= 0) || anyDuplicated(tenors)) {
    stop("`tenors` must be a named vector of at least three distinct positive maturities.",
         call. = FALSE)
  }
  if (!is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda <= 0) {
    stop("`lambda` must be one positive finite number.", call. = FALSE)
  }
  if (is.null(data)) data <- ek_get_series(names(tenors))
  ek_check_wide(data, names(tenors))
  if (anyNA(data$date) || anyDuplicated(data$date)) {
    stop("`data$date` must contain unique, non-missing dates.", call. = FALSE)
  }
  data <- data[order(data$date), , drop = FALSE]
  rates <- data[names(tenors)]
  if (!all(vapply(rates, is.numeric, logical(1))) ||
        any(vapply(rates, function(x) any(!is.na(x) & !is.finite(x)), logical(1)))) {
    stop("Yield columns must contain numeric finite values or NA.", call. = FALSE)
  }

  order_tenors <- order(tenors)
  tenors <- tenors[order_tenors]
  rates <- rates[names(tenors)]
  decay <- lambda * tenors
  slope_load <- -expm1(-decay) / decay
  curvature_load <- slope_load - exp(-decay)
  design <- cbind(level = 1, slope = slope_load, curvature = curvature_load)
  if (qr(design)$rank < 3L) {
    stop("`tenors` do not identify all three Nelson-Siegel factors.", call. = FALSE)
  }

  values <- as.matrix(rates)
  usable <- rowSums(is.finite(values)) >= 3L
  if (!any(usable)) {
    stop("No dates have at least three finite yields.", call. = FALSE)
  }
  dates <- data$date[usable]
  values <- values[usable, , drop = FALSE]
  factors <- matrix(NA_real_, nrow(values), 4L,
                    dimnames = list(NULL, c("level", "slope", "curvature", "rmse")))
  fitted <- matrix(NA_real_, nrow(values), length(tenors),
                   dimnames = list(NULL, names(tenors)))
  for (i in seq_len(nrow(values))) {
    observed <- is.finite(values[i, ])
    fit <- stats::lm.fit(design[observed, , drop = FALSE], values[i, observed])
    if (fit$rank < 3L) {
      stop("Yield maturities do not identify all three Nelson-Siegel factors.",
           call. = FALSE)
    }
    factors[i, 1:3] <- fit$coefficients
    fitted[i, ] <- drop(design %*% fit$coefficients)
    factors[i, "rmse"] <- sqrt(mean(fit$residuals^2))
  }
  factor_data <- data.frame(date = dates, factors, row.names = NULL)
  fitted_data <- data.frame(
    date = rep(dates, each = length(tenors)),
    tenor = rep(names(tenors), times = length(dates)),
    years = rep(unname(tenors), times = length(dates)),
    observed = as.vector(t(values)),
    fitted = as.vector(t(fitted)),
    stringsAsFactors = FALSE
  )
  provenance <- attr(data, "provenance")
  structure(list(
    factors = factor_data,
    fitted = fitted_data,
    tenors = tenors,
    lambda = lambda,
    source = if (is.null(provenance$source)) "provided" else provenance$source,
    synthetic = if (is.null(provenance$synthetic)) {
      isTRUE(attr(data, "synthetic"))
    } else {
      isTRUE(provenance$synthetic)
    },
    data_vintage = if (is.null(provenance$vintage)) NA_character_ else provenance$vintage,
    input_rows = nrow(data),
    dropped_rows = sum(!usable),
    model_version = "nelson-siegel-v1"
  ), class = "ek_yield_curve")
}

#' Fit macro satellites for yield-curve factors
#'
#' Fits separate [ek_satellite_fit()] models for the Nelson-Siegel level,
#' slope and curvature factors. By default, the curve and driver history are
#' fetched from the same econkit source and vintage.
#'
#' @param data Wide Treasury yield data accepted by [ek_yield_curve_fit()].
#' @param macro Wide macro driver data with a `date` column. If omitted, the
#'   same source and vintage as `data` are used; custom `data` therefore
#'   requires an explicit `macro` data frame.
#' @param drivers Names of macro drivers available in `macro` and scenario
#'   paths.
#' @param ar Include each factor's lag in its satellite model.
#' @param tenors Named maturities in years, as in [ek_yield_curve_fit()].
#' @param lambda Positive Nelson-Siegel decay parameter.
#' @return An `ek_yield_curve_satellites` object containing the factor models,
#'   tenors, drivers, source metadata and model version.
#' @export
#' @examples
#' fit <- ek_yield_curve_satellite_fit()
#' ek_yield_curve_project(fit, ek_scenario_library())
ek_yield_curve_satellite_fit <- function(
  data = NULL,
  macro = NULL,
  drivers = c("UNRATE", "NFCI"),
  ar = TRUE,
  tenors = c(DGS3MO = 0.25, DGS2 = 2, DGS5 = 5, DGS10 = 10, DGS30 = 30),
  lambda = 0.6) {
  if (!is.character(drivers) || !length(drivers) || anyNA(drivers) ||
        any(!nzchar(drivers)) || anyDuplicated(drivers)) {
    stop("`drivers` must be distinct, non-empty column names.", call. = FALSE)
  }
  if (any(drivers %in% c("date", "level", "slope", "curvature"))) {
    stop("`drivers` cannot use the date or Nelson-Siegel factor names.", call. = FALSE)
  }
  if (!is.logical(ar) || length(ar) != 1L || is.na(ar)) {
    stop("`ar` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(data)) data <- ek_get_series(names(tenors))
  curve <- ek_yield_curve_fit(data, tenors = tenors, lambda = lambda)
  if (is.null(macro)) {
    provenance <- attr(data, "provenance")
    if (is.null(provenance$source)) {
      stop("Supply `macro` when custom `data` has no econkit provenance.", call. = FALSE)
    }
    vintage <- provenance$vintage
    if (is.null(vintage) || is.na(vintage) || !nzchar(vintage)) vintage <- NULL
    macro <- ek_get_series(drivers, source = provenance$source, vintage = vintage)
  }
  ek_check_wide(macro, drivers)
  if (anyNA(macro$date) || anyDuplicated(macro$date)) {
    stop("`macro$date` must contain unique, non-missing dates.", call. = FALSE)
  }
  if (!all(vapply(macro[drivers], is.numeric, logical(1)))) {
    stop("Macro drivers must be numeric.", call. = FALSE)
  }
  if (any(vapply(macro[drivers], function(x) any(!is.na(x) & !is.finite(x)), logical(1)))) {
    stop("Macro drivers must contain finite values or NA.", call. = FALSE)
  }
  macro_provenance <- attr(macro, "provenance")
  macro <- macro[order(macro$date), c("date", drivers), drop = FALSE]
  factors <- curve$factors[c("date", "level", "slope", "curvature")]
  training <- merge(factors, macro, by = "date", all = FALSE)
  targets <- c("level", "slope", "curvature")
  models <- stats::setNames(lapply(targets, function(target) {
    ek_satellite_fit(training, target, drivers, ar = ar)
  }), targets)
  structure(list(
    models = models,
    tenors = curve$tenors,
    lambda = curve$lambda,
    drivers = drivers,
    ar = ar,
    source = curve$source,
    synthetic = curve$synthetic || isTRUE(macro_provenance$synthetic),
    data_vintage = curve$data_vintage,
    model_version = "curve-satellite-v1"
  ), class = "ek_yield_curve_satellites")
}

#' Project yield curves along macro scenarios
#'
#' Projects the fitted Nelson-Siegel factors with their macro satellite models
#' and reconstructs the fitted yield at each configured maturity. These are
#' scenario-implied par-yield paths, not discount curves or valuation inputs.
#'
#' @param fit An `ek_yield_curve_satellites` object.
#' @param scenarios An `ek_scenario_library` or a data frame of paths with the
#'   satellite drivers.
#' @return An `ek_yield_curve_projection` with factor paths and wide yield
#'   paths, each keyed by scenario, version, step and date.
#' @export
#' @examples
#' fit <- ek_yield_curve_satellite_fit()
#' projected <- ek_yield_curve_project(fit, ek_scenario_library())
#' head(projected$yields)
ek_yield_curve_project <- function(fit, scenarios) {
  if (!inherits(fit, "ek_yield_curve_satellites")) {
    stop("`fit` must be an ek_yield_curve_satellites object.", call. = FALSE)
  }
  targets <- c("level", "slope", "curvature")
  factor_paths <- lapply(targets, function(target) {
    ek_satellite_project(fit$models[[target]], scenarios)
  })
  keys <- c("scenario_id", "version", "step", "date")
  factors <- Reduce(function(x, y) merge(x, y, by = keys, sort = FALSE), factor_paths)
  factors <- factors[order(factors$scenario_id, factors$version, factors$step), ]
  decay <- fit$lambda * fit$tenors
  slope_load <- -expm1(-decay) / decay
  curvature_load <- slope_load - exp(-decay)
  design <- cbind(level = 1, slope = slope_load, curvature = curvature_load)
  yields <- as.matrix(factors[targets]) %*% t(design)
  colnames(yields) <- names(fit$tenors)
  yield_paths <- data.frame(factors[keys], yields, check.names = FALSE)
  structure(list(
    factors = factors,
    yields = yield_paths,
    tenors = fit$tenors,
    lambda = fit$lambda,
    model_version = fit$model_version,
    synthetic = fit$synthetic
  ), class = "ek_yield_curve_projection")
}
