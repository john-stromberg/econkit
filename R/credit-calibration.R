#' Calibrate segment default-rate sensitivities to macro drivers
#'
#' Fits a satellite model ([ek_satellite_fit()]) for each portfolio segment,
#' regressing the log annual default hazard, `log(-log(1 - default_rate))`,
#' on macro drivers. On this complementary log-log scale a driver coefficient
#' is the log-hazard sensitivity per unit of the driver, so the `UNRATE`
#' coefficient can be used directly as `unemployment_beta` in
#' [ek_credit_scenario_stress()].
#'
#' @section Model card:
#' **Purpose:** ground the macro sensitivities used in credit stress and
#' lifetime loss estimates in observed segment default experience.
#' **Assumptions:** a linear, stable and contemporaneous relationship between
#' the log hazard and the drivers; default rates are annualized and
#' observed without censoring. **Limitations:** no lags, own dynamics,
#' portfolio-mix shifts or parameter uncertainty in downstream use;
#' coefficients are associations, not causal effects, and need enough
#' history to cover downturns. The bundled `credit_default_rates` history
#' is synthetic, so sample estimates are illustrative.
#'
#' @param history Long data frame with a Date `date`, character `segment` and
#'   numeric `default_rate` (annualized, in `[0, 1)`) column. Defaults to the
#'   synthetic [credit_default_rates] sample.
#' @param macro Wide data frame with a `date` column and the `drivers`, at the
#'   frequency of `history`. Defaults to `ek_get_series(drivers)` from the
#'   sample, at the frequency of `history`.
#' @param drivers Names of the macro drivers.
#' @return A list with `coefficients` (segment, term, estimate, std_error),
#'   `fit` (segment, r_squared, sigma, n_obs, start, end and mean default
#'   rate), `models` (the `ek_satellite` fits by segment), `drivers` and
#'   `synthetic`.
#' @export
#' @examples
#' sens <- ek_credit_pd_sensitivity()
#' sens$coefficients[sens$coefficients$term == "UNRATE", ]
#' sens$fit
ek_credit_pd_sensitivity <- function(history = NULL, macro = NULL, drivers = "UNRATE") {
  synthetic <- is.null(history)
  if (is.null(history)) history <- ek_load_dataset("credit_default_rates")
  ek_check_wide(history, c("segment", "default_rate"))
  if (!is.character(history$segment) || anyNA(history$segment) ||
        !is.numeric(history$default_rate)) {
    stop("`history` needs character `segment` and numeric `default_rate` columns.",
         call. = FALSE)
  }
  rates <- history$default_rate[!is.na(history$default_rate)]
  if (any(rates < 0 | rates >= 1)) {
    stop("`default_rate` values must be in [0, 1).", call. = FALSE)
  }
  if (anyDuplicated(history[c("date", "segment")])) {
    stop("`history` must have one row per date and segment.", call. = FALSE)
  }
  if (!is.character(drivers) || !length(drivers) || anyDuplicated(drivers) ||
        "log_hazard" %in% drivers) {
    stop("`drivers` must be distinct macro series names.", call. = FALSE)
  }
  if (is.null(macro)) {
    frequency <- if (ek_is_sparse(unique(history$date))) "quarter" else "month"
    macro <- ek_get_series(drivers, frequency = frequency)
    synthetic <- synthetic || isTRUE(ek_provenance(macro)$synthetic)
  }
  ek_check_wide(macro, drivers)
  if (anyDuplicated(macro$date)) stop("`macro` must have one row per date.", call. = FALSE)

  segments <- sort(unique(history$segment))
  fits <- lapply(segments, function(segment) {
    rows <- history[history$segment == segment, c("date", "default_rate"), drop = FALSE]
    data <- merge(rows, macro[c("date", drivers)], by = "date")
    data$log_hazard <- ifelse(data$default_rate > 0,
                              log(-log1p(-data$default_rate)), NA_real_)
    data <- data[stats::complete.cases(data[c("log_hazard", drivers)]), , drop = FALSE]
    if (!nrow(data)) {
      stop("No complete `history` and `macro` observations for segment `", segment, "`.",
           call. = FALSE)
    }
    model <- tryCatch(
      ek_satellite_fit(data[c("date", "log_hazard", drivers)], "log_hazard", drivers),
      error = function(error) {
        stop("Segment `", segment, "`: ", conditionMessage(error), call. = FALSE)
      }
    )
    list(
      model = model,
      coefficients = data.frame(
        segment = segment, term = names(model$coefficients),
        estimate = unname(model$coefficients), std_error = unname(model$std_error),
        stringsAsFactors = FALSE
      ),
      fit = data.frame(
        segment = segment, r_squared = model$r_squared, sigma = model$sigma,
        n_obs = model$n_obs, start = min(data$date), end = max(data$date),
        mean_default_rate = mean(data$default_rate), stringsAsFactors = FALSE
      )
    )
  })
  coefficients <- do.call(rbind, lapply(fits, `[[`, "coefficients"))
  fit <- do.call(rbind, lapply(fits, `[[`, "fit"))
  rownames(coefficients) <- NULL
  rownames(fit) <- NULL
  list(coefficients = coefficients, fit = fit,
       models = stats::setNames(lapply(fits, `[[`, "model"), segments),
       drivers = drivers, synthetic = synthetic)
}
