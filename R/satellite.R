#' Fit a satellite model linking a variable to macro drivers
#'
#' Estimates an OLS regression of a business variable (a delinquency or
#' charge-off rate, a credit spread, deposit growth and so on) on macro
#' drivers that appear in scenario paths, optionally with its own lag
#' (`ar = TRUE`). Use [ek_satellite_project()] to project it along scenario
#' paths.
#'
#' @section Model card:
#' **Purpose:** translate macro scenarios into the variables that drive
#' losses, revenues and balance sheets. **Assumptions:** linear and stable
#' relationship, estimated on rows where every variable is observed; the lag
#' is the previous observed period. **Limitations:** no error-correction,
#' nonlinearity or uncertainty in the projection; coefficients are not
#' causal. Fit at the frequency of use: monthly fits project monthly paths,
#' quarterly fits project quarterly averages of the paths. Results on the
#' sample data are illustrative.
#'
#' @param data Wide data frame with a `date` column, at monthly or quarterly
#'   frequency.
#' @param y Name of the dependent variable.
#' @param x Names of the macro drivers.
#' @param ar Include the lagged dependent variable.
#' @return An object of class `ek_satellite` with `coefficients`, `std_error`,
#'   `r_squared`, `sigma`, `n_obs`, the last observed `y` and date, and the
#'   data frequency.
#' @export
#' @examples
#' data <- ek_get_series(c("DGS10", "UNRATE", "NFCI"))
#' fit <- ek_satellite_fit(data, "DGS10", c("UNRATE", "NFCI"), ar = TRUE)
#' fit
ek_satellite_fit <- function(data, y, x, ar = FALSE) {
  if (!is.character(y) || length(y) != 1L || !is.character(x) || !length(x) ||
        y %in% x || anyDuplicated(x)) {
    stop("`y` must be one variable name and `x` distinct driver names.", call. = FALSE)
  }
  if (!is.logical(ar) || length(ar) != 1L || is.na(ar)) {
    stop("`ar` must be TRUE or FALSE.", call. = FALSE)
  }
  ek_check_wide(data, c(y, x))
  freq <- ek_periods_per_year(data)
  data <- data[order(data$date), , drop = FALSE]
  observed <- data[stats::complete.cases(data[c(y, x)]), c("date", y, x), drop = FALSE]
  if (!all(vapply(observed[c(y, x)], is.numeric, logical(1)))) {
    stop("`y` and `x` must be numeric.", call. = FALSE)
  }
  design <- cbind(const = 1, as.matrix(observed[x]))
  response <- observed[[y]]
  if (ar) {
    design <- cbind(design, lag = c(NA, response[-length(response)]))[-1, , drop = FALSE]
    colnames(design)[ncol(design)] <- paste0(y, "_l1")
    response <- response[-1]
  }
  if (length(response) < ncol(design) + 5L) {
    stop("Too few complete observations for the satellite model.", call. = FALSE)
  }
  fit <- stats::lm.fit(design, response)
  if (fit$rank < ncol(design)) {
    stop("Satellite drivers are collinear.", call. = FALSE)
  }
  df <- length(response) - ncol(design)
  sigma <- sqrt(sum(fit$residuals^2) / df)
  r <- qr.R(fit$qr)
  se <- sigma * sqrt(diag(chol2inv(r)))
  coefficients <- stats::setNames(fit$coefficients, colnames(design))
  structure(list(
    y = y, x = x, ar = ar, coefficients = coefficients,
    std_error = stats::setNames(se[order(fit$qr$pivot)], colnames(design)),
    r_squared = 1 - sum(fit$residuals^2) / sum((response - mean(response))^2),
    sigma = sigma, n_obs = length(response), freq = freq,
    last_y = observed[[y]][nrow(observed)], last_date = observed$date[nrow(observed)]
  ), class = "ek_satellite")
}

#' @export
print.ek_satellite <- function(x, ...) {
  cat(sprintf("<ek_satellite> %s on %s%s, %s, %d observations, R2 = %.2f\n",
              x$y, paste(x$x, collapse = ", "), if (x$ar) " + own lag" else "",
              if (x$freq == 4L) "quarterly" else "monthly", x$n_obs, x$r_squared))
  print(round(cbind(estimate = x$coefficients, std_error = x$std_error), 4))
  invisible(x)
}

#' Project a satellite model along scenario paths
#'
#' Applies an [ek_satellite_fit()] model to each scenario path. Monthly
#' models use the monthly path values; quarterly models use averages of
#' consecutive three-step blocks (steps 1-3, 4-6 and so on), dated at the
#' block's last month. With an own lag, the projection starts from `y0`.
#'
#' @param fit An `ek_satellite` object.
#' @param scenarios An `ek_scenario_library` or a data frame of paths with
#'   `scenario_id`, `version`, `step`, `date` and the model drivers.
#' @param y0 Starting value of the dependent variable for models with an own
#'   lag. Defaults to the last observed value in the fit data.
#' @return A data frame with `scenario_id`, `version`, `step`, `date` and a
#'   column named after the dependent variable. Quarterly projections number
#'   steps by quarter.
#' @export
#' @examples
#' data <- ek_get_series(c("DGS10", "UNRATE", "NFCI"))
#' fit <- ek_satellite_fit(data, "DGS10", c("UNRATE", "NFCI"))
#' head(ek_satellite_project(fit, ek_scenario_library()))
ek_satellite_project <- function(fit, scenarios, y0 = NULL) {
  if (!inherits(fit, "ek_satellite")) stop("`fit` must be an ek_satellite model.", call. = FALSE)
  if (inherits(scenarios, "ek_scenario_library")) {
    ek_validate_scenario_library(scenarios)
    scenarios <- scenarios$paths
  }
  needed <- c("scenario_id", "version", "step", "date", fit$x)
  if (!is.data.frame(scenarios) || !all(needed %in% names(scenarios))) {
    stop("Scenario paths need columns: ", paste(needed, collapse = ", "), call. = FALSE)
  }
  if (is.null(y0)) y0 <- fit$last_y
  if (!is.numeric(y0) || length(y0) != 1L || !is.finite(y0)) {
    stop("`y0` must be one finite number.", call. = FALSE)
  }
  key <- paste(scenarios$scenario_id, scenarios$version, sep = "\r")
  out <- lapply(unique(key), function(k) {
    path <- scenarios[key == k, , drop = FALSE]
    path <- path[order(path$step), , drop = FALSE]
    drivers <- as.matrix(path[fit$x])
    dates <- path$date
    if (fit$freq == 4L) {
      if (nrow(path) %% 3L != 0L) {
        stop("Quarterly satellites need paths with a multiple of three monthly steps.",
             call. = FALSE)
      }
      block <- rep(seq_len(nrow(path) / 3L), each = 3L)
      drivers <- rowsum(drivers, block) / 3
      dates <- dates[seq(3L, nrow(path), by = 3L)]
    }
    level <- drop(cbind(1, drivers) %*% fit$coefficients[c("const", fit$x)])
    if (fit$ar) {
      rho <- fit$coefficients[[length(fit$coefficients)]]
      prev <- y0
      for (i in seq_along(level)) {
        level[i] <- level[i] + rho * prev
        prev <- level[i]
      }
    }
    res <- data.frame(scenario_id = path$scenario_id[1], version = path$version[1],
                      step = seq_along(level), date = dates, stringsAsFactors = FALSE)
    res[[fit$y]] <- unname(level)
    res
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}
