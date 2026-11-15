#' Apply registry transforms
#'
#' Converts series to the units used for analysis, using each series'
#' default `transform` in [ek_series_catalog()] (for example year-over-year
#' growth for payrolls and prices, levels for rates). Columns not in the
#' registry are left unchanged. Transforms are computed on each series'
#' non-missing observations, so quarterly series in a monthly frame are
#' differenced quarter to quarter.
#'
#' @param data A wide data frame with a `date` column, such as the output of
#'   [ek_get_series()].
#' @param transforms Optional named character vector overriding the registry,
#'   for example `c(PAYEMS = "diff")`.
#' @param freq Periods per year of `data` (12 or 4). `NULL` uses the
#'   provenance frequency, or infers it from the dates.
#' @return `data` with transformed columns. Provenance is kept and the
#'   transforms used are stored in the `transforms` attribute.
#' @export
#' @examples
#' x <- ek_apply_transforms(ek_get_series(c("PAYEMS", "UNRATE"), start = "2015-01-01"))
#' tail(x)
ek_apply_transforms <- function(data, transforms = NULL, freq = NULL) {
  ek_check_wide(data)
  freq <- ek_periods_per_year(data, freq)
  cat <- ek_series_catalog()
  ids <- setdiff(names(data), "date")
  used <- stats::setNames(cat$transform[match(ids, cat$id)], ids)
  used[is.na(used)] <- "level"
  if (!is.null(transforms)) {
    extra <- setdiff(names(transforms), ids)
    if (length(extra) > 0) {
      stop("`transforms` names not in data: ", paste(extra, collapse = ", "), call. = FALSE)
    }
    used[names(transforms)] <- transforms
  }
  for (id in ids) {
    obs <- !is.na(data[[id]])
    series_freq <- if (freq == 12L && ek_is_sparse(data$date[obs])) 4L else freq
    data[[id]][obs] <- ek_transform(data[[id]][obs], used[[id]], freq = series_freq)
  }
  attr(data, "transforms") <- used
  data
}

#' Taylor rule
#'
#' Prescribed policy rate from a Taylor (1993)-type rule using the
#' unemployment gap, converted to an output gap with an Okun coefficient:
#'
#' `rate = r_star + inflation + inflation_weight * (inflation - pi_star)
#'   + gap_weight * okun * (u_star - unemployment)`
#'
#' With `inertia` > 0 the rule is smoothed toward last month's actual policy
#' rate: `inertia * policy_rate[t-1] + (1 - inertia) * rate`.
#'
#' @section Model card:
#' **Purpose:** a benchmark for the policy stance, used to judge whether
#' policy is restrictive or accommodative relative to a simple rule.
#' **Assumptions:** `r_star`, `pi_star` and `u_star` are fixed inputs, not
#' estimates; the defaults are illustrative and should be set by the user
#' (for example from central-bank longer-run projections). Okun's law
#' converts the unemployment gap to an output gap. **Limitations:** rules
#' ignore balance-sheet policy, financial conditions and forward guidance,
#' and the prescribed rate is sensitive to the parameters. With the sample
#' data the results are illustrative only.
#'
#' @param data Wide data frame with the policy rate, inflation and
#'   unemployment columns. `NULL` loads them with [ek_get_series()].
#' @param inflation,unemployment,policy_rate Column names in `data`.
#'   `policy_rate` may be missing from `data` if `inertia` is 0.
#' @param inflation_transform Transform applied to the inflation column
#'   (see [ek_transform()]); use `"level"` if it is already a rate.
#' @param r_star Neutral real rate, percent.
#' @param pi_star Inflation target, percent.
#' @param u_star Natural rate of unemployment, percent.
#' @param inflation_weight,gap_weight Rule coefficients (Taylor 1993: 0.5
#'   and 0.5; the "balanced approach" uses a gap weight of 1).
#' @param okun Okun coefficient converting the unemployment gap into an
#'   output gap.
#' @param inertia Weight on the lagged policy rate, between 0 and 1.
#' @param elb Optional effective lower bound; prescribed rates are floored
#'   at this value.
#' @return A data frame with `date`, `inflation`, `unemployment_gap`
#'   (`unemployment - u_star`), `prescribed_rate`, `policy_rate` and
#'   `policy_gap` (actual minus prescribed). Parameters are stored in the
#'   `params` attribute.
#' @export
#' @examples
#' tr <- ek_taylor_rule(u_star = 4.2)
#' tail(tr, 3)
ek_taylor_rule <- function(data = NULL, inflation = "PCEPILFE", unemployment = "UNRATE",
                           policy_rate = "FEDFUNDS", inflation_transform = "yoy",
                           r_star = 1, pi_star = 2, u_star = 4.2, inflation_weight = 0.5,
                           gap_weight = 0.5, okun = 2, inertia = 0, elb = NULL) {
  if (is.null(data)) data <- ek_get_series(c(policy_rate, inflation, unemployment))
  ek_check_wide(data, c(inflation, unemployment))
  nums <- list(r_star = r_star, pi_star = pi_star, u_star = u_star,
               inflation_weight = inflation_weight, gap_weight = gap_weight, okun = okun,
               inertia = inertia)
  for (nm in names(nums)) {
    if (!is.numeric(nums[[nm]]) || length(nums[[nm]]) != 1 || !is.finite(nums[[nm]])) {
      stop("`", nm, "` must be a single finite number.", call. = FALSE)
    }
  }
  if (inertia < 0 || inertia >= 1) stop("`inertia` must be in [0, 1).", call. = FALSE)
  has_policy <- policy_rate %in% names(data)
  if (inertia > 0 && !has_policy) {
    stop("`inertia` > 0 needs the `", policy_rate, "` column.", call. = FALSE)
  }
  data <- data[order(data$date), , drop = FALSE]
  infl <- ek_transform(data[[inflation]], inflation_transform,
                       freq = ek_periods_per_year(data))
  u_gap <- data[[unemployment]] - u_star
  rate <- r_star + infl + inflation_weight * (infl - pi_star) - gap_weight * okun * u_gap
  actual <- if (has_policy) as.numeric(data[[policy_rate]]) else rep(NA_real_, nrow(data))
  if (inertia > 0) rate <- inertia * ek_lag(actual, 1) + (1 - inertia) * rate
  if (!is.null(elb)) rate <- pmax(rate, elb)
  out <- data.frame(date = data$date, inflation = infl, unemployment_gap = u_gap,
                    prescribed_rate = rate, policy_rate = actual,
                    policy_gap = actual - rate)
  attr(out, "params") <- c(nums, list(elb = elb, inflation = inflation,
                                      unemployment = unemployment, policy_rate = policy_rate))
  out
}

#' Economic data surprise index
#'
#' Measures whether recent data have come in above or below a naive
#' expectation. For each indicator (after its registry transform; see
#' [ek_apply_transforms()]) the surprise is the new value minus the
#' expectation formed with data up to the previous observation:
#' `"random_walk"` expects no change and `"trend"` expects the average
#' change over the trailing window. Each surprise is standardized by the
#' standard deviation of that indicator's previous `window` surprises, signed
#' so that positive means stronger activity (see `signs`), averaged across
#' the indicators reporting that month, and smoothed with an exponentially
#' weighted moving average with the given half-life.
#'
#' Every step uses only information available at the time, so the index can
#' be backtested without look-ahead (subject to data revisions; use
#' `ek_get_series(vintage = )` for real-time inputs).
#'
#' @section Model card:
#' **Purpose:** a timely gauge of data momentum relative to expectations for
#' macro and cross-asset positioning. **Assumptions:** expectations are
#' statistical, not survey consensus, so the index measures data momentum
#' rather than true consensus surprises (as in commercial indices).
#' **Limitations:** results depend on the indicator set, transforms and
#' window; release timing within the month is ignored.
#'
#' @param data Wide data frame of indicators. `NULL` loads payrolls,
#'   industrial production and unemployment with [ek_get_series()].
#' @param ids Indicator columns to use; `NULL` uses all columns except
#'   `date`.
#' @param signs Named numeric vector of `1` or `-1` per indicator; indicators
#'   not listed get `1`. Default treats higher unemployment as negative.
#' @param expectation `"random_walk"` or `"trend"`.
#' @param window Trailing observations used to standardize surprises (and to
#'   estimate the trend).
#' @param min_obs Minimum past surprises needed before an indicator
#'   contributes.
#' @param half_life Half-life of the smoothing, in periods; `0` disables
#'   smoothing.
#' @param transform Apply registry transforms first.
#' @return A data frame with `date`, `surprise_index`, `n_indicators` and one
#'   column of standardized, signed surprises per indicator.
#' @export
#' @examples
#' s <- ek_surprise_index()
#' tail(s[, c("date", "surprise_index", "n_indicators")])
ek_surprise_index <- function(data = NULL, ids = NULL, signs = c(UNRATE = -1),
                              expectation = c("random_walk", "trend"), window = 36L,
                              min_obs = 12L, half_life = 3, transform = TRUE) {
  expectation <- match.arg(expectation)
  if (is.null(data)) data <- ek_get_series(c("PAYEMS", "INDPRO", "UNRATE"))
  if (is.null(ids)) ids <- setdiff(names(data), "date")
  ek_check_wide(data, ids)
  if (window < 2 || min_obs < 2 || min_obs > window) {
    stop("Need 2 <= min_obs <= window.", call. = FALSE)
  }
  if (!is.numeric(half_life) || length(half_life) != 1 || half_life < 0) {
    stop("`half_life` must be a non-negative number.", call. = FALSE)
  }
  if (!is.null(signs) && (is.null(names(signs)) || !all(signs %in% c(-1, 1)))) {
    stop("`signs` must be a named vector of 1 or -1.", call. = FALSE)
  }
  data <- data[order(data$date), c("date", ids), drop = FALSE]
  if (transform) data <- ek_apply_transforms(data)

  z <- vapply(ids, function(id) {
    sign <- if (id %in% names(signs)) signs[[id]] else 1
    out <- rep(NA_real_, nrow(data))
    obs <- which(!is.na(data[[id]]))
    out[obs] <- sign * ek_standardized_surprise(data[[id]][obs], expectation, window, min_obs)
    out
  }, numeric(nrow(data)))
  z <- matrix(z, nrow = nrow(data), dimnames = list(NULL, ids))
  n <- rowSums(!is.na(z))
  avg <- ifelse(n > 0, rowSums(z, na.rm = TRUE) / pmax(n, 1), NA_real_)
  index <- if (half_life > 0) ek_ewma(avg, 0.5^(1 / half_life)) else avg

  out <- data.frame(date = data$date, surprise_index = index, n_indicators = as.integer(n))
  cbind(out, as.data.frame(z))
}

ek_standardized_surprise <- function(x, expectation, window, min_obs) {
  n <- length(x)
  change <- c(NA_real_, diff(x))
  expected_change <- rep(0, n)
  if (expectation == "trend") {
    expected_change <- vapply(seq_len(n), function(t) {
      past <- change[max(1, t - window):(t - 1)]
      past <- past[!is.na(past)]
      if (t < 2 || length(past) < min_obs) NA_real_ else mean(past)
    }, numeric(1))
  }
  surprise <- change - expected_change
  vapply(seq_len(n), function(t) {
    if (t < 3) return(NA_real_)
    past <- surprise[max(1, t - window):(t - 1)]
    past <- past[!is.na(past)]
    if (length(past) < min_obs || is.na(surprise[t])) return(NA_real_)
    s <- stats::sd(past)
    if (!is.finite(s) || s == 0) NA_real_ else surprise[t] / s
  }, numeric(1))
}

# EWMA that starts at the first observation and carries the index through
# periods with no new information.
ek_ewma <- function(x, decay) {
  out <- rep(NA_real_, length(x))
  level <- NA_real_
  for (t in seq_along(x)) {
    if (!is.na(x[t])) level <- if (is.na(level)) x[t] else decay * level + (1 - decay) * x[t]
    out[t] <- level
  }
  out
}

ek_check_wide <- function(data, cols = NULL) {
  if (!is.data.frame(data) || !"date" %in% names(data) || !inherits(data$date, "Date")) {
    stop("`data` must be a data frame with a Date `date` column.", call. = FALSE)
  }
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop("Column(s) not in `data`: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

ek_periods_per_year <- function(data, freq = NULL) {
  if (!is.null(freq)) {
    if (!freq %in% c(4, 12)) stop("`freq` must be 4 or 12.", call. = FALSE)
    return(as.integer(freq))
  }
  prov_freq <- attr(data, "provenance")$frequency
  if (!is.null(prov_freq)) return(if (prov_freq == "quarter") 4L else 12L)
  if (ek_is_sparse(data$date)) 4L else 12L
}

ek_is_sparse <- function(dates) {
  length(dates) > 1 && stats::median(diff(sort(as.numeric(dates)))) > 60
}
