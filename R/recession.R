#' Yield-curve recession probability model
#'
#' Fits a probit (default) or logit model of a future recession on current
#' predictors, typically the 10-year minus 3-month Treasury spread (Estrella
#' and Mishkin, 1998). This is the same family of model that the New York Fed
#' publishes.
#'
#' @section Model card:
#' * **Purpose:** probability that the economy is in recession `horizon`
#'   months ahead (`target = "at"`), or at any point within the next `horizon`
#'   months (`target = "within"`). It feeds tactical asset allocation, ALCO
#'   briefings and scenario weighting.
#' * **Inputs:** a recession indicator (NBER-based `USREC`) and one or more
#'   predictors observed at the forecast origin.
#' * **Assumptions:** a stable relationship between the predictors and future
#'   recessions, with a monotone link.
#' * **Limitations:** few recession events, so estimates are uncertain. The
#'   relationship can break down, for example with QE-compressed term premia.
#'   NBER dates are announced with a lag, so use `label_lag` in
#'   [ek_backtest_recession()] for realistic evaluation. It does not give
#'   timing precision within the horizon.
#' * **Monitoring:** out-of-sample AUC, Brier score and calibration from
#'   [ek_backtest_recession()]. Re-estimate when AUC degrades materially.
#'
#' @param data A data frame with a `date` column, the recession column and
#'   the predictor columns (monthly frequency).
#' @param predictors Character vector of predictor columns.
#' @param recession Name of the 0/1 recession indicator column.
#' @param horizon Forecast horizon in months.
#' @param target `"at"` (recession in month t + horizon) or `"within"` (any
#'   recession in months t + 1 to t + horizon).
#' @param link `"probit"` or `"logit"`.
#'
#' @return An object of class `ek_recession_model` containing the fitted
#'   `glm` (`fit`), the specification, and in-sample metrics (`metrics`).
#' @references Estrella, A. and Mishkin, F. S. (1998). Predicting U.S.
#'   recessions: financial variables as leading indicators. *Review of
#'   Economics and Statistics*, 80(1), 45-61.
#' @export
#' @examples
#' d <- ek_get_series()
#' m <- ek_recession_model(d, horizon = 12)
#' m
#' tail(predict(m))
ek_recession_model <- function(data, predictors = "T10Y3M", recession = "USREC",
                               horizon = 12L, target = c("at", "within"),
                               link = c("probit", "logit")) {
  target <- match.arg(target)
  link <- match.arg(link)
  design <- ek_recession_design(data, predictors, recession, horizon, target)
  train <- design[stats::complete.cases(design), , drop = FALSE]
  if (length(unique(train$y)) < 2) {
    stop("Training data must contain both recession and expansion outcomes.", call. = FALSE)
  }
  fit <- stats::glm(
    stats::reformulate(predictors, "y"),
    family = stats::binomial(link = link),
    data = train
  )
  structure(
    list(
      fit = fit,
      predictors = predictors,
      recession = recession,
      horizon = as.integer(horizon),
      target = target,
      link = link,
      train_period = range(train$date),
      metrics = ek_classification_metrics(stats::fitted(fit), train$y)
    ),
    class = "ek_recession_model"
  )
}

#' @export
print.ek_recession_model <- function(x, ...) {
  cat("<ek_recession_model>", x$link, "| horizon:", x$horizon, "months |",
      "target:", x$target, "\n")
  cat("Train period:", format(x$train_period[1]), "to", format(x$train_period[2]), "\n")
  print(round(stats::coef(summary(x$fit)), 4))
  cat("In-sample AUC:", round(x$metrics$auc, 3), "| Brier:", round(x$metrics$brier, 4), "\n")
  invisible(x)
}

#' Predict recession probabilities
#'
#' @param object An `ek_recession_model`.
#' @param newdata Data frame with `date` and predictor columns. Defaults to
#'   the training data.
#' @param ... Unused.
#' @return A data frame with `date` (forecast origin), `target_date` and
#'   `probability`.
#' @export
predict.ek_recession_model <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) newdata <- object$fit$data
  missing <- setdiff(c("date", object$predictors), names(newdata))
  if (length(missing) > 0) {
    stop("`newdata` is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  prob <- rep(NA_real_, nrow(newdata))
  ok <- stats::complete.cases(newdata[object$predictors])
  prob[ok] <- stats::predict(object$fit, newdata = newdata[ok, , drop = FALSE], type = "response")
  data.frame(
    date = newdata$date,
    target_date = ek_add_months(newdata$date, object$horizon),
    probability = prob
  )
}

#' Pseudo out-of-sample backtest of the recession model
#'
#' Re-estimates the model on an expanding window at each forecast origin.
#' Only outcomes that would have been known at that origin are used. A row
#' with origin `j` has a label known at `j + horizon + label_lag`, so the fit
#' at origin `i` uses rows `j <= i - horizon - label_lag`.
#'
#' @inheritParams ek_recession_model
#' @param min_train Minimum number of labelled observations before the first
#'   forecast.
#' @param label_lag Additional months before a recession outcome is known.
#'   NBER announcements typically lag by 6 to 12 months.
#'
#' @return A data frame with `date`, `target_date`, `probability` and
#'   `outcome`. The attribute `metrics` holds [ek_classification_metrics()]
#'   computed on the forecasts.
#' @export
#' @examples
#' d <- ek_get_series()
#' bt <- ek_backtest_recession(d, horizon = 12, label_lag = 6)
#' attr(bt, "metrics")
ek_backtest_recession <- function(data, predictors = "T10Y3M", recession = "USREC",
                                  horizon = 12L, target = c("at", "within"),
                                  link = c("probit", "logit"), min_train = 120L,
                                  label_lag = 0L) {
  target <- match.arg(target)
  link <- match.arg(link)
  design <- ek_recession_design(data, predictors, recession, horizon, target)
  form <- stats::reformulate(predictors, "y")
  family <- stats::binomial(link = link)
  n <- nrow(design)
  prob <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    last <- i - horizon - label_lag
    if (last < min_train) next
    if (anyNA(design[i, predictors])) next
    train <- design[seq_len(last), , drop = FALSE]
    train <- train[stats::complete.cases(train), , drop = FALSE]
    if (nrow(train) < min_train || length(unique(train$y)) < 2) next
    fit <- suppressWarnings(stats::glm(form, family = family, data = train))
    prob[i] <- stats::predict(fit, newdata = design[i, , drop = FALSE], type = "response")
  }
  out <- data.frame(
    date = design$date,
    target_date = design$target_date,
    probability = prob,
    outcome = design$y
  )
  out <- out[!is.na(out$probability), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "metrics") <- ek_classification_metrics(out$probability, out$outcome)
  out
}

ek_recession_design <- function(data, predictors, recession, horizon, target) {
  need <- c("date", recession, predictors)
  missing <- setdiff(need, names(data))
  if (length(missing) > 0) {
    stop("`data` is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (horizon < 1) stop("`horizon` must be at least 1.", call. = FALSE)
  rec <- data[[recession]]
  n <- length(rec)
  y <- if (target == "at") {
    ek_lag(rec, -horizon)
  } else {
    vapply(seq_len(n), function(t) {
      if (t + horizon > n) return(NA_real_)
      max(rec[(t + 1):(t + horizon)])
    }, numeric(1))
  }
  data.frame(
    date = data$date,
    target_date = ek_add_months(data$date, horizon),
    y = y,
    data[predictors]
  )
}

#' Classification metrics for probability forecasts
#'
#' @param prob Predicted probabilities.
#' @param outcome Observed 0/1 outcomes.
#' @return A one-row data frame with `n`, `base_rate`, `auc` (area under the
#'   ROC curve), `brier` (Brier score) and `log_loss`.
#' @export
#' @examples
#' ek_classification_metrics(c(0.1, 0.8, 0.3, 0.9), c(0, 1, 0, 1))
ek_classification_metrics <- function(prob, outcome) {
  ok <- !is.na(prob) & !is.na(outcome)
  p <- prob[ok]
  y <- outcome[ok]
  n_pos <- sum(y == 1)
  n_neg <- sum(y == 0)
  auc <- if (n_pos > 0 && n_neg > 0) {
    r <- rank(p)
    (sum(r[y == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  } else {
    NA_real_
  }
  pc <- pmin(pmax(p, 1e-12), 1 - 1e-12)
  data.frame(
    n = length(y),
    base_rate = mean(y),
    auc = auc,
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(pc) + (1 - y) * log(1 - pc))
  )
}
