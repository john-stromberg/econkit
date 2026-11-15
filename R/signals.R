ek_signal_columns <- c(
  "as_of", "data_vintage", "signal", "value", "label", "direction",
  "percentile", "confidence", "n_obs", "model_version", "git_sha",
  "change_vs_prior", "override", "override_reason", "user"
)

#' Build a point-in-time macro signal snapshot
#'
#' Creates a long-form signal snapshot for a positioning or investment
#' committee workflow. Recession probability is estimated using only
#' recession labels that would have been available by `as_of` (including
#' `label_lag` months for delayed NBER dating). Numeric indicator percentiles
#' use observations up to the snapshot date.
#'
#' `data_vintage` records the latest observation date in the supplied data.
#' It does **not** make revised FRED history point-in-time; a true vintage
#' backtest requires ALFRED vintages or another archived source.
#'
#' @param data Monthly macro data with `date`, `USREC`, `T10Y3M`, `UNRATE`,
#'   `NFCI`, `INDPRO` and `CPIAUCSL`.
#' @param as_of Snapshot date. Defaults to the latest observation date.
#' @param horizon Recession probability horizon in months.
#' @param label_lag Delay in months before recession outcomes are considered
#'   known to the model.
#' @param min_train Minimum number of labelled observations for the recession
#'   model.
#' @param model_version Character model version recorded in each row.
#' @param git_sha Source revision recorded in each row; defaults to
#'   `GITHUB_SHA` when set.
#' @return A data frame with one row per signal and the fields required by
#'   [ek_store_signals()].
#' @export
#' @examples
#' signals <- ek_build_signals()
#' signals[, c("signal", "value", "label", "direction")]
ek_build_signals <- function(data = NULL, as_of = NULL, horizon = 12L,
                             label_lag = 6L, min_train = 60L,
                             model_version = "0.1.0",
                             git_sha = Sys.getenv("GITHUB_SHA", NA_character_)) {
  if (is.null(data)) data <- ek_load_dataset("macro_monthly")
  required <- c("date", "USREC", "T10Y3M", "UNRATE", "NFCI", "INDPRO", "CPIAUCSL")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("`data` is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!inherits(data$date, "Date") || anyDuplicated(data$date) ||
        is.unsorted(data$date, strictly = TRUE)) {
    stop("`data$date` must contain unique, strictly increasing Date values.",
         call. = FALSE)
  }
  if (is.null(as_of)) as_of <- max(data$date)
  as_of <- as.Date(as_of)
  if (length(as_of) != 1L || is.na(as_of)) {
    stop("`as_of` must be one valid date.", call. = FALSE)
  }
  idx <- which(data$date <= as_of)
  if (!length(idx)) stop("No data are available on or before `as_of`.", call. = FALSE)
  i <- max(idx)
  d <- data[seq_len(i), , drop = FALSE]
  if (horizon < 1 || label_lag < 0 || min_train < 2) {
    stop("`horizon`, `label_lag` or `min_train` is outside its valid range.",
         call. = FALSE)
  }

  obs <- function(name) {
    x <- d[[name]]
    good <- which(is.finite(x))
    if (!length(good)) return(list(value = NA_real_, history = numeric()))
    list(value = x[utils::tail(good, 1)], history = x[good])
  }
  series <- stats::setNames(lapply(c("T10Y3M", "UNRATE", "NFCI"), obs),
                            c("T10Y3M", "UNRATE", "NFCI"))
  sahm <- ek_sahm_rule(d$UNRATE)
  sahm_good <- which(is.finite(sahm))
  sahm_obs <- if (length(sahm_good)) {
    list(value = sahm[utils::tail(sahm_good, 1)], history = sahm[sahm_good])
  } else {
    list(value = NA_real_, history = numeric())
  }

  regime <- ek_growth_inflation_regime(
    ek_transform(d$INDPRO, "yoy"),
    ek_transform(d$CPIAUCSL, "yoy")
  )
  regime_value <- if (is.na(regime[i])) NA_character_ else as.character(regime[i])

  design <- ek_recession_design(d, "T10Y3M", "USREC", horizon, "at")
  last_label <- i - horizon - label_lag
  if (last_label < min_train) {
    stop(
      "Not enough recession labels are available as of ", format(d$date[i]),
      "; need at least ", min_train, " training observations after horizon and label lag.",
      call. = FALSE
    )
  }
  train <- design[seq_len(last_label), , drop = FALSE]
  train <- train[stats::complete.cases(train), , drop = FALSE]
  if (nrow(train) < min_train || length(unique(train$y)) < 2) {
    stop("Available recession training data need more observations or both classes.",
         call. = FALSE)
  }
  fit <- stats::glm(y ~ T10Y3M, family = stats::binomial(link = "probit"), data = train)
  current_probability <- stats::predict(
    fit, newdata = design[i, , drop = FALSE], type = "response"
  )

  make_row <- function(signal, value, label, direction, percentile, n_obs) {
    data.frame(
      as_of = as_of,
      data_vintage = d$date[i],
      signal = signal,
      value = as.numeric(value),
      label = if (is.na(label)) NA_character_ else as.character(label),
      direction = direction,
      percentile = as.numeric(percentile),
      confidence = NA_real_,
      n_obs = as.integer(n_obs),
      model_version = as.character(model_version),
      git_sha = if (length(git_sha) && !is.na(git_sha) && nzchar(git_sha)) {
        as.character(git_sha)
      } else {
        NA_character_
      },
      change_vs_prior = NA_real_,
      override = NA_character_,
      override_reason = NA_character_,
      user = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  percentile <- function(x) {
    if (!length(x) || !is.finite(utils::tail(x, 1))) {
      NA_real_
    } else {
      mean(x <= utils::tail(x, 1))
    }
  }
  spread <- series$T10Y3M$value
  unrate <- series$UNRATE$value
  nfci <- series$NFCI$value
  rows <- list(
    make_row("recession_probability", current_probability,
             sprintf("%.0f%%", 100 * current_probability), "higher = more recession risk",
             NA_real_, nrow(train)),
    make_row("term_spread", spread,
             if (is.na(spread)) NA_character_ else if (spread < 0) "inverted" else "positive",
             "lower = more recession risk", percentile(series$T10Y3M$history),
             length(series$T10Y3M$history)),
    make_row("sahm_rule", sahm_obs$value,
             if (is.na(sahm_obs$value)) NA_character_ else
               if (sahm_obs$value >= 0.5) "triggered" else "not triggered",
             "higher = more recession risk", percentile(sahm_obs$history),
             length(sahm_obs$history)),
    make_row("unemployment_rate", unrate, "percent",
             "higher = weaker labour market", percentile(series$UNRATE$history),
             length(series$UNRATE$history)),
    make_row("financial_conditions", nfci,
             if (is.na(nfci)) NA_character_ else if (nfci > 0) "tighter than average" else
               "looser than average",
             "higher = tighter", percentile(series$NFCI$history),
             length(series$NFCI$history)),
    make_row("growth_inflation_regime", NA_real_, regime_value,
             "categorical; see label", NA_real_, sum(!is.na(regime)))
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("ek_signals", class(out))
  out
}

#' Append a signal snapshot to an immutable local store
#'
#' Stores signal history in a single RDS file. Re-publishing identical
#' `(as_of, signal, model_version)` keys is idempotent; attempting to change a
#' previously published key is an error. This first implementation is a
#' single-writer local store, not a concurrent multi-user database.
#'
#' @param signals A signal data frame from [ek_build_signals()].
#' @param path RDS path for the store.
#' @return The normalized store path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' signals <- ek_build_signals()
#' ek_store_signals(signals, \"output/signals.rds\")
#' }
ek_store_signals <- function(signals, path) {
  ek_validate_signals(signals)
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be one non-empty file path.", call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  existing <- if (file.exists(path)) readRDS(path) else signals[FALSE, , drop = FALSE]
  if (nrow(existing)) ek_validate_signals(existing)

  key <- function(x) paste(x$as_of, x$signal, x$model_version, sep = "\r")
  overlap <- match(key(signals), key(existing))
  if (any(!is.na(overlap))) {
    duplicate_rows <- which(!is.na(overlap))
    fields <- setdiff(ek_signal_columns, "change_vs_prior")
    identical_rows <- vapply(duplicate_rows, function(j) {
      old <- existing[overlap[j], fields, drop = FALSE]
      new <- signals[j, fields, drop = FALSE]
      isTRUE(all.equal(old, new, check.attributes = FALSE))
    }, logical(1))
    if (!all(identical_rows)) {
      stop("Published signal keys are immutable; a duplicate key has changed.",
           call. = FALSE)
    }
    signals <- signals[is.na(overlap), , drop = FALSE]
  }
  if (!nrow(signals)) return(invisible(path))

  for (i in seq_len(nrow(signals))) {
    prior <- rbind(existing, signals[seq_len(i - 1L), , drop = FALSE])
    prior <- prior[prior$signal == signals$signal[i] & prior$as_of < signals$as_of[i], ,
                   drop = FALSE]
    if (is.numeric(signals$value[i]) && is.finite(signals$value[i]) && nrow(prior)) {
      previous <- prior[which.max(prior$as_of), , drop = FALSE]
      if (is.finite(previous$value[1])) {
        signals$change_vs_prior[i] <- signals$value[i] - previous$value[1]
      }
    }
  }
  updated <- rbind(existing, signals)
  updated <- updated[order(updated$as_of, updated$signal, updated$model_version), ,
                     drop = FALSE]
  rownames(updated) <- NULL

  temp <- tempfile(pattern = ".signals-", tmpdir = dirname(path))
  on.exit(unlink(temp), add = TRUE)
  saveRDS(updated, temp, version = 3)
  backup <- tempfile(pattern = ".signals-backup-", tmpdir = dirname(path))
  had_store <- file.exists(path)
  if (had_store && !file.rename(path, backup)) {
    stop("Could not move the existing signal store aside for replacement.",
         call. = FALSE)
  }
  if (!file.rename(temp, path)) {
    if (had_store && !file.rename(backup, path)) {
      stop("Could not replace the signal store or restore its backup.",
           call. = FALSE)
    }
    stop("Could not write the updated signal store.", call. = FALSE)
  }
  if (had_store) unlink(backup)
  invisible(path)
}

#' Read the latest published signals as of a date
#'
#' @param path RDS path previously written by [ek_store_signals()].
#' @param as_of Optional maximum snapshot date. Defaults to the latest
#'   published snapshot.
#' @return One latest row per signal, with no row published after `as_of`.
#' @export
#' @examples
#' \dontrun{
#' ek_read_signals(\"output/signals.rds\", as_of = \"2025-01-31\")
#' }
ek_read_signals <- function(path, as_of = NULL) {
  if (length(path) != 1L || !file.exists(path)) {
    stop("Signal store does not exist: ", path, call. = FALSE)
  }
  stored <- readRDS(path)
  ek_validate_signals(stored)
  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
    if (length(as_of) != 1L || is.na(as_of)) {
      stop("`as_of` must be one valid date.", call. = FALSE)
    }
    stored <- stored[stored$as_of <= as_of, , drop = FALSE]
  }
  if (!nrow(stored)) return(stored)
  stored <- stored[order(stored$as_of, stored$signal), , drop = FALSE]
  latest <- !duplicated(stored$signal, fromLast = TRUE)
  out <- stored[latest, , drop = FALSE]
  rownames(out) <- NULL
  out
}

ek_validate_signals <- function(signals) {
  if (!is.data.frame(signals) || !all(ek_signal_columns %in% names(signals))) {
    stop("`signals` must be a data frame with the econkit signal schema.",
         call. = FALSE)
  }
  if (!inherits(signals$as_of, "Date") || !inherits(signals$data_vintage, "Date") ||
        !is.numeric(signals$value) || !is.numeric(signals$percentile) ||
        !is.numeric(signals$confidence) || !is.numeric(signals$change_vs_prior) ||
        !is.integer(signals$n_obs)) {
    stop("Signal dates, values, percentiles, confidence and observation counts have invalid types.",
         call. = FALSE)
  }
  if (anyNA(signals$as_of) || anyNA(signals$signal) || any(!nzchar(signals$signal)) ||
        anyNA(signals$model_version) || any(signals$n_obs < 0) ||
        any(signals$percentile < 0 | signals$percentile > 1, na.rm = TRUE) ||
        anyDuplicated(paste(signals$as_of, signals$signal, signals$model_version, sep = "\r"))) {
    stop("Signal rows contain invalid values or duplicate publication keys.",
         call. = FALSE)
  }
  invisible(signals)
}
