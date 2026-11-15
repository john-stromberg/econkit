ek_catalog_fields <- c(
  "id", "title", "units", "frequency", "category", "transform", "source", "source_id",
  "aggregation", "seasonal_adjustment", "release_lag_days", "license", "in_sample"
)

#' Series catalog
#'
#' Returns the series registry, read from `inst/config/series.yml`. The
#' registry records each series' provider, native frequency, the rule used
#' to aggregate it to lower frequencies, its seasonal adjustment, a
#' conservative publication lag, licensing notes and whether it is part of
#' the bundled synthetic sample data.
#'
#' @return A data frame with one row per series and columns `id`, `title`,
#'   `units`, `frequency` (`daily`, `weekly`, `monthly` or `quarterly`),
#'   `category`, `transform`, `source`, `source_id`, `aggregation` (`mean`,
#'   `last`, `sum` or `max`), `seasonal_adjustment`, `release_lag_days`
#'   (integer days after the end of the native observation period),
#'   `license` and `in_sample` (logical).
#' @export
#' @examples
#' cat <- ek_series_catalog()
#' cat[cat$in_sample, c("id", "frequency", "aggregation")]
ek_series_catalog <- function() {
  path <- system.file("config", "series.yml", package = "econkit", mustWork = TRUE)
  ek_parse_catalog(yaml::read_yaml(path))
}

ek_parse_catalog <- function(cfg) {
  chr_fields <- setdiff(ek_catalog_fields, c("release_lag_days", "in_sample"))
  rows <- lapply(cfg$series, function(s) {
    row <- lapply(stats::setNames(chr_fields, chr_fields), function(f) {
      if (is.null(s[[f]])) NA_character_ else as.character(s[[f]])
    })
    row$release_lag_days <- if (is.null(s$release_lag_days)) {
      NA_integer_
    } else {
      as.integer(s$release_lag_days)
    }
    row$in_sample <- isTRUE(s$in_sample)
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)[ek_catalog_fields]
  ek_validate_catalog(out)
  out
}

ek_validate_catalog <- function(cat) {
  fail <- function(msg) stop("Invalid series registry: ", msg, call. = FALSE)
  if (anyDuplicated(cat$id)) fail(paste("duplicate id", cat$id[duplicated(cat$id)][1]))
  check_set <- function(field, allowed) {
    bad <- cat$id[is.na(cat[[field]]) | !cat[[field]] %in% allowed]
    if (length(bad) > 0) {
      fail(sprintf("`%s` must be one of %s (series %s)", field,
                   paste(allowed, collapse = ", "), paste(bad, collapse = ", ")))
    }
  }
  check_set("frequency", c("daily", "weekly", "monthly", "quarterly"))
  check_set("aggregation", c("mean", "last", "sum", "max"))
  check_set("source", c("fred"))
  check_set("transform", eval(formals(ek_transform)$method))
  bad_lag <- cat$id[is.na(cat$release_lag_days) | cat$release_lag_days < 0]
  if (length(bad_lag) > 0) {
    fail(paste("`release_lag_days` must be a non-negative integer (series",
               paste(bad_lag, collapse = ", "), ")"))
  }
  if (any(is.na(cat$source_id))) fail("every series needs a `source_id`")
  invisible(cat)
}

#' Get macro series
#'
#' Unified access to macro series as a wide data frame at monthly or
#' quarterly frequency. Use `source = "sample"` (default) for the bundled
#' synthetic [macro_monthly] dataset, which works offline. Use
#' `source = "fred"` for live FRED data, which requires the `fredr` package
#' and a `FRED_API_KEY` environment variable (put it in `.Renviron`; never
#' commit it). FRED downloads are cached on disk (see [ek_cache_dir()]).
#'
#' Each series is aggregated to the output frequency with the rule recorded
#' in [ek_series_catalog()] (for example `mean` for rates, `last` for
#' payrolls and `max` for the recession indicator). Quarterly series
#' requested at monthly frequency appear in the first month of each quarter,
#' with the other months `NA`. Quarterly values built from monthly data
#' require all three months; incomplete quarters are `NA`.
#'
#' @section Point-in-time data:
#' `vintage` returns the data as it would have been known on that date.
#' For `source = "fred"` this queries ALFRED real-time vintages, so it
#' reflects both publication timing and later revisions. For
#' `source = "sample"` the synthetic data has no vintages, so values are
#' masked when the end of the period, plus the series' `release_lag_days`,
#' falls after the vintage date. This release-lag approximation does not
#' model revisions; whole periods are dropped rather than partially
#' aggregated.
#'
#' @param ids Character vector of series ids from [ek_series_catalog()].
#'   `NULL` returns the series included in the sample data (`in_sample`).
#' @param source `"sample"` or `"fred"`.
#' @param start,end Optional start and end dates (inclusive), compared with
#'   the period start date.
#' @param refresh For `source = "fred"`, ignore the cache and re-download.
#' @param frequency Output frequency: `"month"` (default) or `"quarter"`.
#' @param vintage Optional as-of date for point-in-time data (see below).
#'
#' @return A data frame with a `date` column (period start) and one numeric
#'   column per series. The `provenance` attribute (see [ek_provenance()])
#'   records the source, vintage, method and series metadata; the `source`
#'   attribute is kept for backward compatibility.
#' @export
#' @examples
#' x <- ek_get_series(c("UNRATE", "T10Y3M"), start = "2000-01-01")
#' head(x)
#' q <- ek_get_series(c("UNRATE", "PAYEMS"), frequency = "quarter", vintage = "2020-06-30")
#' tail(q)
#' ek_provenance(q)$vintage_method
ek_get_series <- function(ids = NULL, source = c("sample", "fred"),
                          start = NULL, end = NULL, refresh = FALSE,
                          frequency = c("month", "quarter"), vintage = NULL) {
  source <- match.arg(source)
  frequency <- match.arg(frequency)
  vintage <- ek_as_vintage(vintage)
  cat <- ek_series_catalog()
  if (is.null(ids)) ids <- cat$id[cat$in_sample]
  ids <- unique(as.character(ids))
  unknown <- setdiff(ids, cat$id)
  if (length(unknown) > 0) {
    stop("Unknown series id(s): ", paste(unknown, collapse = ", "),
         ". See ek_series_catalog().", call. = FALSE)
  }
  meta <- cat[match(ids, cat$id), , drop = FALSE]
  rownames(meta) <- NULL

  out <- switch(source,
    sample = ek_get_sample_series(meta, frequency, vintage),
    fred = ek_get_fred(meta, frequency, vintage, refresh = refresh)
  )

  if (!is.null(start)) out <- out[out$date >= as.Date(start), , drop = FALSE]
  if (!is.null(end)) out <- out[out$date <= as.Date(end), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "source") <- source
  attr(out, "provenance") <- list(
    source = source,
    synthetic = identical(source, "sample"),
    frequency = frequency,
    vintage = if (is.null(vintage)) NA_character_ else format(vintage),
    vintage_method = if (is.null(vintage)) {
      "latest"
    } else if (source == "fred") {
      "alfred"
    } else {
      "release_lag_approximation"
    },
    retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    econkit_version = as.character(utils::packageVersion("econkit")),
    series = meta
  )
  out
}

#' Provenance of a series extract
#'
#' @param x A data frame returned by [ek_get_series()].
#' @return A list with `source`, `synthetic`, `frequency`, `vintage`,
#'   `vintage_method` (`"latest"`, `"alfred"` or
#'   `"release_lag_approximation"`), `retrieved_at` (UTC), `econkit_version`
#'   and `series` (the registry rows of the returned series).
#' @export
#' @examples
#' ek_provenance(ek_get_series("UNRATE"))$synthetic
ek_provenance <- function(x) {
  prov <- attr(x, "provenance", exact = TRUE)
  if (is.null(prov)) stop("`x` has no provenance; use ek_get_series().", call. = FALSE)
  prov
}

#' Convert a series extract to long format
#'
#' Reshapes the wide output of [ek_get_series()] into one row per date and
#' series, with metadata columns suitable for BI tools and data feeds.
#'
#' @param x A data frame returned by [ek_get_series()].
#' @param drop_na Drop rows with missing values.
#' @return A data frame with columns `date`, `series_id`, `value`, `units`,
#'   `frequency`, `source`, `vintage` and `vintage_method`.
#' @export
#' @examples
#' head(ek_series_long(ek_get_series(c("UNRATE", "FEDFUNDS"))))
ek_series_long <- function(x, drop_na = TRUE) {
  prov <- ek_provenance(x)
  ids <- setdiff(names(x), "date")
  rows <- lapply(ids, function(id) {
    data.frame(date = x$date, series_id = id, value = as.numeric(x[[id]]),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(date = as.Date(character()), series_id = character(),
                      value = numeric(), stringsAsFactors = FALSE)
  }
  if (drop_na) out <- out[!is.na(out$value), , drop = FALSE]
  out$units <- prov$series$units[match(out$series_id, prov$series$id)]
  out$frequency <- rep(prov$frequency, nrow(out))
  out$source <- rep(prov$source, nrow(out))
  out$vintage <- rep(prov$vintage, nrow(out))
  out$vintage_method <- rep(prov$vintage_method, nrow(out))
  out <- out[order(out$series_id, out$date), , drop = FALSE]
  rownames(out) <- NULL
  out
}

ek_as_vintage <- function(vintage) {
  if (is.null(vintage)) return(NULL)
  v <- tryCatch(as.Date(vintage), error = function(e) as.Date(NA))
  if (length(v) != 1 || is.na(v)) {
    stop("`vintage` must be a single date (YYYY-MM-DD).", call. = FALSE)
  }
  v
}

ek_aggregation_fun <- function(rule) {
  switch(rule,
    mean = mean,
    last = function(x) x[length(x)],
    sum = sum,
    max = max,
    stop("Unknown aggregation rule: ", rule, call. = FALSE)
  )
}

# Aggregate one series (data frame with `date` and `value`) to `to`, using the
# registry rule. Periods with fewer than `min_obs` observations are NA.
ek_aggregate_series <- function(data, to, rule, min_obs = 1L) {
  data <- data[order(data$date), c("date", "value"), drop = FALSE]
  out <- ek_align_frequency(data, to = to, fun = ek_aggregation_fun(rule))
  if (min_obs > 1L) {
    period <- ek_floor_date(data$date, to)
    n_obs <- tapply(!is.na(data$value), period, sum)
    counts <- as.numeric(n_obs[as.character(out$date)])
    out$value[is.na(counts) | counts < min_obs] <- NA_real_
  }
  out
}

# Last day of the period that starts at `date`.
ek_period_end <- function(date, frequency) {
  months <- switch(frequency, month = 1L, quarter = 3L, monthly = 1L, quarterly = 3L, NA)
  if (is.na(months)) return(as.Date(date))
  ek_add_months(ek_floor_date(date, if (months == 3L) "quarter" else "month"), months) - 1
}

ek_get_sample_series <- function(meta, frequency, vintage) {
  data <- ek_load_dataset("macro_monthly")
  missing <- setdiff(meta$id, names(data))
  if (length(missing) > 0) {
    stop("Series not in sample data: ", paste(missing, collapse = ", "),
         ". Use source = \"fred\".", call. = FALSE)
  }
  data <- data[order(data$date), c("date", meta$id), drop = FALSE]
  if (frequency == "month") {
    out <- data
  } else {
    cols <- lapply(seq_len(nrow(meta)), function(i) {
      s <- data.frame(date = data$date, value = data[[meta$id[i]]])
      agg <- ek_aggregate_series(s, "quarter", meta$aggregation[i], min_obs = 3L)
      names(agg)[2] <- meta$id[i]
      agg
    })
    out <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), cols)
  }
  if (!is.null(vintage)) out <- ek_mask_release_lag(out, meta, frequency, vintage)
  out
}

# Release-lag approximation of point-in-time data: a value is available once
# the later of the output period end and the native period end, plus the
# series' release lag, has passed.
ek_mask_release_lag <- function(data, meta, frequency, vintage) {
  data <- data[data$date <= vintage, , drop = FALSE]
  out_end <- ek_period_end(data$date, frequency)
  for (i in seq_len(nrow(meta))) {
    native_end <- ek_period_end(data$date, meta$frequency[i])
    available <- pmax(out_end, native_end) + meta$release_lag_days[i]
    data[[meta$id[i]]][available > vintage] <- NA_real_
  }
  data
}

ek_load_dataset <- function(name) {
  env <- new.env(parent = baseenv())
  utils::data(list = name, package = "econkit", envir = env)
  if (!exists(name, envir = env, inherits = FALSE)) {
    stop("Bundled dataset not found: ", name, call. = FALSE)
  }
  get(name, envir = env, inherits = FALSE)
}

# Single-series FRED download. With a vintage, ALFRED returns the
# observations as they were published on that date.
ek_fred_fetch <- function(source_id, vintage = NULL) {
  args <- list(series_id = source_id)
  if (!is.null(vintage)) {
    args$realtime_start <- vintage
    args$realtime_end <- vintage
  }
  obs <- do.call(fredr::fredr, args)
  data.frame(date = as.Date(obs$date), value = as.numeric(obs$value))
}

ek_get_fred <- function(meta, frequency = "month", vintage = NULL, refresh = FALSE) {
  if (!requireNamespace("fredr", quietly = TRUE)) {
    stop("Package 'fredr' is required for source = \"fred\".", call. = FALSE)
  }
  if (!nzchar(Sys.getenv("FRED_API_KEY"))) {
    stop(
      "FRED_API_KEY is not set. Add it to your .Renviron, or use source = \"sample\".",
      call. = FALSE
    )
  }
  series <- lapply(seq_len(nrow(meta)), function(i) {
    sid <- meta$source_id[i]
    key <- paste0("fred_", sid, if (!is.null(vintage)) paste0("_v", format(vintage, "%Y%m%d")))
    raw <- ek_cached(key, refresh = refresh, function() ek_fred_fetch(sid, vintage))
    min_obs <- if (meta$frequency[i] == "monthly" && frequency == "quarter") 3L else 1L
    agg <- ek_aggregate_series(raw, frequency, meta$aggregation[i], min_obs = min_obs)
    names(agg)[2] <- meta$id[i]
    agg
  })
  Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), series)
}

#' Cache directory
#'
#' Location of the on-disk cache used for downloaded data. Override it with
#' the `ECONKIT_CACHE` environment variable.
#'
#' @return A directory path.
#' @export
ek_cache_dir <- function() {
  Sys.getenv("ECONKIT_CACHE", tools::R_user_dir("econkit", which = "cache"))
}

ek_cached <- function(key, fetch, refresh = FALSE) {
  dir <- ek_cache_dir()
  path <- file.path(dir, paste0(key, ".rds"))
  if (!refresh && file.exists(path)) return(readRDS(path))
  value <- fetch()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(value, path)
  value
}

#' Floor dates to a period start
#'
#' @param date A `Date` vector.
#' @param to `"month"` or `"quarter"`.
#' @return A `Date` vector of period start dates.
#' @export
#' @examples
#' ek_floor_date(as.Date(c("2024-02-17", "2024-05-31")), "quarter")
ek_floor_date <- function(date, to = c("month", "quarter")) {
  to <- match.arg(to)
  lt <- as.POSIXlt(as.Date(date))
  if (to == "quarter") lt$mon <- (lt$mon %/% 3) * 3
  as.Date(sprintf("%04d-%02d-01", lt$year + 1900, lt$mon + 1))
}

#' Align series to a lower frequency
#'
#' Aggregates daily, weekly or monthly observations to monthly or quarterly
#' frequency. Missing values are dropped within each period.
#'
#' @param data A data frame with a `date` column and numeric columns.
#' @param to Target frequency: `"month"` or `"quarter"`.
#' @param fun Aggregation function, for example `mean` (default) or a
#'   last-value function for end-of-period values.
#' @return A data frame indexed by period start date.
#' @export
#' @examples
#' d <- data.frame(date = as.Date("2024-01-01") + 0:89, x = 1:90)
#' ek_align_frequency(d, "quarter")
ek_align_frequency <- function(data, to = c("month", "quarter"), fun = mean) {
  to <- match.arg(to)
  if (!"date" %in% names(data)) stop("`data` must have a `date` column.", call. = FALSE)
  cols <- setdiff(names(data), "date")
  period <- ek_floor_date(data$date, to)
  agg <- function(x) if (all(is.na(x))) NA_real_ else fun(x[!is.na(x)])
  out <- stats::aggregate(data[cols], by = list(date = period), FUN = agg)
  out[order(out$date), , drop = FALSE]
}

#' Lag a vector
#'
#' @param x A vector.
#' @param n Number of periods to lag (negative values lead).
#' @return A vector the same length as `x`, padded with `NA`.
#' @export
#' @examples
#' ek_lag(1:5, 2)
#' ek_lag(1:5, -1)
ek_lag <- function(x, n = 1L) {
  len <- length(x)
  if (n == 0) return(x)
  pad <- rep(NA, min(abs(n), len))
  if (n > 0) c(pad, x[seq_len(max(len - n, 0))]) else c(x[-seq_len(min(-n, len))], pad)
}

#' Transform a series
#'
#' Applies a standard transformation for macro analysis.
#'
#' @param x Numeric vector, regularly spaced.
#' @param method One of `"level"`, `"diff"` (change over `n` periods), `"pct"`
#'   (percent change over `n` periods), `"yoy"` (percent change over `freq`
#'   periods), `"log_diff"` (100 times the log difference over `n` periods),
#'   or `"annualized"` (compound annualised percent change over `n` periods).
#' @param n Number of periods for `diff`, `pct`, `log_diff` and `annualized`.
#' @param freq Periods per year (12 for monthly, 4 for quarterly).
#' @return A numeric vector the same length as `x`.
#' @export
#' @examples
#' cpi <- ek_get_series("CPIAUCSL")$CPIAUCSL
#' tail(ek_transform(cpi, "yoy"))
ek_transform <- function(x, method = c("level", "diff", "pct", "yoy", "log_diff", "annualized"),
                         n = 1L, freq = 12L) {
  method <- match.arg(method)
  switch(method,
    level = x,
    diff = x - ek_lag(x, n),
    pct = 100 * (x / ek_lag(x, n) - 1),
    yoy = 100 * (x / ek_lag(x, freq) - 1),
    log_diff = 100 * log(x / ek_lag(x, n)),
    annualized = 100 * ((x / ek_lag(x, n))^(freq / n) - 1)
  )
}

ek_add_months <- function(date, n) {
  lt <- as.POSIXlt(as.Date(date))
  lt$mon <- lt$mon + n
  as.Date(lt)
}
