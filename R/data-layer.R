#' Series catalog
#'
#' Returns the catalog of supported macro series, read from
#' `inst/config/series.yml`.
#'
#' @return A data frame with columns `id`, `title`, `units`, `frequency`,
#'   `category` and `transform`.
#' @export
#' @examples
#' ek_series_catalog()
ek_series_catalog <- function() {
  path <- system.file("config", "series.yml", package = "econkit", mustWork = TRUE)
  cfg <- yaml::read_yaml(path)
  fields <- c("id", "title", "units", "frequency", "category", "transform")
  rows <- lapply(cfg$series, function(s) {
    as.data.frame(lapply(stats::setNames(fields, fields), function(f) {
      if (is.null(s[[f]])) NA_character_ else as.character(s[[f]])
    }))
  })
  do.call(rbind, rows)
}

#' Get macro series
#'
#' Unified access to macro series as a wide monthly data frame. Use
#' `source = "sample"` (default) for the bundled synthetic [macro_monthly]
#' dataset, which works offline. Use `source = "fred"` for live FRED data,
#' which requires the `fredr` package and a `FRED_API_KEY` environment
#' variable (put it in `.Renviron`; never commit it). FRED downloads are
#' cached on disk (see [ek_cache_dir()]) and aggregated to monthly frequency
#' with [ek_align_frequency()].
#'
#' @param ids Character vector of series ids. `NULL` returns all catalog series.
#' @param source `"sample"` or `"fred"`.
#' @param start,end Optional start and end dates (inclusive).
#' @param refresh For `source = "fred"`, ignore the cache and re-download.
#'
#' @return A data frame with a `date` column (first day of month) and one
#'   numeric column per series. The attribute `source` records the source.
#' @export
#' @examples
#' x <- ek_get_series(c("UNRATE", "T10Y3M"), start = "2000-01-01")
#' head(x)
ek_get_series <- function(ids = NULL, source = c("sample", "fred"),
                          start = NULL, end = NULL, refresh = FALSE) {
  source <- match.arg(source)
  if (is.null(ids)) ids <- ek_series_catalog()$id
  ids <- unique(as.character(ids))

  out <- switch(source,
    sample = ek_get_sample(ids),
    fred = ek_get_fred(ids, refresh = refresh)
  )

  if (!is.null(start)) out <- out[out$date >= as.Date(start), , drop = FALSE]
  if (!is.null(end)) out <- out[out$date <= as.Date(end), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "source") <- source
  out
}

ek_get_sample <- function(ids) {
  data <- econkit::macro_monthly
  missing <- setdiff(ids, names(data))
  if (length(missing) > 0) {
    stop("Series not in sample data: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data[, c("date", ids), drop = FALSE]
}

ek_get_fred <- function(ids, refresh = FALSE) {
  if (!requireNamespace("fredr", quietly = TRUE)) {
    stop("Package 'fredr' is required for source = \"fred\".", call. = FALSE)
  }
  if (!nzchar(Sys.getenv("FRED_API_KEY"))) {
    stop(
      "FRED_API_KEY is not set. Add it to your .Renviron, or use source = \"sample\".",
      call. = FALSE
    )
  }
  series <- lapply(ids, function(id) {
    raw <- ek_cached(paste0("fred_", id), refresh = refresh, function() {
      obs <- fredr::fredr(series_id = id)
      data.frame(date = as.Date(obs$date), value = as.numeric(obs$value))
    })
    monthly <- ek_align_frequency(raw, to = "month")
    names(monthly)[2] <- id
    monthly
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
#' cpi <- macro_monthly$CPIAUCSL
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
