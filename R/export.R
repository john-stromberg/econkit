#' Export series for BI tools and data platforms
#'
#' Writes the output of [ek_get_series()] in long format (one row per date
#' and series; see [ek_series_long()]) as Parquet or CSV, together with a
#' provenance sidecar `<path>.provenance.json`. Parquet files also carry the
#' provenance in the file's key-value metadata under `econkit_provenance`,
#' so it travels with the data into lakes and warehouses.
#'
#' The provenance includes `data_md5`, a checksum of the exported rows in a
#' canonical CSV form, so consumers can confirm two exports hold the same
#' data regardless of file format. Files are written to a temporary file and
#' then renamed, so readers never see a partially written export.
#'
#' @param x A data frame returned by [ek_get_series()].
#' @param path Output file path.
#' @param format `"parquet"` (requires the `arrow` package) or `"csv"`.
#' @param overwrite Replace existing files at `path`.
#' @return Invisibly, a list with `path`, `provenance_path`, `n_rows` and
#'   `data_md5`.
#' @export
#' @examples
#' x <- ek_get_series(c("UNRATE", "FEDFUNDS"), start = "2020-01-01")
#' out <- ek_export_series(x, tempfile(fileext = ".csv"), format = "csv")
#' str(jsonlite::read_json(out$provenance_path)[c("source", "data_md5", "n_rows")])
ek_export_series <- function(x, path, format = c("parquet", "csv"), overwrite = FALSE) {
  format <- match.arg(format)
  prov <- ek_provenance(x)
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  sidecar <- paste0(path, ".provenance.json")
  if (!overwrite && (file.exists(path) || file.exists(sidecar))) {
    stop("File exists: ", path, ". Use overwrite = TRUE.", call. = FALSE)
  }
  if (format == "parquet" && !requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required for format = \"parquet\".", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  long <- ek_series_long(x)
  meta <- prov
  meta$series <- lapply(seq_len(nrow(prov$series)), function(i) as.list(prov$series[i, ]))
  meta$format <- format
  meta$n_rows <- nrow(long)
  meta$data_md5 <- ek_data_md5(long)
  meta$exported_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  json <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null", na = "null",
                           pretty = TRUE, digits = NA)

  ek_write_atomic(path, function(tmp) {
    if (format == "csv") {
      csv <- long
      csv$date <- format(csv$date)
      utils::write.csv(csv, tmp, row.names = FALSE, na = "")
    } else {
      tbl <- arrow::arrow_table(long)
      tbl$metadata$econkit_provenance <- as.character(json)
      arrow::write_parquet(tbl, tmp)
    }
  })
  ek_write_atomic(sidecar, function(tmp) writeLines(json, tmp, useBytes = TRUE))
  invisible(list(path = path, provenance_path = sidecar, n_rows = meta$n_rows,
                 data_md5 = meta$data_md5))
}

ek_data_md5 <- function(long) {
  canon <- long
  canon$date <- format(canon$date)
  canon$value <- format(canon$value, digits = 15, trim = TRUE)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  utils::write.csv(canon, tmp, row.names = FALSE, na = "")
  unname(tools::md5sum(tmp))
}

ek_write_atomic <- function(path, write) {
  tmp <- tempfile(pattern = ".econkit-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  write(tmp)
  if (file.exists(path) && !file.remove(path)) {
    stop("Could not replace existing file: ", path, call. = FALSE)
  }
  if (!file.rename(tmp, path)) stop("Could not write file: ", path, call. = FALSE)
  invisible(path)
}

#' Expected release calendar
#'
#' Estimates when each period's observation of a registry series becomes
#' available. The rule matches the point-in-time approximation in
#' [ek_get_series()], namely the later of the output period end and the
#' series' native period end, plus `release_lag_days`. Use it to schedule refreshes,
#' or to see which data will land before a meeting or rebalance date.
#'
#' The lags are conservative registry estimates (basis
#' `"registry_release_lag"`), not official release schedules; holidays and
#' agency calendar changes are not modelled.
#'
#' @param ids Series ids from [ek_series_catalog()]; `NULL` uses all series.
#' @param from,to Date range for expected releases (inclusive).
#' @param frequency Output frequency, `"month"` or `"quarter"`, as in
#'   [ek_get_series()].
#' @return A data frame sorted by `expected_release` with columns
#'   `series_id`, `title`, `source`, `period` (start of the observation
#'   period), `period_end`, `expected_release`, `release_lag_days` and
#'   `basis`.
#' @export
#' @examples
#' ek_release_calendar(c("UNRATE", "CPIAUCSL", "GDPC1"), from = "2025-01-01", to = "2025-03-31")
ek_release_calendar <- function(ids = NULL, from = Sys.Date(), to = as.Date(from) + 90,
                                frequency = c("month", "quarter")) {
  frequency <- match.arg(frequency)
  from <- as.Date(from)
  to <- as.Date(to)
  if (length(from) != 1 || length(to) != 1 || is.na(from) || is.na(to) || to < from) {
    stop("`from` and `to` must be dates with from <= to.", call. = FALSE)
  }
  cat <- ek_series_catalog()
  if (is.null(ids)) ids <- cat$id
  unknown <- setdiff(ids, cat$id)
  if (length(unknown) > 0) {
    stop("Unknown series id(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  meta <- cat[match(unique(ids), cat$id), , drop = FALSE]
  rows <- lapply(seq_len(nrow(meta)), function(i) {
    lag <- meta$release_lag_days[i]
    step <- if (frequency == "quarter" || meta$frequency[i] == "quarterly") "quarter" else "month"
    by <- if (step == "quarter") "3 months" else "1 month"
    periods <- seq(ek_floor_date(from - lag - 100, "quarter"), ek_floor_date(to, step), by = by)
    period_end <- pmax(ek_period_end(periods, frequency),
                       ek_period_end(periods, meta$frequency[i]))
    release <- period_end + lag
    keep <- release >= from & release <= to
    if (!any(keep)) return(NULL)
    data.frame(series_id = meta$id[i], title = meta$title[i], source = meta$source[i],
               period = periods[keep], period_end = period_end[keep],
               expected_release = release[keep], release_lag_days = lag,
               basis = "registry_release_lag", stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(series_id = character(), title = character(), source = character(),
                      period = as.Date(character()), period_end = as.Date(character()),
                      expected_release = as.Date(character()), release_lag_days = integer(),
                      basis = character(), stringsAsFactors = FALSE)
  }
  out <- out[order(out$expected_release, out$series_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}
