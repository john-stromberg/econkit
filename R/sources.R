ek_source_registry <- new.env(parent = emptyenv())

ek_builtin_sources <- function() {
  list(
    fred = list(
      name = "fred",
      fetch = function(source_id, vintage = NULL) ek_fred_fetch(source_id, vintage),
      check = ek_fred_check, # nolint: object_usage_linter
      supports_vintage = TRUE,
      vintage_method = "alfred",
      synthetic = FALSE,
      cache = TRUE,
      description = "FRED/ALFRED via the fredr package (needs FRED_API_KEY)."
    )
  )
}

#' Register a data source adapter
#'
#' Adds a provider, such as a licensed vendor extract or an internal data
#' warehouse, that [ek_get_series()] can read from. Series served by the
#' adapter are added to the registry with `source: <name>` in a YAML file
#' listed in the `econkit.registry` option or the `ECONKIT_REGISTRY`
#' environment variable (see [ek_series_catalog()]). The registry's
#' aggregation rules, release lags and vintage handling then apply exactly as
#' they do for FRED.
#'
#' Registrations last for the R session. For the API server, register
#' adapters in the script that starts the server, before `plumber::pr()`.
#'
#' @param name Adapter name, used as the registry `source` value. `"sample"`
#'   and `"fred"` are reserved.
#' @param fetch A function `function(source_id, vintage)` returning a data
#'   frame with a `Date` column `date` and numeric column `value` at the
#'   series' native frequency. `vintage` is `NULL` for the latest data, or a
#'   `Date` when `supports_vintage` is `TRUE`.
#' @param supports_vintage Whether `fetch` returns point-in-time data for a
#'   vintage. If `FALSE`, vintages use the release-lag approximation.
#' @param synthetic Whether the data is synthetic (recorded in provenance).
#' @param cache Whether to cache downloads on disk (see [ek_cache_dir()]).
#' @param description Short description shown by [ek_sources()].
#' @param check Optional function called before fetching, for example to
#'   verify credentials. It should stop with an informative error.
#' @param overwrite Replace an existing registration with the same name.
#' @return The adapter (a list), invisibly.
#' @seealso [ek_csv_fetcher()], [ek_sources()]
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(source_id = "X1", date = c("2024-01-01", "2024-02-01"),
#'                             value = c(1, 2)), path, row.names = FALSE)
#' ek_register_source("vendor_demo", ek_csv_fetcher(path), overwrite = TRUE)
#' ek_sources()
#' ek_unregister_source("vendor_demo")
ek_register_source <- function(name, fetch, supports_vintage = FALSE, synthetic = FALSE,
                               cache = FALSE, description = "", check = NULL,
                               overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1 || !grepl("^[A-Za-z][A-Za-z0-9_.-]*$", name)) {
    stop("`name` must be a single identifier such as \"vendor\".", call. = FALSE)
  }
  if (name %in% c("sample", names(ek_builtin_sources()))) {
    stop("\"", name, "\" is a built-in source and cannot be registered.", call. = FALSE)
  }
  if (!is.function(fetch)) stop("`fetch` must be a function.", call. = FALSE)
  if (!is.null(check) && !is.function(check)) {
    stop("`check` must be a function or NULL.", call. = FALSE)
  }
  flags <- list(supports_vintage = supports_vintage, synthetic = synthetic, cache = cache,
                overwrite = overwrite)
  for (flag in names(flags)) {
    if (!isTRUE(flags[[flag]]) && !isFALSE(flags[[flag]])) {
      stop("`", flag, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!overwrite && exists(name, envir = ek_source_registry, inherits = FALSE)) {
    stop("Source \"", name, "\" is already registered; use overwrite = TRUE.", call. = FALSE)
  }
  adapter <- list(
    name = name, fetch = fetch, check = check, supports_vintage = supports_vintage,
    vintage_method = "source_vintage", synthetic = synthetic, cache = cache,
    description = as.character(description)
  )
  assign(name, adapter, envir = ek_source_registry)
  invisible(adapter)
}

#' @rdname ek_register_source
#' @export
ek_unregister_source <- function(name) {
  if (exists(name, envir = ek_source_registry, inherits = FALSE)) {
    rm(list = name, envir = ek_source_registry)
  }
  invisible(NULL)
}

#' List data sources
#'
#' @return A data frame with one row per available source and columns
#'   `name`, `description`, `builtin`, `supports_vintage`, `synthetic`,
#'   `cache` and `n_series` (number of registry series using the source).
#' @export
#' @examples
#' ek_sources()
ek_sources <- function() {
  adapters <- c(ek_builtin_sources(), mget(sort(ls(ek_source_registry)),
                                           envir = ek_source_registry))
  catalog <- ek_series_catalog()
  out <- data.frame(
    name = c("sample", names(adapters)),
    description = c("Bundled synthetic monthly macro data (offline).",
                    vapply(adapters, `[[`, "", "description")),
    builtin = c(TRUE, names(adapters) %in% names(ek_builtin_sources())),
    supports_vintage = c(FALSE, vapply(adapters, `[[`, TRUE, "supports_vintage")),
    synthetic = c(TRUE, vapply(adapters, `[[`, TRUE, "synthetic")),
    cache = c(FALSE, vapply(adapters, `[[`, TRUE, "cache")),
    stringsAsFactors = FALSE
  )
  out$n_series <- vapply(out$name, function(s) {
    if (s == "sample") sum(catalog$in_sample) else sum(catalog$source == s)
  }, integer(1), USE.NAMES = FALSE)
  rownames(out) <- NULL
  out
}

ek_source_adapter <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    stop("`source` must be a single source name.", call. = FALSE)
  }
  builtin <- ek_builtin_sources()
  if (name %in% names(builtin)) return(builtin[[name]])
  if (exists(name, envir = ek_source_registry, inherits = FALSE)) {
    return(get(name, envir = ek_source_registry))
  }
  stop("Unknown source \"", name, "\". Available: ",
       paste(ek_sources()$name, collapse = ", "), ".", call. = FALSE)
}

#' Fetch series from a long-format CSV file
#'
#' Builds a `fetch` function for [ek_register_source()] from a CSV extract
#' with columns `source_id`, `date` (`YYYY-MM-DD`) and `value`. An optional
#' `realtime_start` column (the date each value was published) makes the file
#' point-in-time: for a vintage, the latest value published on or before that
#' date is used, and dates not yet published are dropped. Without a vintage
#' the latest published value is used. The file is read on each call.
#'
#' @param path Path to the CSV file.
#' @return A function `function(source_id, vintage = NULL)`.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(
#'   source_id = "X1", date = c("2024-01-01", "2024-01-01", "2024-02-01"),
#'   value = c(1, 1.5, 2), realtime_start = c("2024-02-05", "2024-03-05", "2024-03-05")
#' ), path, row.names = FALSE)
#' fetch <- ek_csv_fetcher(path)
#' fetch("X1", as.Date("2024-02-10"))
#' fetch("X1")
ek_csv_fetcher <- function(path) {
  force(path)
  function(source_id, vintage = NULL) {
    if (!file.exists(path)) stop("Source file not found: ", path, call. = FALSE)
    raw <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
    missing <- setdiff(c("source_id", "date", "value"), names(raw))
    if (length(missing) > 0) {
      stop("Source file is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
    }
    x <- raw[raw$source_id == source_id, , drop = FALSE]
    if (nrow(x) == 0) stop("Series `", source_id, "` not found in ", path, call. = FALSE)
    x$date <- as.Date(x$date)
    x$value <- suppressWarnings(as.numeric(x$value))
    if ("realtime_start" %in% names(x)) {
      x$realtime_start <- as.Date(x$realtime_start)
      if (!is.null(vintage)) x <- x[x$realtime_start <= as.Date(vintage), , drop = FALSE]
      x <- x[order(x$date, x$realtime_start), , drop = FALSE]
      x <- x[!duplicated(x$date, fromLast = TRUE), , drop = FALSE]
    } else if (!is.null(vintage)) {
      stop("Source file has no `realtime_start` column, so it cannot serve vintages.",
           call. = FALSE)
    }
    x <- x[order(x$date), , drop = FALSE]
    data.frame(date = x$date, value = x$value)
  }
}
