#' Model inventory and monitoring thresholds
#'
#' Reads the model inventory in `inst/config/models.yml`: one entry per model
#' with its entry-point function (whose help page holds the model card),
#' version, risk tier, status, owner, intended use, last review date and
#' monitoring thresholds. Organizations can add models, or override bundled
#' ones with the same `id`, in their own YAML files in the same layout, listed
#' in `path`, `options(econkit.model_registry = )` or the
#' `ECONKIT_MODEL_REGISTRY` environment variable (`;`-separated).
#'
#' @param monitoring If `TRUE`, return the monitoring thresholds (one row per
#'   model and metric, with `lower`, `upper` and `drift_z`) instead of the
#'   inventory.
#' @param path Optional additional registry files. Defaults to the option or
#'   environment variable.
#' @return A data frame.
#' @seealso [ek_record_run()], [ek_monitor_metrics()]
#' @export
#' @examples
#' ek_model_registry()[c("id", "function", "risk_tier", "status")]
#' ek_model_registry(monitoring = TRUE)
ek_model_registry <- function(monitoring = FALSE, path = NULL) {
  specs <- ek_model_specs(path)
  if (monitoring) {
    rows <- lapply(specs, function(m) {
      if (!length(m$monitoring)) return(NULL)
      do.call(rbind, lapply(m$monitoring, function(x) {
        data.frame(model_id = m$id, metric = x$metric,
                   lower = ek_num_or_na(x$lower), upper = ek_num_or_na(x$upper),
                   drift_z = ek_num_or_na(x$drift_z), stringsAsFactors = FALSE)
      }))
    })
    out <- do.call(rbind, rows)
    if (is.null(out)) {
      out <- data.frame(model_id = character(), metric = character(), lower = numeric(),
                        upper = numeric(), drift_z = numeric(), stringsAsFactors = FALSE)
    }
    rownames(out) <- NULL
    return(out)
  }
  fields <- c("id", "name", "function", "version", "category", "risk_tier", "status",
              "owner", "intended_use")
  out <- as.data.frame(lapply(stats::setNames(fields, fields), function(f) {
    vapply(specs, function(m) as.character(m[[f]]), character(1))
  }), stringsAsFactors = FALSE, check.names = FALSE)
  out$last_review <- as.Date(vapply(specs, function(m) {
    if (is.null(m$last_review) || !nzchar(m$last_review)) NA_character_ else m$last_review
  }, character(1)))
  out$n_metrics <- vapply(specs, function(m) length(m$monitoring), integer(1))
  out$help <- paste0("?", out[["function"]])
  rownames(out) <- NULL
  out
}

ek_model_specs <- function(path = NULL) {
  if (is.null(path)) {
    path <- getOption("econkit.model_registry", Sys.getenv("ECONKIT_MODEL_REGISTRY", ""))
    path <- strsplit(paste(path, collapse = ";"), ";", fixed = TRUE)[[1]]
    path <- trimws(path[nzchar(trimws(path))])
  }
  files <- c(system.file("config", "models.yml", package = "econkit", mustWork = TRUE), path)
  specs <- list()
  for (file in files) {
    if (!file.exists(file)) stop("Model registry file not found: ", file, call. = FALSE)
    for (m in yaml::read_yaml(file)$models) specs[[m$id]] <- ek_check_model_spec(m, file)
  }
  unname(specs)
}

ek_check_model_spec <- function(m, file) {
  required <- c("id", "name", "function", "version", "category", "risk_tier", "status",
                "owner", "intended_use")
  missing <- required[!vapply(required, function(f) {
    is.character(m[[f]]) && length(m[[f]]) == 1L && nzchar(m[[f]])
  }, logical(1))]
  where <- paste0(" (model `", if (is.character(m$id)) m$id else "?", "` in ", basename(file), ")")
  if (length(missing)) {
    stop("Missing or invalid field(s) ", paste(missing, collapse = ", "), where, call. = FALSE)
  }
  if (!m$risk_tier %in% c("low", "medium", "high") ||
        !m$status %in% c("prototype", "validated", "retired")) {
    stop("`risk_tier` must be low/medium/high and `status` prototype/validated/retired",
         where, call. = FALSE)
  }
  for (x in m$monitoring) {
    bounds <- c(x$lower, x$upper, x$drift_z)
    if (!is.character(x$metric) || length(x$metric) != 1L ||
          (length(bounds) && !is.numeric(bounds))) {
      stop("Monitoring entries need a `metric` and numeric bounds", where, call. = FALSE)
    }
  }
  m
}

ek_num_or_na <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)

#' Record and read model-run lineage
#'
#' `ek_record_run()` appends one JSON line per model run to a run log: a run
#' ID, UTC timestamp, model ID and version (from [ek_model_registry()]),
#' econkit and R versions, an optional user, MD5 hashes of the inputs and
#' outputs, a summary of the inputs' data provenance (source, synthetic flag,
#' vintage and series), parameters and monitoring metrics. `ek_read_runs()`
#' reads the log back as a data frame.
#'
#' Hashes are of the serialized R objects, ignoring retrieval timestamps and
#' environments, so rerunning on unchanged data gives the same input hash.
#' They detect changes within an installation; they are not portable
#' identifiers across R versions. Appending one line per run is safe for a
#' single writer; use one log per process, or a database, for concurrent
#' writers.
#'
#' @param model_id A model `id` from [ek_model_registry()].
#' @param inputs Input data: a data frame, model object or list of them.
#' @param outputs Model outputs.
#' @param params Named list of parameters.
#' @param metrics Optional named numeric vector of monitoring metrics.
#' @param user Optional user or service name.
#' @param note Optional free-text note.
#' @param log Run-log file. Defaults to `options(econkit.run_log = )`, the
#'   `ECONKIT_RUN_LOG` environment variable or `runs.jsonl` in the user data
#'   directory.
#' @return `ek_record_run()` returns the record (a list), invisibly.
#'   `ek_read_runs()` returns a data frame with one row per run; `params`
#'   and `metrics` are JSON strings.
#' @seealso [ek_monitor_metrics()]
#' @export
#' @examples
#' log <- tempfile(fileext = ".jsonl")
#' data <- ek_get_series(c("FEDFUNDS", "UNRATE"))
#' fit <- ek_bvar(data, p = 2)
#' fc <- ek_bvar_forecast(fit, h = 6, draws = 50, seed = 1)
#' ek_record_run("bvar_minnesota", inputs = data, outputs = fc$summary,
#'               params = list(p = 2, h = 6), log = log)
#' ek_read_runs(log)
ek_record_run <- function(model_id, inputs = NULL, outputs = NULL, params = list(),
                          metrics = NULL, user = NULL, note = NULL, log = NULL) {
  log <- ek_run_log_path(log)
  registry <- ek_model_registry()
  if (!is.character(model_id) || length(model_id) != 1L || !model_id %in% registry$id) {
    stop("`model_id` must be a model id from ek_model_registry().", call. = FALSE)
  }
  if (!is.list(params) || (length(params) && is.null(names(params)))) {
    stop("`params` must be a named list.", call. = FALSE)
  }
  if (!is.null(metrics) && (!is.numeric(metrics) || is.null(names(metrics)) ||
                              any(!nzchar(names(metrics))))) {
    stop("`metrics` must be a named numeric vector.", call. = FALSE)
  }
  timestamp <- Sys.time()
  input_md5 <- ek_object_md5(inputs)
  output_md5 <- ek_object_md5(outputs)
  ek_run_counter$n <- ek_run_counter$n + 1L
  run_id <- substr(ek_object_md5(list(format(timestamp, "%Y%m%d%H%M%OS6"), Sys.getpid(),
                                      ek_run_counter$n, input_md5, output_md5)), 1, 16)
  record <- list(
    run_id = run_id,
    timestamp = format(timestamp, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
    model_id = model_id,
    model_version = registry$version[registry$id == model_id],
    econkit_version = as.character(utils::packageVersion("econkit")),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    user = user,
    input_md5 = input_md5,
    output_md5 = output_md5,
    provenance = ek_provenance_summary(inputs),
    params = params,
    metrics = if (is.null(metrics)) NULL else as.list(metrics),
    note = note
  )
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", na = "null",
                           digits = NA, POSIXt = "ISO8601", Date = "ISO8601")
  cat(line, "\n", file = log, append = TRUE, sep = "")
  invisible(record)
}

ek_run_counter <- new.env(parent = emptyenv())
ek_run_counter$n <- 0L

#' @rdname ek_record_run
#' @param since Optional date or date-time (interpreted in UTC); only runs at or after it
#'   are returned.
#' @export
ek_read_runs <- function(log = NULL, model_id = NULL, since = NULL) {
  log <- ek_run_log_path(log)
  empty <- data.frame(
    run_id = character(), timestamp = as.POSIXct(character(), tz = "UTC"),
    model_id = character(), model_version = character(), econkit_version = character(),
    user = character(), input_md5 = character(), output_md5 = character(),
    synthetic = logical(), sources = character(), vintages = character(),
    params = character(), metrics = character(), note = character(),
    stringsAsFactors = FALSE
  )
  if (!file.exists(log)) return(empty)
  lines <- readLines(log, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(empty)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  json <- function(x) {
    if (is.null(x) || !length(x)) return(NA_character_)
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA))
  }
  collapse <- function(prov, field) {
    if (!length(prov)) return(NA_character_)
    paste(unique(vapply(prov, function(p) chr(p[[field]]), character(1))), collapse = ",")
  }
  rows <- lapply(lines, function(line) {
    r <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    prov <- r$provenance
    synthetic <- vapply(prov, function(p) isTRUE(p$synthetic), logical(1))
    data.frame(
      run_id = chr(r$run_id),
      timestamp = as.POSIXct(chr(r$timestamp), format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      model_id = chr(r$model_id), model_version = chr(r$model_version),
      econkit_version = chr(r$econkit_version), user = chr(r$user),
      input_md5 = chr(r$input_md5), output_md5 = chr(r$output_md5),
      synthetic = if (length(prov)) any(synthetic) else NA,
      sources = collapse(prov, "source"), vintages = collapse(prov, "vintage"),
      params = json(r$params), metrics = json(r$metrics), note = chr(r$note),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (!is.null(model_id)) out <- out[out$model_id %in% model_id, , drop = FALSE]
  if (!is.null(since)) {
    out <- out[which(out$timestamp >= as.POSIXct(since, tz = "UTC")), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

ek_run_log_path <- function(log = NULL) {
  if (is.null(log)) log <- getOption("econkit.run_log", Sys.getenv("ECONKIT_RUN_LOG", ""))
  if (!nzchar(log)) log <- file.path(tools::R_user_dir("econkit", which = "data"), "runs.jsonl")
  if (!is.character(log) || length(log) != 1L) stop("`log` must be one file path.", call. = FALSE)
  log
}

ek_object_md5 <- function(x) {
  if (is.null(x)) return(NA_character_)
  tmp <- tempfile()
  on.exit(unlink(tmp))
  con <- file(tmp, "wb")
  serialize(ek_hash_ready(x), con, version = 3)
  close(con)
  unname(tools::md5sum(tmp))
}

# Drops retrieval timestamps and environments, which change between otherwise
# identical runs.
ek_hash_ready <- function(x, depth = 0L) {
  if (is.null(x)) return(x)
  if (is.environment(x)) return("<environment>")
  if (is.function(x)) return("<function>")
  attr(x, ".Environment") <- NULL
  terms <- attr(x, "terms")
  if (!is.null(terms)) {
    attr(terms, ".Environment") <- NULL
    attr(x, "terms") <- terms
  }
  prov <- attr(x, "provenance")
  if (is.list(prov)) {
    prov$retrieved_at <- NULL
    attr(x, "provenance") <- prov
  }
  if (is.list(x) && depth < 6L) {
    if (!is.null(names(x)) && "retrieved_at" %in% names(x)) x$retrieved_at <- NULL
    x[] <- lapply(x, ek_hash_ready, depth = depth + 1L)
  }
  x
}

ek_provenance_summary <- function(x, depth = 0L) {
  prov <- if (is.data.frame(x)) attr(x, "provenance") else if (is.list(x)) x$provenance
  if (is.list(prov) && !is.null(prov$source)) {
    series <- if (is.data.frame(prov$series)) prov$series$id else NULL
    return(list(list(source = prov$source, synthetic = isTRUE(prov$synthetic),
                     frequency = prov$frequency, vintage = prov$vintage,
                     vintage_method = prov$vintage_method, series = I(series))))
  }
  if (is.list(x) && !is.data.frame(x) && depth < 2L) {
    out <- unlist(lapply(x, ek_provenance_summary, depth = depth + 1L), recursive = FALSE)
    return(unique(out))
  }
  list()
}

#' Monitor model metrics against thresholds and recent history
#'
#' Compares the latest value of each model metric with its bounds and with
#' its recent history. A value outside `lower`/`upper` is a breach; a value
#' whose z-score versus the previous `window` values exceeds `drift_z` in
#' absolute value is drift. Thresholds default to those in
#' [ek_model_registry()].
#'
#' @param metrics A data frame with `model_id`, `metric`, `value` and a
#'   `date` or `timestamp` column, or a run log from [ek_read_runs()], whose
#'   recorded metrics are used.
#' @param thresholds Data frame with `model_id`, `metric`, `lower`, `upper`
#'   and `drift_z` (`NA` for none). Defaults to the registry thresholds.
#' @param window Number of previous values used for drift.
#' @param min_history Minimum previous values needed to compute drift.
#' @return A data frame with one row per model and metric: the latest
#'   `date` and `value`, the bounds, `n_history`, `history_mean`, `z_score`,
#'   `breach`, `drift` and `status` (`"breach"`, `"drift"` or `"ok"`),
#'   ordered with breaches first.
#' @export
#' @examples
#' metrics <- data.frame(
#'   model_id = "nowcast_factor_bridge", metric = "r_squared",
#'   date = seq(as.Date("2024-01-01"), by = "quarter", length.out = 6),
#'   value = c(0.62, 0.58, 0.6, 0.61, 0.59, 0.25)
#' )
#' ek_monitor_metrics(metrics)
ek_monitor_metrics <- function(metrics, thresholds = NULL, window = 12L, min_history = 3L) {
  if (is.data.frame(metrics) && "metrics" %in% names(metrics) && "run_id" %in% names(metrics)) {
    metrics <- ek_runs_metrics(metrics)
  }
  time_col <- intersect(c("date", "timestamp"), names(metrics))[1]
  if (!is.data.frame(metrics) || is.na(time_col) ||
        !all(c("model_id", "metric", "value") %in% names(metrics)) ||
        !is.numeric(metrics$value)) {
    stop("`metrics` needs model_id, metric, numeric value and date or timestamp columns.",
         call. = FALSE)
  }
  if (is.null(thresholds)) thresholds <- ek_model_registry(monitoring = TRUE)
  if (!is.data.frame(thresholds) ||
        !all(c("model_id", "metric", "lower", "upper", "drift_z") %in% names(thresholds))) {
    stop("`thresholds` needs model_id, metric, lower, upper and drift_z columns.", call. = FALSE)
  }
  window <- as.integer(window)
  min_history <- as.integer(min_history)
  if (length(window) != 1L || is.na(window) || window < 2L ||
        length(min_history) != 1L || is.na(min_history) || min_history < 2L) {
    stop("`window` and `min_history` must be integers of at least 2.", call. = FALSE)
  }
  metrics <- metrics[!is.na(metrics$value), , drop = FALSE]
  key <- paste(metrics$model_id, metrics$metric, sep = "\r")
  th_key <- paste(thresholds$model_id, thresholds$metric, sep = "\r")
  out <- lapply(unique(key), function(k) {
    m <- metrics[key == k, , drop = FALSE]
    m <- m[order(m[[time_col]]), , drop = FALSE]
    n <- nrow(m)
    latest <- m$value[n]
    history <- utils::tail(m$value[-n], window)
    th <- thresholds[th_key == k, , drop = FALSE]
    lower <- if (nrow(th)) th$lower[1] else NA_real_
    upper <- if (nrow(th)) th$upper[1] else NA_real_
    drift_z <- if (nrow(th)) th$drift_z[1] else NA_real_
    hist_sd <- if (length(history) >= min_history) stats::sd(history) else NA_real_
    z <- if (!is.na(hist_sd) && hist_sd > 0) (latest - mean(history)) / hist_sd else NA_real_
    breach <- isTRUE(latest < lower) || isTRUE(latest > upper)
    drift <- isTRUE(abs(z) > drift_z)
    data.frame(
      model_id = m$model_id[1], metric = m$metric[1], time = m[[time_col]][n], value = latest,
      lower = lower, upper = upper, n_history = length(history),
      history_mean = if (length(history)) mean(history) else NA_real_, z_score = z,
      drift_z = drift_z, breach = breach, drift = drift,
      status = if (breach) "breach" else if (drift) "drift" else "ok",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  names(out)[names(out) == "time"] <- time_col
  out <- out[order(match(out$status, c("breach", "drift", "ok")), out$model_id, out$metric), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

ek_runs_metrics <- function(runs) {
  rows <- lapply(seq_len(nrow(runs)), function(i) {
    if (is.na(runs$metrics[i])) return(NULL)
    values <- unlist(jsonlite::fromJSON(runs$metrics[i], simplifyVector = FALSE))
    data.frame(model_id = runs$model_id[i], metric = names(values),
               value = as.numeric(values), timestamp = runs$timestamp[i],
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    stop("The run log has no recorded metrics.", call. = FALSE)
  }
  out
}
