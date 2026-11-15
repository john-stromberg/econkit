#' Connect to the configured PostgreSQL signal store
#'
#' Reads connection settings from `ECONKIT_PGHOST`, `ECONKIT_PGDATABASE` and
#' `ECONKIT_PGUSER`. `ECONKIT_PGPORT` defaults to 5432, `ECONKIT_PGSSLMODE`
#' defaults to `prefer`, and the password can be supplied through
#' `ECONKIT_PGPASSWORD` or libpq's standard environment settings.
#'
#' @return A DBI connection. The caller is responsible for disconnecting it.
#' @export
#' @examples
#' \dontrun{
#' conn <- ek_connect_postgres()
#' on.exit(DBI::dbDisconnect(conn))
#' }
ek_connect_postgres <- function() {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for PostgreSQL storage.", call. = FALSE)
  }
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Package 'RPostgres' is required for PostgreSQL storage.", call. = FALSE)
  }
  required <- c("ECONKIT_PGHOST", "ECONKIT_PGDATABASE", "ECONKIT_PGUSER")
  config <- Sys.getenv(required, unset = "")
  missing <- required[!nzchar(config)]
  if (length(missing)) {
    stop("Set PostgreSQL connection variables: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  port <- Sys.getenv("ECONKIT_PGPORT", "5432")
  if (!grepl("^[0-9]+$", port) || as.numeric(port) < 1 ||
        as.numeric(port) > 65535) {
    stop("`ECONKIT_PGPORT` must be an integer from 1 to 65535.", call. = FALSE)
  }
  args <- list(
    drv = RPostgres::Postgres(),
    host = config[["ECONKIT_PGHOST"]],
    port = as.integer(port),
    dbname = config[["ECONKIT_PGDATABASE"]],
    user = config[["ECONKIT_PGUSER"]],
    sslmode = Sys.getenv("ECONKIT_PGSSLMODE", "prefer")
  )
  password <- Sys.getenv("ECONKIT_PGPASSWORD", "")
  if (nzchar(password)) args$password <- password
  do.call(DBI::dbConnect, args)
}

#' Initialize the PostgreSQL signal table
#'
#' Creates the append-only signal table and its unique publication key if it
#' does not already exist. The table is shared by
#' [ek_store_signals_db()] and [ek_read_signals_db()].
#'
#' @param conn An open DBI connection.
#' @param table One SQL table name.
#' @return The table name, invisibly.
#' @export
#' @examples
#' \dontrun{
#' conn <- ek_connect_postgres()
#' ek_init_signal_store_db(conn)
#' DBI::dbDisconnect(conn)
#' }
ek_init_signal_store_db <- function(conn, table = "econkit_signals") {
  ek_check_db_connection(conn)
  table <- ek_db_table_name(table)
  qtable <- DBI::dbQuoteIdentifier(conn, table)
  fields <- c(
    as_of = "DATE NOT NULL",
    data_vintage = "DATE NOT NULL",
    signal = "TEXT NOT NULL",
    value = "DOUBLE PRECISION",
    label = "TEXT",
    direction = "TEXT",
    percentile = "DOUBLE PRECISION",
    confidence = "DOUBLE PRECISION",
    n_obs = "INTEGER NOT NULL",
    model_version = "TEXT NOT NULL",
    git_sha = "TEXT",
    change_vs_prior = "DOUBLE PRECISION",
    override = "TEXT",
    override_reason = "TEXT",
    user = "TEXT"
  )
  definitions <- paste0(
    as.character(DBI::dbQuoteIdentifier(conn, names(fields))),
    " ", unname(fields)
  )
  key <- as.character(DBI::dbQuoteIdentifier(
    conn, c("as_of", "signal", "model_version")
  ))
  sql <- paste0(
    "CREATE TABLE IF NOT EXISTS ", as.character(qtable), " (",
    paste(definitions, collapse = ", "),
    ", PRIMARY KEY (", paste(key, collapse = ", "), "))"
  )
  DBI::dbExecute(conn, sql)
  invisible(table)
}

#' Append signals to a DBI-backed immutable store
#'
#' Repeated identical publication keys are idempotent. A duplicate
#' `(as_of, signal, model_version)` with different content is rejected. New
#' rows receive `change_vs_prior` from the latest earlier stored value when
#' one is available.
#'
#' @param signals A data frame from [ek_build_signals()].
#' @param conn An open DBI connection.
#' @param table Signal table initialized by [ek_init_signal_store_db()].
#' @return The number of newly inserted rows, invisibly.
#' @export
#' @examples
#' \dontrun{
#' conn <- ek_connect_postgres()
#' ek_init_signal_store_db(conn)
#' ek_store_signals_db(ek_build_signals(), conn)
#' DBI::dbDisconnect(conn)
#' }
ek_store_signals_db <- function(signals, conn, table = "econkit_signals") {
  ek_validate_signals(signals)
  ek_check_db_connection(conn)
  table <- ek_db_table_name(table)
  ek_init_signal_store_db(conn, table)
  if (!nrow(signals)) return(invisible(0L))

  signals <- signals[order(signals$as_of, signals$signal, signals$model_version), ,
                     drop = FALSE]
  inserted <- 0L
  qtable <- as.character(DBI::dbQuoteIdentifier(conn, table))
  qcolumns <- as.character(DBI::dbQuoteIdentifier(conn, ek_signal_columns))
  key_columns <- as.character(DBI::dbQuoteIdentifier(
    conn, c("as_of", "signal", "model_version")
  ))
  compare_columns <- setdiff(ek_signal_columns, "change_vs_prior")

  DBI::dbWithTransaction(conn, {
    if (inherits(conn, "PqConnection")) {
      lock_name <- as.character(DBI::dbQuoteLiteral(
        conn, paste0("econkit.signal_store.", table)
      ))
      DBI::dbGetQuery(
        conn,
        paste0("SELECT pg_advisory_xact_lock(hashtext(", lock_name, "))")
      )
    }
    for (i in seq_len(nrow(signals))) {
      row <- signals[i, ek_signal_columns, drop = FALSE]
      key_values <- vapply(
        c("as_of", "signal", "model_version"),
        function(column) {
          as.character(DBI::dbQuoteLiteral(conn, row[[column]][1]))
        },
        character(1)
      )
      where <- paste(key_columns, "=", key_values, collapse = " AND ")
      select <- paste0("SELECT ", paste(qcolumns, collapse = ", "),
                       " FROM ", qtable, " WHERE ", where)
      existing <- ek_normalize_db_signals(DBI::dbGetQuery(conn, select))
      if (nrow(existing)) {
        if (!ek_signal_rows_equal(
          existing[1, , drop = FALSE], row, compare_columns
        )) {
          stop("Published signal keys are immutable; a duplicate key has changed.",
               call. = FALSE)
        }
        next
      }

      prior_where <- paste(
        as.character(DBI::dbQuoteIdentifier(conn, "signal")), "=",
        as.character(DBI::dbQuoteLiteral(conn, row$signal[1])), " AND ",
        as.character(DBI::dbQuoteIdentifier(conn, "as_of")), "<",
        as.character(DBI::dbQuoteLiteral(conn, row$as_of[1]))
      )
      prior_sql <- paste0(
        "SELECT ", as.character(DBI::dbQuoteIdentifier(conn, "value")),
        " FROM ", qtable, " WHERE ", prior_where, " ORDER BY ",
        as.character(DBI::dbQuoteIdentifier(conn, "as_of")),
        " DESC LIMIT 1"
      )
      prior <- DBI::dbGetQuery(conn, prior_sql)
      if (is.finite(row$value[1]) && nrow(prior) &&
            is.finite(prior$value[1])) {
        row$change_vs_prior <- row$value[1] - prior$value[1]
      }
      values <- vapply(ek_signal_columns, function(column) {
        as.character(DBI::dbQuoteLiteral(conn, row[[column]][1]))
      }, character(1))
      insert <- paste0(
        "INSERT INTO ", qtable, " (", paste(qcolumns, collapse = ", "),
        ") VALUES (", paste(values, collapse = ", "),
        ") ON CONFLICT (", paste(key_columns, collapse = ", "),
        ") DO NOTHING"
      )
      rows_added <- DBI::dbExecute(conn, insert)
      inserted <- inserted + as.integer(rows_added)

      stored <- ek_normalize_db_signals(DBI::dbGetQuery(conn, select))
      if (!nrow(stored) ||
            !ek_signal_rows_equal(stored[1, , drop = FALSE], row, compare_columns)) {
        stop("A concurrent publication used this key with different content.",
             call. = FALSE)
      }
    }
  })
  invisible(inserted)
}

#' Read the latest DBI-backed signals as of a date
#'
#' @param conn An open DBI connection.
#' @param as_of Optional maximum snapshot date.
#' @param table Signal table initialized by [ek_init_signal_store_db()].
#' @return One latest row per signal, with no row published after `as_of`.
#' @export
#' @examples
#' \dontrun{
#' conn <- ek_connect_postgres()
#' ek_read_signals_db(conn, as_of = "2025-01-31")
#' DBI::dbDisconnect(conn)
#' }
ek_read_signals_db <- function(conn, as_of = NULL,
                               table = "econkit_signals") {
  ek_check_db_connection(conn)
  table <- ek_db_table_name(table)
  if (!DBI::dbExistsTable(conn, table)) {
    stop("Signal table is not initialized; call `ek_init_signal_store_db()`.",
         call. = FALSE)
  }
  qtable <- as.character(DBI::dbQuoteIdentifier(conn, table))
  qcolumns <- as.character(DBI::dbQuoteIdentifier(conn, ek_signal_columns))
  sql <- paste0("SELECT ", paste(qcolumns, collapse = ", "), " FROM ", qtable)
  if (!is.null(as_of)) {
    as_of <- as.Date(as_of)
    if (length(as_of) != 1L || is.na(as_of)) {
      stop("`as_of` must be one valid date.", call. = FALSE)
    }
    sql <- paste0(
      sql, " WHERE ", as.character(DBI::dbQuoteIdentifier(conn, "as_of")),
      " <= ", as.character(DBI::dbQuoteLiteral(conn, as_of))
    )
  }
  stored <- ek_normalize_db_signals(DBI::dbGetQuery(conn, sql))
  ek_validate_signals(stored)
  if (!nrow(stored)) return(stored)
  stored <- stored[order(stored$as_of, stored$signal, stored$model_version), ,
                   drop = FALSE]
  latest <- !duplicated(stored$signal, fromLast = TRUE)
  out <- stored[latest, , drop = FALSE]
  rownames(out) <- NULL
  out
}

ek_check_db_connection <- function(conn) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for database-backed signal storage.",
         call. = FALSE)
  }
  if (!inherits(conn, "DBIConnection")) {
    stop("`conn` must be an open DBI connection.", call. = FALSE)
  }
}

ek_db_table_name <- function(table) {
  if (!is.character(table) || length(table) != 1L || is.na(table) ||
        !nzchar(table)) {
    stop("`table` must be one non-empty SQL table name.", call. = FALSE)
  }
  table
}

ek_normalize_db_signals <- function(signals) {
  if (!inherits(signals$as_of, "Date")) signals$as_of <- as.Date(signals$as_of)
  if (!inherits(signals$data_vintage, "Date")) {
    signals$data_vintage <- as.Date(signals$data_vintage)
  }
  signals$n_obs <- as.integer(signals$n_obs)
  signals[, ek_signal_columns, drop = FALSE]
}

ek_signal_rows_equal <- function(left, right, columns) {
  isTRUE(all.equal(
    left[, columns, drop = FALSE],
    right[, columns, drop = FALSE],
    check.attributes = FALSE
  ))
}
