test_that("DBI signal store is immutable, idempotent and supports as-of reads", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  old <- ek_build_signals(macro_monthly, as_of = as.Date("2018-12-01"))
  new <- ek_build_signals(macro_monthly, as_of = as.Date("2019-01-01"))
  expect_error(ek_read_signals_db(conn), "not initialized")
  expect_equal(ek_init_signal_store_db(conn), "econkit_signals")
  expect_equal(ek_store_signals_db(old, conn), nrow(old))
  expect_equal(ek_store_signals_db(old, conn), 0L)
  expect_equal(ek_store_signals_db(new, conn), nrow(new))

  current <- ek_read_signals_db(conn)
  prior <- ek_read_signals_db(conn, as_of = "2018-12-15")
  expect_true(all(current$as_of == as.Date("2019-01-01")))
  expect_true(all(prior$as_of == as.Date("2018-12-01")))
  expect_equal(
    current$change_vs_prior[current$signal == "term_spread"],
    current$value[current$signal == "term_spread"] -
      prior$value[prior$signal == "term_spread"]
  )

  changed <- old
  changed$value[changed$signal == "term_spread"] <- 999
  expect_error(ek_store_signals_db(changed, conn), "immutable")
})

test_that("PostgreSQL connection requires explicit non-secret settings", {
  vars <- c("ECONKIT_PGHOST", "ECONKIT_PGDATABASE", "ECONKIT_PGUSER")
  old <- Sys.getenv(vars, unset = NA_character_)
  on.exit({
    restore <- which(!is.na(old))
    if (length(restore)) {
      do.call(Sys.setenv, as.list(stats::setNames(old[restore], vars[restore])))
    }
    Sys.unsetenv(vars[is.na(old)])
  }, add = TRUE)
  Sys.unsetenv(vars)

  skip_if_not_installed("DBI")
  skip_if_not_installed("RPostgres")
  expect_error(ek_connect_postgres(), "Set PostgreSQL connection variables")
})
