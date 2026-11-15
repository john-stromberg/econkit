test_that("credit stress API accepts portfolio JSON and rejects invalid inputs", {
  skip_if_not_installed("plumber")
  api <- system.file("plumber", "api.R", package = "econkit")
  router <- plumber::pr(api)
  post <- function(body) {
    req <- new.env()
    req$REQUEST_METHOD <- "POST"
    req$PATH_INFO <- "/v1/banking/credit-stress"
    req$QUERY_STRING <- ""
    req$HTTP_CONTENT_TYPE <- "application/json"
    req$rook.input <- list(
      read = function() charToRaw(body),
      rewind = function() invisible(NULL),
      read_lines = function() body
    )
    router$call(req)
  }
  decode <- function(response) {
    body <- response$body
    if (!is.raw(body)) body <- charToRaw(paste(body, collapse = ""))
    plumber::parser_json()(body, "application/json")
  }
  valid <- paste0(
    '{"portfolio":[{"segment":"CRE","ead":1000000,',
    '"pd_12m":0.025,"lgd":0.4,"unemployment_beta":0.35},',
    '{"segment":"consumer","ead":500000,"pd_12m":0.04,',
    '"lgd":0.6,"unemployment_beta":0.45}]}'
  )
  result <- post(valid)
  expect_equal(result$status, 200L)
  parsed <- decode(result)
  expect_equal(nrow(parsed$summary), 3L)
  expect_equal(nrow(parsed$segments), 6L)
  expect_equal(parsed$summary$expected_loss[parsed$summary$scenario_id == "base"], 22000)
  expect_identical(parsed$portfolio_source, "provided")

  invalid_segment <- paste0(
    '{"portfolio":[{"segment":1,"ead":1000000,',
    '"pd_12m":0.025,"lgd":0.4,"unemployment_beta":0.35}]}'
  )
  result <- post(invalid_segment)
  expect_equal(result$status, 400L)
  expect_match(decode(result)$error, "portfolio array")

  invalid_pd <- paste0(
    '{"portfolio":[{"segment":"CRE","ead":1000000,',
    '"pd_12m":1,"lgd":0.4,"unemployment_beta":0.35}]}'
  )
  result <- post(invalid_pd)
  expect_equal(result$status, 400L)
  expect_match(decode(result)$error, "PDs")

  unexpected_field <- sub("^\\{", "{\"scenario\":\"custom\",", valid)
  result <- post(unexpected_field)
  expect_equal(result$status, 400L)
  expect_match(decode(result)$error, "portfolio array")
})
