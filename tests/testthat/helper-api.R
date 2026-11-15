ek_test_router <- function() {
  plumber::pr(system.file("plumber", "api.R", package = "econkit"))
}

# Calls a route in memory: GET without a body, POST with a JSON body.
ek_test_call <- function(router, path, query = "", body = NULL, key = NULL) {
  req <- new.env()
  req$REQUEST_METHOD <- if (is.null(body)) "GET" else "POST"
  req$PATH_INFO <- path
  req$QUERY_STRING <- query
  if (!is.null(key)) req$HTTP_X_API_KEY <- key
  if (!is.null(body)) req$HTTP_CONTENT_TYPE <- "application/json"
  payload <- if (is.null(body)) raw(0) else charToRaw(body)
  req$rook.input <- list(
    read = function() payload,
    rewind = function() invisible(NULL),
    read_lines = function() if (is.null(body)) character() else body
  )
  response <- router$call(req)
  raw_body <- response$body
  if (!is.raw(raw_body)) raw_body <- charToRaw(paste(raw_body, collapse = ""))
  content_type <- response$headers[["Content-Type"]]
  json <- is.null(content_type) || grepl("json", content_type)
  list(status = response$status, headers = response$headers, raw = raw_body,
       body = if (json) plumber::parser_json()(raw_body, "application/json"))
}
