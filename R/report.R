#' Render the macro dashboard
#'
#' Renders the parameterised Quarto macro dashboard, which covers recession
#' probability, the yield curve, the Sahm rule, regimes, the output gap and
#' financial conditions. Requires the Quarto CLI and the `quarto`, `ggplot2`
#' and `gt` packages.
#'
#' @param output_file Path of the HTML file to create.
#' @param source Data source passed to [ek_get_series()]: `"sample"` or
#'   `"fred"`.
#' @param start Start date for charts.
#' @param horizon Recession forecast horizon in months.
#' @return The path to the rendered file, invisibly.
#' @export
#' @examples
#' \dontrun{
#' ek_render_macro_dashboard("macro-dashboard.html")
#' }
ek_render_macro_dashboard <- function(output_file = "macro-dashboard.html",
                                      source = c("sample", "fred"),
                                      start = "1990-01-01", horizon = 12L) {
  source <- match.arg(source)
  for (pkg in c("quarto", "ggplot2", "gt")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required to render reports.", call. = FALSE)
    }
  }
  template <- system.file("templates", "macro-dashboard.qmd", package = "econkit",
                          mustWork = TRUE)
  work <- tempfile("econkit-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  input <- file.path(work, "macro-dashboard.qmd")
  file.copy(template, input)
  quarto::quarto_render(
    input,
    execute_params = list(source = source, start = start, horizon = horizon),
    quiet = TRUE
  )
  output_file <- normalizePath(output_file, mustWork = FALSE)
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(work, "macro-dashboard.html"), output_file, overwrite = TRUE)
  invisible(output_file)
}
