#' Start settings for SQLR R package templates
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `start_sqlr()` handles the initial commands to start Systematic Quantitative
#' Literature Reviews R packages.
#'
#' @param id A string with the Google Sheets ID from the 'Sheets' table.
#' @param sheet (optional) a string indicating the worksheet/tab where the
#'   sheets data can be found on the 'Sheets spreadsheet (default: `"Dataset"`).
#'
#' @family SQLR system functions
#' @template param_a
#' @export
start_sqlr <- function(id, sheet = "Dataset", package = NULL) {
    checkmate::assert_string(id)
    checkmate::assert_string(sheet)
    checkmate::assert_string(package, null.ok = TRUE)

    shush(normalize_extdata())
    shush(write_metadata(id, sheet))
    shush(write_sheet(package = package))

    message("\n", "Run (in order):\n\n",
            "devtools::document()\n",
            "devtools::load_all()")
}
