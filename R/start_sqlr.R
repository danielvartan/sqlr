#' Start settings for SQLR R package templates
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `start_sqlr()` handles the initial commands to start Systematic Quantitative
#' Literature Reviws R packages.
#'
#' @param id A string with the Google Sheets ID from the 'Sheets' table.
#' @param sheet (optional) a string indicating the worksheet/tab where the
#'   sheets data can be found on the 'Sheets spreadsheet (default: `"Dataset"`).
#'
#' @family data functions
#' @template param_a
#' @export
start_sqlr <- function(id, sheet = "Dataset", package = NULL) {
    checkmate::assert_string(id)
    checkmate::assert_string(sheet)
    checkmate::assert_string(package, null.ok = TRUE)

    if (!is_namespace_loaded("devtools")) {
        stop("This function requires the 'devtools' package to run. ",
             "You can install it by running: \n\n",
             'install.packages("devtools")', call. = FALSE)
    }

    shush(write_metadata(id, sheet))
    shush(write_sheet(package = package))

    devtools::document()
    devtools::load_all()

    usethis::use_github_action_check_standard()
    usethis::use_travis()
    usethis::use_pkgdown_github_pages()
    usethis::use_coverage()

    message("\n", "Run (in order):\n\n",
            "'devtools::document()' (Ctrl + Shift + D)\n",
            "'devtools::load_all()' (Ctrl + Shift + L)")
}
