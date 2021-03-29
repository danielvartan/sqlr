#' Get paths to `sqlr` raw data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `raw_data()` returns the raw data paths from the `sqlr` package.
#'
#' @param type (optional) a string indicating the type of file of the raw dataset.
#'   If `NULL`, all raw dataset file names will be printed (default: `NULL`).
#' @param file (optional) a string indicating the file name of the raw dataset.
#'   If `NULL`, all raw dataset file names will be printed (default: `NULL`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return If `path = NULL`, returns a `character` object with all raw dataset
#'   file names available. Else, it returns the `file` path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To list all raw dataset file names available __
#' raw_data()
#'
#' ## __ To get the file path from a specific raw dataset __
#' raw_data(raw_data()[1])}
raw_data <- function(type = NULL, file = NULL, quiet = FALSE) {
    index <- list(
        citation = list(name = "Citation",
                        path = "extdata/citation"),
        search_history = list(name = "Search history",
                              path = "extdata/search_history")
    )

    checkmate::assert_string(file, null.ok = TRUE)
    checkmate::assert_choice(type, names(index), null.ok = TRUE)
    checkmate::assert_flag(quiet)

    package <- "sqlr"

    if (is.null(file) && is.null(type)) {
        shush(alert("File types:\n"), quiet)
        dir(system.file("extdata", package = package))
    } else if (is.null(file) && !is.null(type)) {
        shush(alert(index[[type]]$name, " files:", "\n"), quiet)
        dir(system.file(index[[type]]$path, package = package))
    } else if (!is.null(file) && !is.null(type)) {
        system.file(index[[type]]$path, file, package = package,
                    mustWork = TRUE)
    } else if (!is.null(file) & is.null(type)) {
        stop("When 'file' is assigned, the 'type' argument cannot be 'NULL'",
             call. = FALSE)
    }
}
