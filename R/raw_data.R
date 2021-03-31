#' Get paths to `sqlr` raw data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `raw_data()` returns the raw data paths of the `sqlr` package.
#'
#' @param type (optional) a string indicating the file type of the raw dataset
#'   (default: `NULL`).
#' @param file (optional) a string indicating the file name of the raw dataset.
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return
#'
#' * If `type = NULL`, a `character` object with all file type names
#'   available.
#' * If `type != NULL` and `file = NULL`, a `character` object with all file
#' names available from type.
#' * If `type != NULL` and `file != NULL`, a string file name path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data()
#' }
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
        dir(system.file("extdata", package = package))
    } else if (is.null(file) && !is.null(type)) {
        dir(system.file(index[[type]]$path, package = package))
    } else if (!is.null(file) && !is.null(type)) {
        system.file(index[[type]]$path, file, package = package,
                    mustWork = TRUE)
    } else if (!is.null(file) & is.null(type)) {
        stop("When 'file' is assigned, the 'type' argument cannot be 'NULL'",
             call. = FALSE)
    }
}
