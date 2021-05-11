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
#' @param file (optional) a `character` object indicating the file name(s) of
#'   the raw dataset.
#' @param package (optional) a string indicating the package with the database
#'   data. If `NULL`, the function will try to use the name of the active
#'   project directory (requires the `rstudioapi` package) (default: `sqlr`).
#'
#' @return
#'
#' * If `type = NULL`, a `character` object with all file type names
#'   available.
#' * If `type != NULL` and `file = NULL`, a `character` object with all file
#' names available from type.
#' * If `type != NULL` and `file != NULL`, a string with the file name path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data()
#' }
raw_data <- function(type = NULL, file = NULL, package = "sqlr") {
    index <- list(
        reference = list(name = "Reference",
                         path = "inst/extdata/reference"),
        search_history = list(name = "Search history",
                              path = "inst/extdata/search_history")
    )

    checkmate::assert_character(file, min.len = 1, null.ok = TRUE)
    checkmate::assert_choice(type, names(index), null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)

    if (is.null(file) && is.null(type)) {
        list.files(system.file("inst/extdata", package = package))
    } else if (is.null(file) && !is.null(type)) {
        list.files(system.file(index[[type]]$path, package = package))
    } else if (!is.null(file) && !is.null(type)) {
        system.file(index[[type]]$path, file, package = package,
                    mustWork = TRUE)
    } else if (!is.null(file) & is.null(type)) {
        stop("When 'file' is assigned, the 'type' argument cannot be 'NULL'",
             call. = FALSE)
    }
}
