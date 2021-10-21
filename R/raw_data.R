#' Get paths to `sqlr` raw data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `raw_data()` returns the raw data paths of the `sqlr` package.
#'
#' @param type (optional) a string indicating the file type of the raw data
#'   (default: `NULL`).
#' @param file (optional) a `character` object indicating the file name(s) of
#'   the raw data (default: `NULL`).
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
#' * If `type != NULL` and `file != NULL`, a `character` with the file name(s)
#' path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' raw_data()
#' }
raw_data <- function(type = NULL, file = NULL, package = "sqlr") {
    choices <- c("reference", "search_history")

    checkmate::assert_character(file, min.len = 1, null.ok = TRUE)
    checkmate::assert_choice(type, choices, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)

    if (is.null(file) && is.null(type)) {
        list.files(find_path("extdata", package))
    } else if (is.null(file) && !is.null(type)) {
        list.files(file.path(find_path("extdata", package), type))
    } else if (!is.null(file) && !is.null(type)) {
        out <- file.path(find_path("extdata", package), type, file)
        checkmate::assert_file_exists(out)
        out
    } else if (!is.null(file) & is.null(type)) {
        cli::cli_abort(paste0(
            "When {cli::col_blue('file')} is assigned the ",
            "{cli::col_red('type')} argument cannot be ",
            "{cli::col_silver('NULL')}."
        ))
    }
}
