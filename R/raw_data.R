#' Get paths to `sqlr.pregnancy` raw datasets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `raw_data()` returns the raw data paths from the `sqlr.pregnancy` package.
#'
#' @param file A string indicating the file name of the raw dataset. If `NULL`,
#'   all raw dataset file names will be printed (default: `NULL`).
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
raw_data <- function(file = NULL) {
    checkmate::assert_string(file, null.ok = TRUE)

    if (is.null(file)) {
        dir(system.file("extdata/citation", package = "sqlr.pregnancy"))
    } else {
        system.file("extdata/citation", file, package = "sqlr.pregnancy",
                    mustWork = TRUE)
    }
}
