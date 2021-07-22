#' Write the `reference` table to the package and to Google Spreadsheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' SQLR system.
#'
#' `write_reference()` writes a `data.frame` to the reference data file of
#' the package an to its Google Spreadsheets file.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See `?write_metadata()` to learn more.
#'
#' @param data The `data.frame` object to write.
#'
#' @family SQLR system functions
#' @template param_a
#' @template param_b
#' @export
#'
#' @examples
#' \dontrun{
#' write_reference(data.frame(a = 1, b = 1))}
write_reference <- function(data, package = NULL, quiet = FALSE) {
    checkmate::assert_data_frame(data, min.rows = 1)
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(quiet)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)

    assert_data("sheets", package, alert = "gipso_1")
    utils::data("sheets", package = package, envir = environment())
    checkmate::assert_subset("reference", names(sheets))

    # R CMD Check variable bindings fix
    sheets <- where <- doi <- pmid <- year <- NULL

    if(!(dir.exists("./data/"))) dir.create("./data/")
    file <- paste0("./data/", "reference", ".rda")
    reference <- data

    shush(cli::cli_alert("Writing the 'reference' table to the package."),
          quiet = quiet)

    save(reference, file = file, envir = environment(), compress = "bzip2",
         version = 2)
    rm(reference)

    shush(cli::cli_alert(
        "Writing the 'reference' table to Google Spreadsheets."), quiet = quiet)

    range_write(data, name = "reference", package = package, quiet = quiet)

    cli::cli_alert_info(paste0(
        "{.strong {cli::col_red('Run (in order)')}}:\n\n",
        "{.strong devtools::document() [Ctrl + Shift  + D]\n",
        "devtools::load_all() [Ctrl + Shift  + L]}"))

    invisible(NULL)
}
