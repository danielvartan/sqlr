#' Write the `reference` table to the package and to Google Spreadsheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' `sqlr` system.
#'
#' `write_reference()` writes a `data.frame` to the reference data file of
#' the package an to its Google Spreadsheets file.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See [write_metadata()] to learn more.
#'
#' @param data The `data.frame` object to write.
#'
#' @family reference/citation functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' write_reference(data.frame(a = 1, b = 1))}
write_reference <- function(data, package = rutils:::get_package_name()) {
    checkmate::assert_data_frame(data, min.rows = 1)
    checkmate::assert_string(package, null.ok = TRUE)
    rutils:::assert_namespace(package)
    rutils:::assert_interactive()

    rutils:::assert_data("sheets", package)
    utils::data("sheets", package = package, envir = environment())
    checkmate::assert_subset("reference", names(sheets))

    # R CMD Check variable bindings fix
    # nolint start: object_usage_linter.
    sheets <- where <- doi <- pmid <- year <- NULL
    # nolint end

    googlesheets4::gs4_auth()

    if (!(dir.exists("./data/"))) dir.create("./data/")
    file <- paste0("./data/", "reference", ".rda")
    reference <- data

    cli::cli_alert_info(paste0(
        "Writing the {.strong {cli::col_blue('reference')}} table to ",
        "the package."
        ))

    save(reference, file = file, envir = environment(), compress = "bzip2",
         version = 2)
    rm(reference)

    cli::cli_alert_info(paste0(
        "Writing the {.strong {cli::col_blue('reference')}} table to ",
        "Google Spreadsheets."
    ))

    range_write(data, name = "reference", package = package)

    cli::cat_line()
    cli::cli_alert_info(paste0(
        "{.strong {cli::col_red('Run (in order)')}}:\n\n",
        "{.strong devtools::document() [Ctrl + Shift  + D]\n",
        "devtools::load_all() [Ctrl + Shift  + L]}"))

    invisible(NULL)
}
