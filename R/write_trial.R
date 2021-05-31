#' Write the `trial_*` table to a Google Spreadsheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' SQLR system.
#'
#' `write_trial()` writes a `trial_*` table of the Systematic Quantitative
#' Literature Review (SQLR) system to a Google Spreadsheets.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See `?write_metadata()` to learn more.
#'
#' @param trial_id A string indicating the ID of the trial to be written.
#'
#' @family SQLR system functions
#' @template param_a
#' @template param_b
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ Writing the Trial - DUP table __
#' write_trial("dup")
#'
#' ## __ Checking the number of rows with the 'reference' table __
#' nrow(dplyr::filter(reference, criteria_id == "DUP")) +
#' sheet_nrow("trial_nr1") == nrow(reference)}
write_trial <- function(trial_id, package = NULL, quiet = FALSE) {
    pattern <- "^[a-zA-Z0-9]{3}$|^[a-zA-Z0-9]{3}-[a-zA-Z0-9]{3}$"

    checkmate::assert_string(trial_id, pattern = pattern)
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(quiet)
    assert_interactive()
    require_pkg("utils", "googlesheets4")

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)

    # R CMD Check variable bindings fix
    sheets <- reference <- trial <- criteria_id <- where <- NULL

    assert_data("sheets", package, alert = "gipso_1")
    utils::data("sheets", package = package, envir = environment())

    assert_data("trial", package)
    utils::data("trial", package = package, envir = environment())

    trial_name <- paste0("trial_", tolower(trial_id))

    if (!trial_name %in% names(sheets)) {
        stop(single_quote_(trial_name), " was not found in the 'sheets' ",
             "table.", call. = FALSE)
    }

    if (!tolower(trial_id) %in% tolower(trial$trial_id)) {
        stop(single_quote_(trial_id), " was not found in the 'trial' ",
             "table.", call. = FALSE)
    }

    trial_index <- which(trial$trial_id == toupper(trial_id))

    if (!trial_index == 1 && isFALSE(trial$approval[trial_index - 1])) {
        trial_x <- toupper(trial$trial_id[trial_index - 1])

        stop("The ", single_quote_(trial_x), " trial_id, i.e., the trial_id ",
             "that comes before ", single_quote_(toupper(trial_id)), " is not ",
             "approved in the 'trial' table. This trial must be approved ",
             "and the 'reference' table must be updated before running ",
             "'write_trial()'.", call. = FALSE)
    }

    assert_data("reference", package)
    utils::data("reference", package = package, envir = environment())

    str_subset <- function(x) {
        dplyr::case_when(
            nchar(x) >= 50000 ~ stringr::str_sub(x, 1, 49999),
            TRUE ~ x
        )
    }

    cols <- c("reference_id", "criteria_id", "trial_id", "pdf", "type",
              "doi", "pmid", "author", "year", "title", "abstract", "keyword",
              "journal", "place_published", "volume", "issue", "start_page",
              "end_page")

    reference <- reference %>%
        dplyr::select(
            dplyr::all_of(cols[which(cols %in% names(reference))])) %>%
        dplyr::mutate(dplyr::across(where(is.character), str_subset))

    if (!tolower(trial_id) == "dup") {
        trial_id_ <- toupper(trial_id)

        reference <- reference %>%
            dplyr::filter(is.na(criteria_id)) %>%
            dplyr::mutate(trial_id = trial_id_)
    }

    if (nrow(reference) == 0) {
        stop("The 'reference' dataset ended with no rows after the cleaning",
             "process.")
    }

    range_write(reference, name = trial_name, package = package, quiet = quiet)

    invisible(NULL)
}
