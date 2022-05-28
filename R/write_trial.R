#' Write a trial table to a Google Spreadsheets
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
#' You must have a `sheets` data object with the sheets metadata and an updated
#' `reference` table before running this function. See [write_metadata()] and
#' [update_reference()] to learn more.
#'
#' @param trial_id A string indicating the ID of the trial to be written.
#'
#' @family reference/citation functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' ## Writing the Trial - DUP table
#'
#' write_trial("dup")
#'
#' ## Checking the number of rows with the 'reference' table
#'
#' nrow(dplyr::filter(reference, !is.na(criteria_id)) +
#' sheet_nrow("trial_nr1") == nrow(reference)}
write_trial <- function(trial_id, package = gutils:::get_package_name()) {
    pattern <- "^[a-zA-Z0-9]{3}$|^[a-zA-Z0-9]{3}-[a-zA-Z0-9]{3}$"

    checkmate::assert_string(trial_id, pattern = pattern)
    checkmate::assert_string(package, null.ok = TRUE)
    gutils:::assert_interactive()
    gutils:::require_pkg("utils", "googlesheets4")
    gutils:::assert_namespace(package)

    # R CMD Check variable bindings fix
    sheets <- reference <- trial <- criteria_id <- where <- NULL

    googlesheets4::gs4_auth()

    gutils:::assert_data("sheets", package)
    utils::data("sheets", package = package, envir = environment())

    gutils:::assert_data("trial", package)
    utils::data("trial", package = package, envir = environment())

    trial_name <- paste0("trial_", tolower(trial_id))

    if (!trial_name %in% names(sheets)) {
        cli::cli_abort(paste0(
            "{cli::col_red(trial_name)} was not found in the ",
            "{cli::col_blue('sheets')} table."))
    }

    if (!tolower(trial_id) %in% tolower(trial$trial_id)) {
        cli::cli_abort(paste0(
            "{cli::col_red(toupper(trial_id))} was not found in the ",
            "{cli::col_blue('trial')} table."))
    }

    trial_index <- which(trial$trial_id == toupper(trial_id))

    if (!trial_index == 1 && isFALSE(trial$approval[trial_index - 1])) {
        trial_x <- toupper(trial$trial_id[trial_index - 1])

        cli::cli_abort(paste0(
            "The {cli::col_red(trial_x)} trial_id, i.e., the trial_id ",
            "that comes before {cli::col_red(trial_id)} is not ",
            "approved in the {cli::col_grey('trial')} table. ",
            "This trial must be approved and the ",
            "{cli::col_grey('reference')} table must be updated ",
            "before running {cli::col_blue('write_trial()')}."))
    }

    gutils:::assert_data("reference", package)
    utils::data("reference", package = package, envir = environment())

    cols <- c("reference_id", "criteria_id", "trial_id", "pdf", "type",
              "doi", "pmid", "author", "year", "title", "abstract", "keyword",
              "journal", "place_published", "volume", "issue", "start_page",
              "end_page", "publisher", "standard_number", "length")

    reference <- reference %>%
        dplyr::select(
            dplyr::all_of(cols[which(cols %in% names(reference))]))

    if (!tolower(trial_id) == "dup") {
        trial_id_ <- toupper(trial_id)

        reference <- reference %>%
            dplyr::filter(is.na(criteria_id)) %>%
            dplyr::mutate(trial_id = trial_id_)
    }

    if (nrow(reference) == 0) {
        cli::cli_abort(paste0(
            "The {cli::col_green('reference')} ",
            "dataset ended with no rows after the cleaning process."))
    }

    range_write(reference, name = trial_name, package = package)

    invisible(NULL)
}

#' Update the `reference` table using a trial table
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' SQLR system.
#'
#' `update_reference()` updates the `criteria_id`, `trial_id`, and `pdf` columns
#' of the `reference` table using the data available in a trial table.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See [write_metadata()] to learn more.
#'
#' @param trial_id A string indicating the ID of the trial to be written.
#' @param write (optional) a `logical` value indicating if the function must
#'   update the `reference` data table of the package and to write the new
#'   reference table to the reference spreadsheet listed on the `sqlr::sheets`
#'   object (default: `TRUE`).
#'
#' @family reference/citation functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' update_reference("NR1")}
update_reference <- function(trial_id, package = gutils:::get_package_name(),
                             write = TRUE) {
    pattern <- "^[a-zA-Z0-9]{3}$|^[a-zA-Z0-9]{3}-[a-zA-Z0-9]{3}$"

    checkmate::assert_string(trial_id, pattern = pattern)
    checkmate::assert_string(package, null.ok = TRUE)
    gutils:::require_pkg("utils", "googlesheets4")
    gutils:::assert_namespace(package)

    # R CMD Check variable bindings fix
    sheets <- reference <- trial <- NULL
    reference_id <- criteria_id <- pdf <- NULL

    gutils:::assert_data("sheets", package)
    utils::data("sheets", package = package, envir = environment())

    gutils:::assert_data("reference", package)
    utils::data("reference", package = package, envir = environment())

    gutils:::assert_data("trial", package)
    utils::data("trial", package = package, envir = environment())

    if (isTRUE(write)) {
        gutils:::assert_interactive()

        googlesheets4::gs4_auth()
    }

    trial_name <- paste0("trial_", tolower(trial_id))

    if (!trial_name %in% names(sheets)) {
        cli::cli_abort(paste0(
            "{cli::col_red(trial_name)} was not found in the ",
            "{cli::col_blue('sheets')} table."))
    }

    if (!tolower(trial_id) %in% tolower(trial$trial_id)) {
        cli::cli_abort(paste0(
            "{cli::col_red(toupper(trial_id))} was not found in the ",
            "{cli::col_blue('trial')} table."))
    }

    trial_data <- read_sheet(trial_name, package = package) %>%
        dplyr::select(reference_id, criteria_id, trial_id, pdf)

    out <- reference %>%
        dplyr::left_join(trial_data, by = "reference_id")

    for (i in c("trial_id", "criteria_id", "pdf")) {
        index <- out[[paste0(i, ".x")]]

        if (i == "trial_id") {
            out <- out %>% dplyr::mutate(
                !!as.symbol(paste0(i, ".x")) :=
                    dplyr::case_when(
                        is.na(!!as.symbol(paste0(i, ".x"))) &
                            is.na(criteria_id.y) ~
                            as.character(NA),
                        TRUE ~ dplyr::coalesce(
                            !!as.symbol(paste0(i, ".y")),
                            !!as.symbol(paste0(i, ".x")))))
        } else {
            out <- out %>%
                dplyr::mutate(!!as.symbol(paste0(i, ".x")) :=
                                  dplyr::coalesce(!!as.symbol(paste0(i, ".y")),
                                                  !!as.symbol(paste0(i, ".x"))))
        }

        out <- out %>%
            dplyr::rename_with(.cols = paste0(i, ".x"), .fn = ~ i) %>%
            dplyr::select(-!!as.symbol(paste0(i, ".y")))

        if (identical(index, out[[i]])) {
            cli::cli_alert_warning(paste0(
                "{.strong {cli::col_blue(i)}} was ",
                "{.strong {cli::col_red('not')}} updated."))
        } else {
            changes <- length(gutils:::rm_na(out[[i]])) -
                length(gutils:::rm_na(index))

            cli::cli_alert_success(paste0(
                "{.strong {cli::col_blue(i)}} was updated. ",
                "{.strong {cli::col_green(changes)}} records added."))
        }
    }

    if (isTRUE(write)) write_reference(out)

    invisible(out)
}
