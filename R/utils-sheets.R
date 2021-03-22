#' Get the `sheet_id` of database entities on Google Sheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sheet_id()` retrieve the `sheet_id` from entities tables on Google Sheets.
#'
#' @param (optional) name A string indicating the sheet name (default: `NULL`).
#'
#' @return If `name = NULL`, returns a `character` object with all sheet names
#'   available. Else, it returns the `sheet_id` object of the sheet indicated in
#'   `name`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' ## __ To list all the names of all sheets available __
#' sheet_id()
#'
#' ## __ To get the 'sheet_id' object of a specific sheet __
#' sheet_id(sheet_id()[1])}
sheet_id <- function(name = NULL) {
    checkmate::assert_string(name, null.ok = TRUE)

    sheets <- list(
        domain = "16B-JH-3QiqPPdFIiYHssKAZKFe8ehYtYVuFqFH8MnrU",
        constraint = "1Ikw73ifT09CoPaWOc7avugybEsbQ7HIkO_Fn7R0WSrU",
        keyword = "1Re3lLhjbpBmYBVCMdt37-ZwAOKmWnPdMEJD8J7Sc86k",
        source = "1Ef0wzRMjTcibYwWQ0T0Ap2s3c07A-DiGqCSduUSxlMY",
        search = "1yZCRt3G94JS73T55rGCxdgUHrKsM6QX6RHSeRUxe3J4"
    )

    if (is.null(name)) {
        names(sheets)
    } else {
        checkmate::assert_choice(name, names(sheets))
        googlesheets4::as_sheets_id(sheets[[name]])
    }
}

#' Read sheets of database entities on Google Sheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `read_sheet()` read and returns entities tables on Google Sheets.
#'
#' @param name (optional) A string indicating the sheet name.
#'
#' @return If `name = NULL`, returns an invisible `list` object with `tibbles`
#'   objects of all sheet tables available as elements. Else, it returns an
#'   invisible `tibble` object of the sheet table indicated in `name`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' ## __ To get a 'list' with all the sheets __
#' read_sheet()
#'
#' ## __ To get only only a specific sheet __
#' read_sheet(sheet_id()[1])}
read_sheet <- function(name = NULL) {
    sheets <- list(
        domain = list(name = "domain", sheet = "Dataset"),
        constraint = list(name = "constraint", sheet = "Dataset"),
        keyword = list(name = "keyword", sheet = "Dataset"),
        source = list(name = "source", sheet = "Dataset"),
        search = list(name = "search", sheet = "Dataset")
    )

    checkmate::assert_subset(name, names(sheets), empty.ok = TRUE)

    if (!is.null(name)) sheets <- sheets[name]

    for (i in sheets) {
        data <- googlesheets4::read_sheet(sheet_id(i$name), sheet = i$sheet)
        data <- data[which(!is.na(data[[1]])), ]
        # data[paste0(i$name, "_id")] <- seq(nrow(data))
        # data <- data %>%
        #     dplyr::mutate(dplyr::across(dplyr::ends_with("_id"), as.integer))

        assign(i$name, data)
    }

    if ("keyword" %in% ls()) {
        keyword <- keyword %>%
            dplyr::arrange(domain_id, language, dplyr::desc(approved), keyword)
    }

    if (!is.null(name)) {
        invisible(get(name))
    } else {
        variables <- mget(ls())
        invisible(variables[names(sheets)])
    }
}

#' Write the sheets of database entities on Google Sheets to the package
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `write_sheet` reads and writes entities tables on Google Sheets in the
#' data directory of the `sqlr.package`.
#'
#' @param name (optional) A string indicating the sheet name.
#' @param load_all (optional) A `logical` value indicating if the function must
#'   call `devtools::load_all()` function to reload the package to memory.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' ## __ To write all sheets __
#' write_sheet()
#'
#' ## __ To write only a specific sheet __
#' write_sheet(sheet_id()[1])}
write_sheet <- function(name = NULL, load_all = TRUE) {
    sheets <- sheet_id()
    checkmate::assert_subset(name, sheets, empty.ok = TRUE)
    checkmate::assert_flag(load_all)

    if (!(dir.exists("./data/"))) dir.create("./data/")

    if (!is.null(name)) {
        assign(name, read_sheet(name))
        sheets <- name
    } else {
        list2env(read_sheet())
    }

    for (i in sheets) {
        file <- paste0("./data/", i, ".rda")
        save(list = i, file = file, envir = environment(),
             compress = "bzip2", version = 2)
    }

    if (isTRUE(load_all)) devtools::load_all()

    invisible(NULL)
}
