#' Get the `sheet_id` of database entities on Google Sheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sheet_id()` retrieve the `sheet_id` from entities tables on Google Sheets.
#'
#' @param name (optional) A string indicating the sheet name (default: `NULL`).
#' @param package (optional) A string indicating the package where the sheets
#'   list can be found. If `NULL`, the function will try to use the name of the
#'   project active directory (requires the `rstudioapi` package) (default:
#'   `NULL`).
#'
#' @return If `name = NULL`, returns a `character` object with all sheet names
#'   available. Else, it returns the `sheet_id` object of the sheet indicated in
#'   `name`.
#'
#' @family GIPSO functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To list all the all sheet names available __
#' sheet_id()
#'
#' ## __ To get the 'sheet_id' object of a specific sheet __
#' sheet_id(sheet_id()[1])}
sheet_id <- function(name = NULL, package = NULL) {
    sheets <- NULL # R CMD Check variable bindings fix

    checkmate::assert_string(name, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (!is_namespace_loaded("utils") ||
        !is_namespace_loaded("googlesheets4")) {
        stop("This function requires the 'utils' and 'googlesheets4' packages ",
             'to run. You can install them by running: \n \n',
             'install.packages("utils") \n',
             'install.packages("googlesheets4")' , call. = FALSE)
    }

    if (is.null(package)) {
        package <- stringr::str_extract(
            rstudioapi::getActiveProject(), "[a-zA-Z0-9.]*$")
    }

    utils::data("sheets", package = package, envir = environment())

    checkmate::assert_subset(name, names(sheets), empty.ok = TRUE)

    if (is.null(name)) {
        names(sheets)
    } else {
        checkmate::assert_choice(name, names(sheets))
        googlesheets4::as_sheets_id(sheets[[name]][["id"]])
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
#' @return If `name = NULL`, returns an invisible `list` object with `tibbles`
#'   objects of all sheet tables available as elements. Else, it returns an
#'   invisible `tibble` object of the sheet table indicated in `name`.
#'
#' @family GIPSO functions
#' @inheritParams sheet_id
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To get a 'list' with all the sheets __
#' read_sheet()
#'
#' ## __ To get only only a specific sheet __
#' read_sheet(sheet_id()[1])}
read_sheet <- function(name = NULL, package = NULL) {
    checkmate::assert_string(name, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (!is_namespace_loaded("utils") ||
        !is_namespace_loaded("googlesheets4")) {
        stop("This function requires the 'utils' and 'googlesheets4' packages ",
             'to run. You can install them by running: \n \n',
             'install.packages("utils") \n',
             'install.packages("googlesheets4")' , call. = FALSE)
    }

    if (is.null(package)) {
        package <- stringr::str_extract(
            rstudioapi::getActiveProject(), "[a-zA-Z0-9.]*$")
    }

    utils::data("sheets", package = package, envir = environment())

    checkmate::assert_subset(name, names(sheets), empty.ok = TRUE)

    if (!is.null(name)) sheets <- sheets[name]

    for (i in sheets) {
        data <- googlesheets4::read_sheet(
            sheet_id(name = i$name, package = package),
            sheet = i$sheet)
        data <- data[which(!is.na(data[[1]])), ]
        # data[paste0(i$name, "_id")] <- seq(nrow(data))
        # data <- data %>%
        #     dplyr::mutate(dplyr::across(dplyr::ends_with("_id"), as.integer))

        assign(i$name, data)
    }

    if ("keyword" %in% ls()) {
        keyword <- keyword %>%
            dplyr::arrange(.data$domain_id, .data$language,
                           dplyr::desc(.data$approved), .data$keyword)
    }

    if (!is.null(name)) {
        invisible(get(name))
    } else {
        export <- stringr::str_subset(ls(), "name", negate = TRUE)
        variables <- mget(export)
        invisible(variables[names(sheets)])
    }
}

#' Write sheets of database entities on Google Sheets to the package
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `write_sheet` reads entities tables on Google Sheets and write them in the
#' data directory of an R package.
#'
#' @family GIPSO functions
#' @inheritParams sheet_id
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To write all sheets __
#' write_sheet()
#'
#' ## __ To write only a specific sheet __
#' write_sheet(sheet_id()[1])}
write_sheet <- function(name = NULL, package = NULL) {
    checkmate::assert_string(name, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (!is_namespace_loaded("utils") ||
        !is_namespace_loaded("googlesheets4")) {
        stop("This function requires the 'utils' and 'googlesheets4' packages ",
             'to run. You can install them by running: \n \n',
             'install.packages("utils") \n',
             'install.packages("googlesheets4")' , call. = FALSE)
    }

    if (is.null(package)) {
        package <- stringr::str_extract(
            rstudioapi::getActiveProject(), "[a-zA-Z0-9.]*$")
    }

    dialog <- dialog_line(
        "WARNING: You need to configure the 'sheets' list before using",
        "'write_sheet()'. Press 'esc' to exit or 'enter' to continue >",
        space_above = FALSE, space_below = FALSE)

    sheets <- sheet_id(package = package)
    checkmate::assert_subset(name, sheets, empty.ok = TRUE)

    if (!(dir.exists("./data/"))) dir.create("./data/")

    if (!is.null(name)) {
        assign(name, read_sheet(name = name, package = package))
        sheets <- name
    } else {
        list2env(read_sheet(package = package))
    }

    for (i in sheets) {
        if(!(dir.exists("./data/"))) dir.create("./data/")

        file <- paste0("./data/", i, ".rda")
        save(list = i, file = file, envir = environment(),
             compress = "bzip2", version = 2)
    }

    invisible(NULL)
}
