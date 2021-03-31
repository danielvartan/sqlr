#' Create a list with Google Sheets metadata for tables hosted on the platform
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `write_metadata()` creates and returns a `list` object containing lists with
#' the Google Sheets metadata of the review tables hosted on the platform.
#'
#' This must be set for all R packages from GIPSO for systematic quantitative
#' literature reviews.
#'
#' You can see a example of a metadata sheets in: <https://bit.ly/2PFWhev>.
#'
#' @param id A string with the Google Sheets ID from the sheets table.
#' @param sheet (optional) a string indicating the worksheet/tab where the
#'   sheets data can be found on the sheets spreadsheet (default: `"Dataset"`).
#'
#' @return An invisible `list` object containing lists with the Google Sheets
#'   metadata of the review tables hosted on the platform.
#'
#' @family data functions
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' write_metadata("1x6Aj8cXl9qFtpXq48Q6zdmT-w9EEqNG1iVQcKVRhgKM")
#' }
write_metadata <- function(id, sheet = "Dataset") {
    checkmate::assert_string(id)
    checkmate::assert_string(sheet)

    name <- where <- NULL # R CMD Check variable bindings fix

    if (!is_namespace_loaded("googlesheets4") ||
        !is_namespace_loaded("usethis")) {
        stop("This function requires the 'googlesheets4' and 'usethis' ",
             "packages to run. You can install them by running: \n\n",
             'install.packages("googlesheets4") \n',
             'install.packages("usethis")', call. = FALSE)
    }

    data <- googlesheets4::read_sheet(id, sheet, col_types = "c")

    data <- data %>%
        dplyr::filter(!(name == "operators")) %>%
        dplyr::mutate(across(where(is.character), stringr::str_squish))

    sheets <- list()

    for (i in seq_len(nrow(data))) {
        sheets[[data$name[i]]] <- list(name = data$name[i],
                                       type = data$type[i],
                                       id = data$id[i],
                                       sheet = data$sheet[i])
    }

    usethis::use_data(sheets, overwrite = TRUE)

    invisible(sheets)
}

#' Read the review tables hosted on Google Sheets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `read_sheet()` reads and returns the review tables hosted on Google Sheets.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See `?write_metadata()` to learn more.
#'
#' @param name (optional) A `character` object indicating the name or names of
#'   the sheets that the function must return (default: `NULL`).
#'
#' @return
#'
#' * If `name = NULL`, an invisible `list` object with `tibbles` objects of all
#' sheet/tables available as elements.
#' * If `name` have length > 1, an invisible `list` object with `tibbles`
#' objects of sheet/tables indicated in `name` as elements.
#' * If `name` have length == 1, an invisible `tibble` object of the sheet/table
#' indicated in `name`.
#'
#' @family GIPSO functions
#' @template param_a
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To get a 'list' object with all the sheets __
#' read_sheet()
#'
#' ## __ To get a 'list' object with some sheets __
#' read_sheet(c(names(sheets)[1], names(sheets)[2]))
#'
#' ## __ To get a 'tibble' object of a specific sheet __
#' read_sheet(sheet_id()[1])
#' }
read_sheet <- function(name = NULL, package = NULL) {
    checkmate::assert_character(name, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (!is_namespace_loaded("utils") ||
        !is_namespace_loaded("googlesheets4")) {
        stop("This function requires the 'utils' and 'googlesheets4' ",
             "packages to run. You can install them by running: \n\n",
             'install.packages("utils") \n',
             'install.packages("googlesheets4")', call. = FALSE)
    }

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("sheets", package, alert = "gipso_1")

    sheets <- NULL # R CMD Check variable bindings fix

    utils::data("sheets", package = package, envir = environment())
    checkmate::assert_subset(name, names(sheets), empty.ok = TRUE)

    if (!is.null(name)) {
        sheets <- sheets[name]
    } else {
        name <- stringr::str_subset(names(sheets), "^sheets$", negate = TRUE)
    }

    for (i in sheets) {
        data <- googlesheets4::read_sheet(i$id, i$sheet)
        assign(i$name, data)
    }

    if (length(name) == 1) {
        invisible(get(name))
    } else {
        variables <- mget(ls())
        invisible(variables[name])
    }
}

#' Write the review tables hosted on Google Sheets to the package
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `write_sheet()` reads and write the review tables hosted on Google Sheets in
#' the data directory of an R package.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See `?write_metadata()` to learn more.
#'
#' `write_sheet()` don't rewrite the `sheets` table.
#'
#' @param name (optional) A `character` object indicating the name or names of
#'   the sheets that the function must write (default: `NULL`).
#'
#' @family GIPSO functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To write all sheets __
#' write_sheet()
#'
#' ## __ To write one or more specific sheets __
#' write_sheet(sheet_id()[1])}
write_sheet <- function(name = NULL, package = NULL) {
    checkmate::assert_character(name, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (!is_namespace_loaded("utils") ||
        !is_namespace_loaded("googlesheets4")) {
        stop("This function requires the 'utils' and 'googlesheets4' packages ",
             'to run. You can install them by running: \n \n',
             'install.packages("utils") \n',
             'install.packages("googlesheets4")' , call. = FALSE)
    }

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("sheets", package, alert = "gipso_1")

    sheets <- NULL # R CMD Check variable bindings fix

    utils::data("sheets", package = package, envir = environment())

    if (!is.null(name)) {
        if (any(name == "sheets")) {
            stop("You can't rewrite the 'sheets' table. ",
                 "Use 'write_metadata()' to reload the sheets metadata.",
                 call. = FALSE)
        }

        checkmate::assert_subset(name, names(sheets), empty.ok = TRUE)

        if (length(name) == 1) {
            assign(name, read_sheet(name = name, package = package))
        } else {
            list2env(read_sheet(name, package), envir = environment())
        }
    } else {
        name <- stringr::str_subset(names(sheets), "^sheets$", negate = TRUE)
        list2env(read_sheet(name, package), envir = environment())
    }

    envir <- environment()

    for (i in name) {
        if(!(dir.exists("./data/"))) dir.create("./data/")

        file <- paste0("./data/", i, ".rda")
        save(list = i, file = file, envir = envir,
             compress = "bzip2", version = 2)
    }

    message("\n", "Don't forget to run 'devtools::load_all()' ",
            "(Ctrl + Shift + L).")

    invisible(NULL)
}
