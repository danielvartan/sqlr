# Source the file before running the functions

# library(checkmate)
# library(usethis)

#' Create a list with Google Sheets metadata
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sheets()` creates and returns a `list` object containing lists with the
#' Google Sheets metadata of the review entity tables.
#'
#' This must be set for all R packages from GIPSO for systematic quantitative
#' literature reviews.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `sheets.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @return An invisible `list` object containing lists with the Google Sheets
#'   metadata of the review entity tables.
#'
#' @family data functions
#' @export
#'
#' @examples
#' \dontrun{
#' sheets()}
sheets <- function(write = FALSE) {
    checkmate::assert_flag(write)

    sheets <- list(
        domain = list(
            name = "domain",
            id = "16B-JH-3QiqPPdFIiYHssKAZKFe8ehYtYVuFqFH8MnrU",
            sheet = "Dataset"),
        constraint = list(
            name = "constraint",
            id = "1Ikw73ifT09CoPaWOc7avugybEsbQ7HIkO_Fn7R0WSrU",
            sheet = "Dataset"),
        keyword = list(
            name = "keyword",
            id = "1Re3lLhjbpBmYBVCMdt37-ZwAOKmWnPdMEJD8J7Sc86k",
            sheet = "Dataset"),
        source = list(
            name = "source",
            id = "1Ef0wzRMjTcibYwWQ0T0Ap2s3c07A-DiGqCSduUSxlMY",
            sheet = "Dataset"),
        search = list(
            name = "search",
            id = "1yZCRt3G94JS73T55rGCxdgUHrKsM6QX6RHSeRUxe3J4",
            sheet = "Dataset")
    )

    if (isTRUE(write)) {
        if(!(dir.exists("./data/"))) dir.create("./data/")

        usethis::use_data(sheets, overwrite = TRUE)
    }

    invisible(sheets)
}

# sheets <- sheets()
