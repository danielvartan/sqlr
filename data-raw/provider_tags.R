# Source the file before running the functions

# library(checkmate)
# library(googlesheets4)
# library(usethis)

#' Create a list with search field tags from several databases
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `provider_tags()` returns a `list` object containing `data.frame` objects
#' with the search field tags of several databases.
#'
#' The provider tags values are stored in a Google Sheets. You can see it at
#' <https://bit.ly/3m4Ys7z>.
#'
#' See the `query()` function documentation to learn more.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `provider_tags.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @family data functions
#' @export
#'
#' @examples
#' \dontrun{
#' provider_tags()
#' }
provider_tags <- function(write = FALSE) {
    checkmate::assert_flag(write)

    id <- "1ljlhZ5r5DozohySGFiGUPI1MDQehjnSR3tlXTqmdt48"

    sheets <- list(
        apa = list(name = "apa", sheet = "APA"),
        ebsco = list(name = "ebsco", sheet = "EBSCO"),
        embase = list(name = "embase", sheet = "Embase"),
        lilacs = list(name = "lilacs", sheet = "LILACS"),
        pubmed = list(name = "pubmed", sheet = "PubMed"),
        scielo = list(name = "scielo", sheet = "SciELO"),
        scopus = list(name = "scopus", sheet = "Scopus"),
        wos = list(name = "wos", sheet = "Web of Science")
    )

    provider_tags <- list()

    for (i in sheets) {
        data <- googlesheets4::read_sheet(id, i$sheet, col_types = "c",
                                          na = c("", "NA"))
        provider_tags[[i$name]] <- data
    }

    if (isTRUE(write)) usethis::use_data(provider_tags, overwrite = TRUE)

    message("\n", "Don't forget to run 'devtools::load_all()' ",
            "(Ctrl + Shift + L).")

    invisible(provider_tags)
}

# provider_tags <- provider_tags()
