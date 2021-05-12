# Source the file before running the functions

# library(checkmate)
# library(dplyr)
# library(googlesheets4)
# library(magrittr)
# library(usethis)

#' Build a lookup `list` with RIS tags used in several databases
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `ris_tags()` returns a lookup `list` object containing `data.frame` objects
#' with RIS tags used in several databases.
#'
#' The RIS tags values are stored in a Google Sheets. You can see it at
#' <https://bit.ly/3efSgHr>.
#'
#' See the `read_ris()` function documentation to learn more.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `ris_tags.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @family data functions
#' @export
#'
#' @examples
#' \dontrun{
#' ris_tags()
#' }
ris_tags <- function(write = FALSE) {
    checkmate::assert_flag(write)

    id <- "1T39uRC96lWo3OgxZQviO2qFiBwa9qiRpUYMaLLwwynM"

    sheets <- list(
        general = list(name = "general", sheet = "General"),
        apa = list(name = "apa", sheet = "APA"),
        ebsco = list(name = "ebsco", sheet = "EBSCO"),
        embase = list(name = "embase", sheet = "Embase"),
        pubmed = list(name = "pubmed", sheet = "PubMed"),
        scopus = list(name = "scopus", sheet = "Scopus"),
        wos = list(name = "wos", sheet = "Web of Science")
    )

    ris_tags <- list()

    for (i in sheets) {
        data <- googlesheets4::read_sheet(id, i$sheet, col_types = "c",
                                          na = c("", "NA")) %>%
            dplyr::mutate(order = as.integer(order)) %>%
            dplyr::arrange(order)

        ris_tags[[i$name]] <- data
    }

    if (isTRUE(write)) usethis::use_data(ris_tags, overwrite = TRUE)

    message("\n", "Run (in order):\n\n",
            "devtools::document() [Ctrl + Shift  + D]\n",
            "devtools::load_all() [Ctrl + Shift  + L]")

    invisible(ris_tags)
}

# ris_tags <- ris_tags()
