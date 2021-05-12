# Source the file before running the functions

# library(checkmate)
# library(dplyr)
# library(googlesheets4)
# library(magrittr)
# library(usethis)

#' Build a lookup `list` with types of references used in RIS files
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `ris_types()` returns a lookup `list` object with elements for each type of
#' reference used in RIS files accompanied by a regular expression (for pattern
#' matching).
#'
#' The RIS type values are stored in a Google Sheets. You can see it at
#' <https://bit.ly/3biGkmp>.
#'
#' This values are used with the `create_reference()` function.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `ris_types.rda` file to `"./data/"` (default: `FALSE`).
#'
#' @family data functions
#' @export
#'
#' @examples
#' \dontrun{
#' ris_types()
#' }
ris_types <- function(write = FALSE) {
    checkmate::assert_flag(write)

    id <- "1zDhHeYAujRf0eedDFAdwN0vprh4tBN7omAm9RwsbF08"

    data <- googlesheets4::read_sheet(id, "General", col_types = "c",
                                      na = c("", "NA")) %>%
        dplyr::mutate(order = as.integer(order)) %>%
        dplyr::arrange(order)

    ris_types <- list()

    for (i in seq_len(nrow(data))) {
        j <- data[i, ]

        ris_types[[j$type]] <- list(type = j$type,
                                    description = j$description,
                                    pattern = j$pattern)
    }

    if (isTRUE(write)) usethis::use_data(ris_types, overwrite = TRUE)

    message("\n", "Run (in order):\n\n",
            "devtools::document() [Ctrl + Shift  + D]\n",
            "devtools::load_all() [Ctrl + Shift  + L]")

    invisible(ris_types)
}

# ris_types <- ris_types()
