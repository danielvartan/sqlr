#' Count references/citations from files
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `count_ref()` count references/citations from files. If more than one file is
#' used, the function returns the sum of the count of the references/citations
#' from each file.
#'
#' At the moment, `count_ref()` works only with PubMed and RIS (Research
#' Information Systems) formats.
#'
#' @details
#'
#' `count_ref()` also allows you to read ZIP compacted files. Just assign
#' the ZIP file in the `file` argument.
#'
#' @param file (optional) a `character` object indicating RIS or ZIP file names.
#'   If not assigned, a dialog window will be open enabling the user to search
#'   and select a file (only for interactive sessions).
#'
#' @return A `numeric` value with the count or sum of the count of the
#'   references/citations found in the `file` argument.
#'
#' @family reference/citation functions
#' @export
#'
#' @examples
#' \dontrun{
#' references <- raw_data("reference")
#' file <- references[grep("_apa_", references)]
#' file <- raw_data("reference", file)
#'
#' count_ref(file)}
count_ref <- function(file = file.choose()) {
    checkmate::assert_character(file, min.len = 1, any.missing = FALSE,
                                all.missing = FALSE)

    if (length(file) > 1 ||
        any(stringr::str_detect(file, "(?i).zip$"), na.rm = TRUE)) {
        if (any(stringr::str_detect(file, ".zip$"), na.rm = TRUE)) {
            gutils:::require_pkg("utils")
        }

        out <- 0

        for (i in file) {
            if (stringr::str_detect(i, "(?i).zip$")) {
                i <- utils::unzip(i, exdir = tempdir())
            }

            out <- sum(out, count_ref(i))
        }

        return(out)
    }

    out <- 0

    for (i in file) {
        data <- guess_ref(i, return_data = TRUE)
        out <- sum(out, data$count)
    }

    out
}
