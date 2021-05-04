#' Count citations/references from RIS files
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `count_ris()` count citations/references from RIS (Research Information
#' Systems) files. If more than one file is used, the function returns the sum
#' of the count of the citations/references from each file.
#'
#' @details
#'
#' `count_ris()` also allows you to read zip compacted files. Just assign
#' the zip file in the `file` argument.
#'
#' @param file (optional) A `character` object indicating RIS or ZIP file names.
#'   If not assigned, a dialog window will be open enabling the user to search
#'   and select a file (only for interactive sessions).
#'
#' @return A `numeric` object with the count or sum of the count of the
#'   citations/references found in the 'file' argument.
#'
#' @family citation functions
#' @export
#'
#' @examples
#' \dontrun{
#' file <- "2021-04-28 - Citations - APA - EN - 1-1150.zip"
#' file <- raw_data("citation", file)
#'
#' count_ris(file)
#' #> [1] 1150 # Expected}
count_ris <- function(file = file.choose()) {
    checkmate::assert_character(file, min.len = 1, any.missing = FALSE,
                                all.missing = FALSE)

    if (all(stringr::str_detect(file, ".zip$"))) {
        if (!require_namespace("utils", quietly = TRUE)) {
            stop("This function requires the 'utils' package ",
                 'to unzip files. You can install it by running: \n\n',
                 'install.packages("utils")', call. = FALSE)
        }

        out <- 0

        for (i in file) {
            zip_files <- utils::unzip(i, exdir = tempdir())
            out <- sum(out, count_ris(zip_files))
        }

        return(out)
    }

    out <- 0

    for (i in file) {
        data <- readLines(i, warn = FALSE)

        index_1 <- vapply(data, stringr::str_detect, logical(1),
                          pattern = "^PMID$|^PMID |^PMID-")
        index_2 <- vapply(data, stringr::str_detect, logical(1),
                          pattern = "^ER$|^ER |^ER-")

        if (length(which(index_1 == TRUE)) >= length(which(index_2 == TRUE))) {
            index <- index_1
        } else {
            index <- index_2
        }

        out <- sum(out, length(which(index == TRUE)))
    }

    out
}
