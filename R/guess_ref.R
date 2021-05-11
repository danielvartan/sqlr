#' Guess the format of references/citations from a file
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `guess_ref()` try to guess the format of references/citations from a file
#' (_e.g._, PubMed, RIS).
#'
#' At the moment, `guess_ref()` works only with PubMed and RIS (Research
#' Information Systems) formats.
#'
#' @details
#'
#' ## `return_data` argument
#'
#' When `return_data = TRUE`, `guess_ref()` will return a `list` object with
#' the following elements:
#'
#' * `format`: a string with the name of the reference/citation format found in
#' the `file` argument.
#' * `data`: a `character` object representing all the lines found in the `file`
#' argument.
#' * `count`: A `numeric` value with the count of the references/citations found
#' in the `file` argument. If you're only interest in this data, use the
#' `count_ref()` function.
#' * `index`: an `integer` object with values indicating the cut points of the
#' references/citations. If the cut point is located at the end of the
#' references/citations, the `between` value (see below) will be set to
#' `"right"`, else, it will be set to `"left`.
#' * `between`: a `"left"` or `"right"` value that can be used with the
#' `cutter()` function. See the documentation for the mentioned function to
#' learn more.
#'
#' @param file (optional) a string indicating a reference/citation file. If not
#'   assigned, a dialog window will be open enabling the user to search and
#'   select a file (only for interactive sessions).
#' @param return_data (optional) a `logical` value indicating if the function
#'   must return a `list` object with the data and metadata from `file`. See the
#'   Details section to learn more (default: `FALSE`).
#'
#' @return
#'
#' * If `return_data = FALSE`, a string with the name of the reference/citation
#' format found in the `file` argument.
#' * If `return_data = TRUE`, a `list` object with the data and metadata from
#' the `file` argument. See the Details section to learn more.
#' * The function will return an invisible `NULL` if it fails to found a format.
#'
#' @family reference/citation functions
#' @inheritParams tidy_keyword
#' @export
#'
#' @examples
#' \dontrun{
#' if (require_namespace("utils", quietly = TRUE)) {
#'     citations <- raw_data("citation")
#'     file <- citations[grep("_apa_", citations)]
#'     file <- raw_data("citation", file)
#'     file <- zip_files <- utils::unzip(file, exdir = tempdir())[1]
#'
#'     guess_ref(file)
#' }}
guess_ref <- function(file = file.choose(), return_data = FALSE,
                      quiet = FALSE) {
    checkmate::assert_string(file)
    checkmate::assert_flag(return_data)
    checkmate::assert_flag(quiet)

    if (stringr::str_detect(file, "(?i).zip$")) {
        stop("'.zip' files are not allowed.", call. = FALSE)
    }

    encoding <- readr::guess_encoding(file)
    encoding <- encoding$encoding[which(encoding$confidence ==
                                            max(encoding$confidence))]
    data <- readLines(file, encoding = encoding, warn = FALSE)

    pubmed <- detect_pattern(data, "^EDAT$|^EDAT |^EDAT-")
    ris <- detect_pattern(data, "^ER$|^ER |^ER-")

    if (sum(pubmed$count, ris$count) == 0 ||
        pubmed$count == ris$count) {
        if (isTRUE(quiet)) {
            invisible(NULL)
        } else {
            cat(crayonize("'guess_ref()' wasn't able to guess the citation/",
                          "reference format from the file."),
                emojinize("disappointed", left_space = TRUE),
                sep = "")

            invisible(NULL)
        }
    } else if (pubmed$count > ris$count) {
        pubmed <- detect_pattern(data, "^PMID$|^PMID |^PMID-")

        out <- list(format = "PubMed", data = data, count = pubmed$count,
                    index = pubmed$index, between = "left")
    } else {
        out <- list(format = "RIS", data = data, count = ris$count,
                    index = ris$index, between = "right")
    }

    if (isTRUE(return_data)) {
        out
    } else {
        out$format
    }
}

detect_pattern <- function(x, pattern) {
    checkmate::assert_character(x, min.len = 1)
    checkmate::assert_string(pattern)

    out <- vapply(x, stringr::str_detect, logical(1), pattern = pattern)
    list(count = length(which(out == TRUE)), index = which(out == TRUE))
}
