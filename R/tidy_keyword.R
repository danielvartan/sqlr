#' Tidy keywords
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tidy_keyword()` tidy keyword values for database search.
#'
#' @param ... One or more `character` objects containing keywords.
#' @param min_chars (optional) A number indicating the minimal number of
#'   characters a keyword must have. Keywords that don't comply to this setting
#'   will be transformed to `NA`.
#' @param delimiter (optional) A string of length 1 with the delimiter used on
#'   `...` values. Use `delimiter = NULL` to disable this behavior (default:
#'   `NULL`).
#' @param clean_modifiers (optional) A `logical` value indicating if keywords
#'   with modifiers must be transformed to `NA`. Modifiers follow the
#'   `[*?():'\"]` pattern (default: `TRUE`).
#' @param sort (optional) A `logical` value indicating if the output must be
#'   ordered alphabetically (default: `FALSE`).
#' @param na_rm (optional) A `logical` value indicating if `NA` values must be
#'   removed from the output (default: `TRUE`).
#' @param duplicate_rm (optional) A `logical` value indicating if duplicate
#'   values must be removed from the output (default: `TRUE`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A `character` object with keywords in `...` tidied.
#'
#' @family keyword functions
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' tidy_keyword("Lorem", "Ipsum dolor", c("sit", "amet"))
#' #> [1] "lorem"           "\"ipsum dolor\"" "sit" # Expected
#' #> [4] "amet" # Expected
#'
#' tidy_keyword("Lorem", "Ipsum dolor", c("sit", "amet"), sort = TRUE)
#' #> [1] "amet"            "\"ipsum dolor\"" "lorem" # Expected
#' #> [4] "sit" # Expected
#'
#' tidy_keyword("Lorem", "Ipsum, dolor", delimiter = ",")
#' #> [1] "lorem" "ipsum" "dolor" # Expected
#'
#' tidy_keyword("Lorem", "Ipsum*", clean_modifiers = TRUE, na_rm = TRUE)
#' #> [1] "lorem" # Expected
#'
#' tidy_keyword("Lorem", "Ipsum*", clean_modifiers = TRUE, na_rm = FALSE)
#' #> [1] "lorem" NA # Expected
tidy_keyword <- function(..., min_chars = 1, delimiter = ",",
                         clean_modifiers = TRUE, sort = FALSE,
                         na_rm = TRUE, duplicate_rm = TRUE, quiet = FALSE) {
    out <- list(...)

    checkmate::assert_string(delimiter, pattern = "^.$", null.ok = TRUE)
    checkmate::assert_number(min_chars, lower = 1)
    checkmate::assert_flag(clean_modifiers)
    checkmate::assert_flag(sort)
    checkmate::assert_flag(na_rm)
    checkmate::assert_flag(duplicate_rm)
    checkmate::assert_flag(quiet)
    lapply(out, checkmate::assert_character)
    lapply(strsplit(unlist(out), delimiter), checkmate::assert_character)

    out <- unlist(out, use.names = FALSE)

    if (!is.null(delimiter)) {
        out <- unlist(strsplit(out, delimiter))
    }

    invalid <- grepl("'", unlist(out, use.names = FALSE), perl = TRUE)

    if (any(invalid) && isFALSE(quiet)) {
        warning(inline_collapse(out[invalid]), " have invalid values. ",
                "If 'na_rm = FALSE' those values will be transformed to ",
                "'NA'. Else, they will be removed from the output.",
                call. = FALSE)
    }

    out <- stringr::str_squish(out)
    out <- tolower(out)
    out <- gsub(" OR ", delimiter, out)

    modifiers <- "[*?():'\"]"

    out <- dplyr::case_when(
        nchar(out) < min_chars ~ as.character(NA),
        isTRUE(clean_modifiers) & grepl(modifiers, out, perl = TRUE) ~
            as.character(NA),
        TRUE ~ out
    )

    if (isTRUE(sort)) out <- sort(out, na.last = TRUE)
    if (isTRUE(na_rm)) out <- out[!is.na(out)]
    if (isTRUE(duplicate_rm)) out <- out[!duplicated(out)]

    out <- dplyr::if_else(
        is.na(out) | grepl("^[a-zA-Z0-9]+$", out, perl = TRUE) |
            grepl(modifiers, out, perl = TRUE),
        out, double_quote_(out))

    out
}
