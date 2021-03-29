#' Tidy keywords
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `tidy_keyword()` tidy keyword values for database search.
#'
#' @details
#'
#' ## `enclosure` argument
#'
#' The `enclosure` argument let you choose the type of enclosure for keywords
#' with special characters, _i.e_, all keywords that don't conform to the
#' pattern `"^[a-zA-Z0-9]+$"`. The default value is `"double quote"`.
#'
#' Valid choices for this argument are:
#'
#' * `"single quote"`: to enclose the keyword with single quotes (_e.g._,
#' `'lorem'`)
#' * `"double quote"`: to enclose the keyword with double quotes (_e.g._,
#' `"ipsum"`)
#' * `"round bracket"`: to enclose the keyword with round brackets or
#' parenthesis (_e.g._ `(dolor)`)
#' * `"curly bracket"`: to enclose the keyword with curly brackets (_e.g._
#' `{dolor}`)
#' * `"square bracket"`: to enclose the keyword with square brackets (_e.g._
#' `[dolor]`)
#'
#' ## `clean modifiers` argument
#'
#' Modifiers follow the `[*$?():'\"]` pattern. If `clean
#' modifiers` is set to `TRUE`, all keywords with the latter pattern will be
#' transformed to `NA`.
#'
#' @param ... One or more `character` objects containing keywords.
#' @param min_chars (optional) A number indicating the minimal number of
#'   characters a keyword must have. Keywords that don't comply to this setting
#'   will be transformed to `NA` (default: `1`).
#' @param delimiter (optional) A string with the delimiter used on `...` values.
#'   Use `delimiter = NULL` to disable this behavior (default: `NULL`).
#' @param enclosure (optional) A string indicating the type of enclosure for
#'   keywords with special characters (like spaces) (default: `"double quote"`).
#' @param clean_modifiers (optional) A `logical` value indicating if keywords
#'   with modifiers must be transformed to `NA` (default: `TRUE`).
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
                         enclosure = "double quote",
                         clean_modifiers = TRUE, sort = FALSE,
                         na_rm = TRUE, duplicate_rm = TRUE, quiet = FALSE) {
    out <- list(...)
    choices <- c("single quote", "double quote", "round bracket",
                 "curly bracket", "square bracket")

    checkmate::assert_string(delimiter, pattern = "^.$", null.ok = TRUE)
    checkmate::assert_number(min_chars, lower = 1)
    checkmate::assert_choice(enclosure, choices)
    checkmate::assert_flag(clean_modifiers)
    checkmate::assert_flag(sort)
    checkmate::assert_flag(na_rm)
    checkmate::assert_flag(duplicate_rm)
    checkmate::assert_flag(quiet)
    lapply(out, checkmate::assert_character)

    out <- out %>% unlist(use.names = FALSE)

    if (!is.null(delimiter)) {
        out <- out %>% strsplit(delimiter) %>% unlist()
    }

    invalid <- grepl("'", unlist(out, use.names = FALSE), perl = TRUE)

    if (any(invalid) && isFALSE(quiet)) {
        warning(inline_collapse(out[invalid]), " have invalid values. ",
                "If 'na_rm = FALSE' those values will be transformed to ",
                "'NA'. Else, they will be removed from the output.",
                call. = FALSE)
    }

    out <- out %>%
        stringr::str_squish() %>%
        tolower() %>%
        stringr::str_replace_all(" OR ", delimiter)

    modifiers <- "[*$?():'\"]"

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
        out, enclosure(out, enclosure))

    out
}
