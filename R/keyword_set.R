#' Get a keyword set from the `keyword` dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `keyword_set` returns a tidied single keyword set from the keyword.
#'
#' @param domain_id An `integer` or `numeric` value indicating the domain ID to
#'   return.
#' @param language (optional) A string indicating the language constraint of
#'   the keywords (case insensitive).
#'
#' @return A `character` object with a tidied single keyword set from the
#'   keyword dataset.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## __ To write all sheets __
#' write_sheet()
#'
#' ## __ To write only a specific sheet __
#' write_sheet(sheet_id()[1])}
keyword_set <- function(domain_id, language = NULL, update = FALSE) {
    domains <- as.numeric(unique(keyword$domain_id))
    languages <- tolower(unique(keyword$language))
    domain_id <- shush(as.numeric(domain_id))

    checkmate::assert_number(domain_id)
    checkmate::assert_choice(as.numeric(domain_id), domains)
    checkmate::assert_flag(update)

    if (!is.null(language)) {
        checkmate::assert_string(as.character(language))
        checkmate::assert_choice(tolower(language), languages)
    }

    if (isTRUE(update)) {
        data <- read_sheet("keyword")
    } else {
        data <- keyword
    }

    data <- data %>%
        dplyr::filter(approved == TRUE) %>%
        dplyr::mutate(
            keyword = dplyr::case_when(
                !is.na(keyword) & !is.na(variation) ~
                    paste0(keyword, ",", variation),
                !is.na(keyword) & is.na(variation) ~
                    keyword
            )
        ) %>%
        dplyr::select(domain_id, language, keyword)

    domain <- domain_id
    lang <- unique(data$language)[languages %in% tolower(language)]

    if (is.null(language)) {
        data <- data %>%
            dplyr::filter(domain_id == domain)
    } else {
        data <- data %>%
            dplyr::filter(domain_id == domain, language == lang)
    }

    tidy_keyword(data$keyword)
}
