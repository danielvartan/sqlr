#' Group keywords from the `keyword` dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `keyword_set` returns a keyword set from the `keyword` dataset.
#'
#' @param domain_id An `integer` or `numeric` value indicating the domain ID to
#'   return.
#' @param language (optional) A string indicating the language constraint of
#'   the keywords (case insensitive).
#'
#' @return A `character` object with a keyword set.
#'
#' @family GIPSO functions
#' @inheritParams sheet_id
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' keyword_set(1, "english")}
keyword_set <- function(domain_id, language = NULL, package = NULL) {
    keyword <- approved <- NULL # R CMD Check variable bindings fix

    if (!is_namespace_loaded("utils")) {
        stop("This function requires the 'utils' package ",
             'to run. You can install it by running: \n \n',
             'install.packages("utils")', call. = FALSE)
    }

    if (is.null(package)) {
        package <- stringr::str_extract(
            rstudioapi::getActiveProject(), "[a-zA-Z0-9.]*$")
    }

    utils::data("keyword", package = package, envir = environment())
    data <- keyword

    cols <- c("domain_id", "language", "keyword", "variation", "approved")

    checkmate::assert_data_frame(data, min.rows = 1)
    checkmate::assert_subset(cols, names(data))

    domains <- as.numeric(unique(data$domain_id))
    languages <- tolower(unique(data$language))
    domain_id <- shush(as.numeric(domain_id))

    checkmate::assert_number(domain_id)
    checkmate::assert_choice(domain_id, domains)

    if (!is.null(language)) {
        checkmate::assert_string(as.character(language))
        checkmate::assert_choice(tolower(language), languages)
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

    dom <- domain_id
    lang <- unique(data$language)[languages %in% tolower(language)]

    if (is.null(language)) {
        data <- data %>% dplyr::filter(domain_id == dom)
    } else {
        data <- data %>%
            dplyr::filter(domain_id == dom, language == lang)
    }

    out <- paste0(data$keyword, collapse = ",")
    out <- unlist(strsplit(out, ","))
    out <- trimws(out)

    out
}
