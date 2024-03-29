#' Group keywords from the `keyword` dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' `sqlr` system.
#'
#' `keyword_set()` returns a keyword set from the `keyword` dataset.
#'
#' @param domain_id An `integer` or `numeric` value indicating the domain ID to
#'   return.
#' @param language (optional) A string indicating the language constraint of
#'   the keywords (case insensitive).
#'
#' @return A `character` object with a keyword set.
#'
#' @family keyword functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' keyword_set(1, "english")}
keyword_set <- function(domain_id, language = NULL,
                        package = rutils:::get_package_name()) {
    checkmate::assert_integerish(domain_id)
    checkmate::assert_string(language, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)
    rutils:::require_pkg("utils")

    rutils:::assert_namespace(package)
    rutils:::assert_data("keyword", package)

    keyword <- approval <- NULL # R CMD Check variable bindings fix

    utils::data("keyword", package = package, envir = environment())
    cols <- c("domain_id", "language", "keyword", "variation", "approval")

    checkmate::assert_data_frame(keyword, min.rows = 1)
    checkmate::assert_subset(cols, names(keyword))

    domains <- as.numeric(unique(keyword$domain_id))
    languages <- tolower(unique(keyword$language))
    domain_id <- as.numeric(domain_id)

    checkmate::assert_choice(domain_id, domains)

    if (!is.null(language)) {
        checkmate::assert_choice(tolower(language), languages)
    }

    keyword <- keyword %>%
        dplyr::filter(approval == TRUE) %>%
        dplyr::mutate(
            keyword = dplyr::case_when(
                !is.na(keyword) & !is.na(variation) ~
                    paste0(keyword, ",", variation),
                !is.na(keyword) & is.na(variation) ~
                    keyword
            )) %>%
        dplyr::select(domain_id, language, keyword)

    dom <- domain_id
    lang <- unique(keyword$language)[languages %in% tolower(language)]

    if (is.null(language)) {
        keyword <- keyword %>% dplyr::filter(domain_id == dom)
    } else {
        keyword <- keyword %>%
            dplyr::filter(domain_id == dom, language == lang)
    }

    paste0(keyword$keyword, collapse = ",") %>%
        strsplit(",") %>%
        unlist() %>%
        trimws()
}
