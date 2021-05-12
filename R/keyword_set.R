#' Group keywords from the `keyword` dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' SQLR system.
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
#' @family SQLR system functions
#' @template param_a
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' keyword_set(1, "english")}
keyword_set <- function(domain_id, language = NULL, package = NULL) {
    keyword <- approved <- NULL # R CMD Check variable bindings fix

    if (!require_namespace("utils", quietly = TRUE) ||
        !require_namespace("magrittr", quietly = TRUE)) {
        stop("This function requires the 'utils' and 'magrittr' packages ",
             'to run. You can install them by running: \n\n',
             'install.packages("utils") \n',
             'install.packages("magrittr")' , call. = FALSE)
    }

    checkmate::assert_integerish(domain_id)
    checkmate::assert_string(language, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("keyword", package, alert = "gipso_2")

    utils::data("keyword", package = package, envir = environment())
    cols <- c("domain_id", "language", "keyword", "variation", "approved")

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
        dplyr::filter(approved == TRUE) %>%
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
