#' Write queries in the `search` dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' SQLR system.
#'
#' `write_query()` create queries for each row of the `search` dataset and
#' write them in the `query` column.
#'
#' @param range (optional) a string indicating the cell range or start of the
#'   target rectangle where the function must write the output. See the `range`
#'   argument from `googlesheets4::range_write()` to learn more.
#'
#' @family SQLR system functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' write_query()}
write_query <- function(range = NULL, package = NULL) {
    checkmate::assert_string(range, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)
    require_pkg("utils", "googlesheets4")

    # R CMD Check variable bindings fix
    sheets <- provider <- language <- domain_set <- NULL
    constraint_set <- query <- approved <- constraint <- constraint_id <- NULL

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("sheets", package, alert = "gipso_1")
    assert_data("search", package, alert = "gipso_2")

    utils::data("sheets", package = package, envir = environment())
    utils::data("search", package = package, envir = environment())

    checkmate::assert_data_frame(search, min.rows = 1)

    if (is.null(range)) {
        range <- paste0(LETTERS[grep("^query$", names(search))], "2")
    }

    search <- search %>%
        dplyr::select(provider:query, approved) %>%
        dplyr::mutate(
            provider = dplyr::case_when(
                tolower(provider) == "apa psycnet" ~ "APA",
                tolower(provider) == "ebscohost" ~ "EBSCO",
                tolower(provider) == "web of science" ~ "WoS",
                TRUE ~ provider
            )
        )

    out <- character()

    for (i in seq_len(nrow(search))) {
        if (is.na(search$domain_set[i])) {
            out <- out %>% append(as.character(NA))
        } else {
            if (tolower(search$provider[i]) == "scopus") {
                enclosure <- "curly bracket"
            } else {
                enclosure <- "double quote"
            }

            out <- out %>% append(
                query(domain_set(search$domain_set[i],
                                 search$language[i],
                                 package),
                      provider = search$provider[i],
                      constraint = constraint_set(search$constraint_set[i],
                                                  package),
                      delimiter = ",", print = FALSE, clipboard = FALSE,
                      enclosure = enclosure)
            )
        }
    }

    out <- dplyr::tibble(query = out)
    googlesheets4::range_write(sheets$search$id, out, sheets$search$sheet,
                               range, col_names = FALSE, reformat = FALSE)
    write_sheet("search", package = package)

    invisible(NULL)
}

domain_set <- function(x, language, package = NULL) {
    checkmate::assert_string(x)
    checkmate::assert_string(language, null.ok = TRUE)
    checkmate::assert_string(package, null.ok = TRUE)

    x <- x %>%
        stringr::str_squish() %>%
        unlist() %>%
        stringr::str_replace_all("[^A-Za-z0-9 ]", "")

    pattern <- "^[0-9]$|^[0-9]+(\\s\\w+\\s[0-9]+)+$"

    if (isFALSE(stringr::str_detect(x, pattern))) {
        stop("'x' must conform to the pattern ",
             "'^[0-9]$|^[0-9]+(\\s\\w+\\s[0-9]+)+$'", ", i.e., ",
             "'x' must be a string like '1', 1 AND 2', or '1 NOT 2 AND 3'. ",
             "Please note that all characters that don't conform to the ",
             "pattern '[^A-Za-z0-9 ]' are removed.",
             call. = FALSE)
    }

    domains <- x %>%
        stringr::str_extract_all("[0-9]+") %>%
        unlist() %>%
        as.numeric()

    x <- x %>%
        stringr::str_split(" ") %>%
        unlist()

    out <- character()

    for (i in seq_along(x)) {
        if (x[i] %in% domains) {
            set <- keyword_set(as.numeric(x[i]), language, package)
            tidy <- tidy_keyword(set)

            if (!(length(set) == length(tidy))) {
                stop("Some keywords from the domain ", single_quote_(x[i]),
                     " and language ", single_quote_(language),
                     " are missing after the keyword tidying process.")
            }

            out <- out %>% append(keyword_set(as.numeric(x[i]), language,
                                              package))
        } else {
            out <- out %>% append(x[i])
        }
    }

    paste0(out, collapse = ",")
}

constraint_set <- function(x, package = NULL) {
    checkmate::assert_string(x)
    checkmate::assert_string(package, null.ok = TRUE)

    # R CMD Check variable bindings fix
    constraint <- constraint_id <- NULL

    x <- x %>%
        stringr::str_squish() %>%
        unlist() %>%
        stringr::str_replace_all("[^A-Za-z0-9 ]", "")

    pattern <- "^[0-9]$|^[0-9]+(\\s\\w+\\s[0-9]+)+$"

    if (isFALSE(stringr::str_detect(x, pattern))) {
        stop("'x' must conform to the pattern ",
             "'^[0-9]$|^[0-9]+(\\s\\w+\\s[0-9]+)+$'", ", i.e., ",
             "'x' must be a string like '1', 1 AND 2', or '1 NOT 2 AND 3'. ",
             "Please note that all characters that don't conform to the ",
             "pattern '[^A-Za-z0-9 ]' are removed.",
             call. = FALSE)
    }

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("constraint", package, alert = "gipso_2")

    utils::data("constraint", package = package, envir = environment())

    out <- x %>%
        stringr::str_extract_all("[0-9]+") %>%
        unlist() %>%
        as.numeric()

    out <- constraint %>%
        dplyr::filter(!(tolower(class) == "intrinsic"), constraint_id %in% out)

    out$name
}
