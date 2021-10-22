#' Build and write the `reference` table to a SQLR package
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `build_reference()` builds and write the `reference` entity table of
#' the Systematic Quantitative Literature Review (SQLR) system to an R package.
#'
#' You must have a `sheets` data object with the sheets metadata before running
#' this function. See [write_metadata()] to learn more.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `reference.rda` file to `"./data/"` and also write to the reference
#'   spreadsheet listed on the `sqlr::sheets` object (default: `TRUE`).
#'
#' @family reference/citation functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' build_reference()}
build_reference <- function(package = gutils:::get_package_name(),
                            write = TRUE) {
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(write)
    gutils:::require_pkg("utils", "googlesheets4")

    # R CMD Check variable bindings fix
    sheets <- NULL

    gutils:::assert_namespace(package)
    gutils:::assert_data("sheets", package, alert = "gipso_1")

    utils::data("sheets", package = package, envir = environment())
    choices <- c("source", "search")
    checkmate::assert_subset(choices, names(sheets))

    if (isTRUE(write)) {
        if (!is_interactive()) {
            stop("You must be in a interactive R session to write the ",
                 "'reference' table.", call. = FALSE)
        }

        checkmate::assert_subset("reference", names(sheets))
        googlesheets4::gs4_auth()
    }

    cli::cat_line()
    cli::cli_alert_info(paste0(
        "{.strong This may take a while. Please be patient.} \U00023F3"
        ))
    cli::cat_line()

    out <- read_ref_extdata(package = package) %>%
        tidy_reference() %>%
        identify_ref_duplicates() %>%
        assign_ref_ids(package = package)

    if (isTRUE(write)) out %>% write_reference(package = package)

    invisible(out)
}

read_ref_extdata <- function(package = gutils:::get_package_name()) {
    checkmate::assert_string(package, null.ok = TRUE)
    gutils:::assert_namespace(package)

    gutils::normalize_extdata(package)
    files <- gutils::raw_data_2("reference", package = package)

    if (length(files) == 0) {
        stop("The 'reference' folder is empty.", call. = FALSE)
    }

    cli::cli_alert_info("Reading and parsing reference/citation files.")

    out <- dplyr::tibble()

    for (i in files) {
        pattern <- "(?<=reference_).+(?=_en|pt|es)"
        lookup <- stringr::str_extract(i, pattern)
        lookup <- stringr::str_replace(lookup, "^web-of-science$", "wos")

        i <- gutils::raw_data_2("reference", i, package = package)
        data <- gutils:::shush(refstudio::read_ref(i, lookup = lookup))
        out <- dplyr::bind_rows(out, data)
    }

    invisible(out)
}

tidy_reference <- function(x) {
    checkmate::assert_data_frame(x, min.rows = 1)

    # R CMD Check variable bindings fix
    type <- work_type <- doi <- pmid <- year <- NULL

    cli::cli_alert_info("Tidying the references/citations.")

    x <- x %>% dplyr::mutate(dplyr::across(.fns = stringr::str_squish))

    if (all(c("type", "work_type") %in% names(x), na.rm = TRUE)) {
        for (i in refstudio::ris_types) {
            x <- x %>% dplyr::mutate(
                type = dplyr::if_else(
                    is.na(type) & !is.na(work_type) &
                        stringr::str_detect(work_type, i$pattern),
                    i$type,
                    type,
                    missing = as.character(NA)))
        }
    }

    if ("doi" %in% names(x)) {
        # About the DOI pattern: <https://bit.ly/3eA2By0>.
        pattern_doi <- "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"

        x <- x %>%
            dplyr::mutate(doi = stringr::str_extract(doi, pattern_doi))
    }

    if ("pmid" %in% names(x)) {
        pattern_pmid <- "^\\d+$"

        x <- x %>%
            dplyr::mutate(pmid = stringr::str_extract(pmid, pattern_pmid))
    }

    if ("year" %in% names(x)) {
        x <- x %>% dplyr::mutate(
            year = stringr::str_extract(year, "^\\d{4}"))
    }

    if (all(c("start_page", "end_page", "pagination") %in% names(x),
            na.rm = TRUE)) {
        x <- x %>% dplyr::mutate(
            start_page = dplyr::case_when(
                is.na(start_page) & !is.na(pagination) &
                    stringr::str_detect(pagination, "-") ~
                    stringr::str_extract(pagination, "^.+(?=-)"),
                is.na(start_page) & !is.na(pagination) ~ pagination,
                TRUE ~ start_page),
            end_page = dplyr::case_when(
                is.na(end_page) & !is.na(pagination) &
                    stringr::str_detect(pagination, "-") ~
                    stringr::str_extract(pagination, "(?<=-).+"),
                TRUE ~ end_page))
    }

    cols <- c("type", "doi", "pmid", "author", "year", "title",
              "abstract", "keyword", "journal", "place_published", "volume",
              "issue", "start_page", "end_page", "publisher", "standard_number",
              "author_info", "author_id", "secondary_author", "tertiary_author",
              "editor", "corporate_author", "subsidiary_author", "short_title",
              "secondary_title", "tertiary_title", "journal_abbreviation",
              "book_title", "work_type", "publication_status", "language",
              "database", "provider", "file", "length")

    x <- x %>% dplyr::rowwise() %>%
        dplyr::select(dplyr::all_of(cols[which(cols %in% names(x))])) %>%
        dplyr::mutate(
            length = dplyr::n_distinct(dplyr::c_across(), na.rm = TRUE)) %>%
        dplyr::ungroup()

    x <- x %>%
        dplyr::select(dplyr::all_of(cols[which(cols %in% names(x))]))

    if ("year" %in% names(x)) {
        x <- x %>% dplyr::mutate(year = as.integer(year))
    }

    if ("pmid" %in% names(x)) {
        x <- x %>% dplyr::mutate(pmid = as.numeric(pmid))
    }

    x
}

identify_ref_duplicates <- function(x) {
    checkmate::assert_data_frame(x, min.rows = 1)

    # R CMD Check variable bindings fix
    criteria_id <- trial_id <- NULL

    cli::cli_alert_info("Identifying and marking duplications.")

    x <- x %>%
        dplyr::mutate(criteria_id = as.character(NA),
                      trial_id = as.character(NA)) %>%
        dplyr::relocate(criteria_id, trial_id) %>%
        dplyr::arrange(dplyr::desc(length)) %>%
        dplyr::mutate(criteria_id = dplyr::case_when(
            duplicated(tolower(doi), incomparables = NA) ~ "DUP",
            duplicated(pmid, incomparables = NA) ~ "DUP",
            TRUE ~ as.character(NA))) %>%
        dplyr::mutate(trial_id = criteria_id)

    x
}

assign_ref_ids <- function(x, package = gutils:::get_package_name()) {
    checkmate::assert_data_frame(x, min.rows = 1)
    checkmate::assert_string(package, null.ok = TRUE)
    gutils:::assert_namespace(package)

    # R CMD Check variable bindings fix
    sheets <- source <- search <- provider <- year <- title <- NULL
    reference_id <- criteria_id <- trial_id <- source_id <- search_id <- NULL
    pdf <- NULL

    gutils:::assert_data("sheets", package, alert = "gipso_1")
    utils::data("sheets", package = package, envir = environment())

    gutils:::assert_data("source", package)
    utils::data("source", package = package, envir = environment())

    gutils:::assert_data("search", package)
    utils::data("search", package = package, envir = environment())

    cli::cli_alert_info("Assigning IDs and finalizing the dataset.")

    lookup_builder <- function(provider, id, filter = NULL) {
        checkmate::assert_character(provider)
        gutils:::assert_identical(provider, id, type = "length",
                                  any.missing = FALSE, null.ok = FALSE)
        checkmate::assert_logical(filter, min.len = 1, null.ok = TRUE)

        id <- gutils:::shush(as.integer(id))

        if (!is.null(filter)) {
            provider <- provider[filter]
            id <- id[filter]
        }

        out <- list()

        for (i in seq_along(provider)) {
            pattern <- paste0("(?i)", provider[i])

            out[[provider[i]]] <- list(provider = provider[i],
                                       id = id[i],
                                       pattern = pattern)
        }

        out
    }

    source_lookup <- lookup_builder(source$provider, source$source_id)
    search_lookup <- lookup_builder(search$provider, search$search_id,
                                    search$approval)

    for (i in c("source", "search")) {
        var <- paste0(i, "_id")
        lookup <- get(paste0(i, "_lookup"))

        x <- x %>% dplyr::mutate(!!as.symbol(var) := as.integer(NA))

        for (j in lookup) {
            x <- x %>% dplyr::mutate(
                !!as.symbol(var) := dplyr::if_else(
                    stringr::str_detect(provider, j$pattern),
                    j$id,
                    !!as.symbol(var),
                    missing = as.integer(NA)))
        }
    }

    if (!length(which(is.na(x$source_id))) == 0 ||
        !length(which(is.na(x$search_id))) == 0) {
        stop("'source_id' and 'search_id' have 'NA's. ",
             "Check the 'source' and 'search' tables and try again.",
             call. = FALSE)
    }

    x <- x %>%
        dplyr::arrange(dplyr::desc(year), title) %>%
        dplyr::mutate(pdf = as.character(NA))

    x <- x %>% dplyr::mutate(reference_id = seq(1, nrow(x))) %>%
        dplyr::relocate(reference_id, criteria_id, trial_id, source_id,
                        search_id, pdf)

    x
}
