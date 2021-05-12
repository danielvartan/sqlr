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
#' this function. See `?write_metadata()` to learn more.
#'
#' @param write (optional) a `logical` value indicating if the function must
#'   write a `reference.rda` file to `"./data/"` and also write to the reference
#'   spreadsheet listed on the `sqlr::sheets` object (default: `TRUE`).
#'
#' @family SQLR system functions
#' @template param_a
#' @template param_b
#' @export
#'
#' @examples
#' \dontrun{
#' build_reference()}
build_reference <- function(package = NULL, write = TRUE, quiet = FALSE) {
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(write)
    checkmate::assert_flag(quiet)

    # R CMD Check variable bindings fix
    sheets <- NULL

    if (!require_namespace("utils", quietly = TRUE) ||
        !require_namespace("googlesheets4", quietly = TRUE)) {
        stop("This function requires the 'utils' and 'googlesheets4' packages ",
             'to run. You can install them by running: \n\n',
             'install.packages("utils") \n',
             'install.packages("googlesheets4")' , call. = FALSE)
    }

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("sheets", package, alert = "gipso_1")

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

    message <- paste0("\n",
                      crayonize("This may take a while. Please be patient."),
                      emojinize("hourglass_flowing_sand", left_space = TRUE),
                      "\n\n")
    if (isFALSE(quiet)) cat(message)

    out <- read_ref_extdata(package = package, quiet = quiet) %>%
        tidy_reference(quiet = quiet) %>%
        identify_ref_duplicates(quiet = quiet) %>%
        assign_ref_ids(package = package, quiet = quiet)

    if (isTRUE(write)) {
        out %>% write_reference(package = package, quiet = quiet)
    }

    invisible(out)
}

read_ref_extdata <- function(package = NULL, quiet = FALSE) {
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(quiet)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)

    normalize_extdata(package)
    files <- raw_data("reference", package = package)

    if (length(files) == 0) {
        stop("The 'reference' folder is empty.", call. = FALSE)
    }

    alert("** Reading and parsing reference/citation files **",
          combined_styles = c("silver"), abort = quiet)

    out <- dplyr::tibble()

    for (i in files) {
        pattern <- "(?<=reference_).+(?=_en|pt|es)"
        lookup <- stringr::str_extract(i, pattern)
        lookup <- stringr::str_replace(lookup, "^web-of-science$", "wos")

        i <- raw_data("reference", i)
        data <- read_ref(i, lookup = lookup, quiet = TRUE)
        out <- dplyr::bind_rows(out, data)
    }

    invisible(out)
}

tidy_reference <- function(x, quiet = FALSE) {
    checkmate::assert_data_frame(x, min.rows = 1)
    checkmate::assert_flag(quiet)

    # R CMD Check variable bindings fix
    type <- work_type <- doi <- pmid <- year <- NULL

    alert("** Tidying the references/citations **",
          combined_styles = c("silver"), abort = quiet)

    x <- x %>% dplyr::mutate(dplyr::across(.fns = stringr::str_squish))

    if (all(c("type", "work_type") %in% names(x), na.rm = TRUE)) {
        for (i in sqlr::ris_types) {
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

identify_ref_duplicates <- function(x, quiet = FALSE) {
    checkmate::assert_data_frame(x, min.rows = 1)
    checkmate::assert_flag(quiet)

    # R CMD Check variable bindings fix
    criteria_id <- trial_id <- NULL

    alert("** Identifying and marking duplications **",
          combined_styles = c("silver"), abort = quiet)

    x <- x %>%
        dplyr::mutate(criteria_id = as.character(NA),
                      trial_id = as.character(NA)) %>%
        dplyr::relocate(criteria_id, trial_id) %>%
        dplyr::arrange(dplyr::desc(length)) %>%
        dplyr::mutate(criteria_id = dplyr::case_when(
            duplicated(doi, incomparables = NA) ~ "DUP",
            duplicated(pmid, incomparables = NA) ~ "DUP",
            TRUE ~ as.character(NA))) %>%
        dplyr::mutate(trial_id = criteria_id)

    x
}

assign_ref_ids <- function(x, package = NULL, quiet = FALSE) {
    checkmate::assert_data_frame(x, min.rows = 1)
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(quiet)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("sheets", package, alert = "gipso_1")

    # R CMD Check variable bindings fix
    sheets <- provider <- year <- title <- reference_id <- NULL
    criteria_id <- trial_id <- source_id <- search_id <- NULL
    pdf <- NULL

    utils::data("sheets", package = package, envir = environment())
    choices <- c("source", "search")
    checkmate::assert_subset(choices, names(sheets))

    alert("** Assigning IDs and finalizing the dataset **",
          combined_styles = c("silver"), abort = quiet)

    list2env(read_sheet(choices, package), envir = environment())

    lookup_builder <- function(provider, id, filter = NULL) {
        checkmate::assert_character(provider)
        assert_identical(provider, id, type = "length", any.missing = FALSE,
                         null.ok = FALSE)
        checkmate::assert_logical(filter, min.len = 1, null.ok = TRUE)

        id <- shush(as.integer(id))

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
             "Check the 'source' and 'search' spreadsheet and try again.",
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

write_reference <- function(x, package = NULL, quiet = FALSE) {
    checkmate::assert_data_frame(x, min.rows = 1)
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_flag(quiet)

    if (is.null(package)) package <- get_package_name()
    assert_namespace(package)
    assert_data("sheets", package, alert = "gipso_1")

    # R CMD Check variable bindings fix
    sheets <- where <- doi <- pmid <- year <- NULL

    if(!(dir.exists("./data/"))) dir.create("./data/")
    file <- paste0("./data/", "reference", ".rda")
    reference <- x

    alert("** Writing the 'reference' table to the package **",
          combined_styles = c("silver"), abort = quiet)

    save(reference, file = file, envir = environment(), compress = "bzip2",
         version = 2)
    rm(reference)

    utils::data("sheets", package = package, envir = environment())
    checkmate::assert_subset("reference", names(sheets))

    alert("** Writing the 'reference' table to Google Sheets **",
          combined_styles = c("silver"), abort = quiet)

    str_subset <- function(x) {
        dplyr::case_when(
            nchar(x) >= 50000 ~ stringr::str_sub(x, 1, 49999),
            TRUE ~ x
        )
    }

    x <- x %>% dplyr::mutate(dplyr::across(where(is.character), str_subset))

    range_write(x, name = "reference", package = package, quiet = quiet)

    message("\n", "Run (in order):\n\n",
            "devtools::document() [Ctrl + Shift  + D]\n",
            "devtools::load_all() [Ctrl + Shift  + L]")

    invisible(NULL)
}
