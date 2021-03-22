query <- function(provider, domain_set, constraint_set) {
    choices <- c("pubmed", "embase", "ebscohost", "apa psycnet",
                 "web of science", "scopus", "lilacs", "scielo")

    checkmate::assert_choice(tolower(provider), choices)
    checkmate::assert_string(domain_set)
    checkmate::assert_string(constraint_set)

    provider <- tolower(provider)
    d_id <- unlist(stringr::str_extract_all(domain_set, "[0-9]+"))
    c_id <- unlist(stringr::str_extract_all(constraint_set, "[0-9]+"))

    language <- constraint %>%
        dplyr::filter(constraint_id %in% as.integer(c_id),
                      class == "Intrinsic")

    if (!(nrow(language) == 1)) {
        stop("'constraint_set' must contain one, and only one, 'intrinsic' ",
             "value", call. = FALSE)
    } else {
        language <- language$name
    }

    domain <- list(logic = domain_set)

    for (i in as.integer(d_id)) {
        domain <- append(domain, list(i = paste(
            keyword_set(i, language), collapse = " OR ")))
        names(domain)[length(domain)] <- i
    }

    constraint <- constraint %>%
        dplyr::filter(constraint_id %in% as.integer(c_id),
                      class == "Filter")

    if (nrow(constraint) == 0) {
        constraint <- NULL
    } else {
        constraint <- constraint$name
    }

    if (provider == "pubmed") {
        pubmed(domain = domain, constraint = constraint)
    } else {
        NULL
    }
}

pubmed <- function(..., constraint = NULL, domain = NULL) {
    out <- unlist(list(...), use.names = FALSE)

    checkmate::assert_character(out, min.len = 1, null.ok = TRUE)
    checkmate::assert_list(domain, min.len = 2, null.ok = TRUE)

    choices <- c("title", "abstract")
    if (!is.null(constraint)) contraint <- tolower(constraint)
    checkmate::assert_choice(constraint, choices, null.ok = TRUE)

    if (is.null(out) && is.null(domain)) {
        stop("'...' or 'domain' must be assigned", call. = FALSE)
    }

    if (!is.null(out) && !is.null(domain)) {
        stop("'...' and 'domain' can't both be assigned", call. = FALSE)
    }

    constraint <- tolower(constraint)
    constraint_tags <- list(
        title = "[Title]",
        abstract = "[Title/Abstract]"
    )

    if ("abstract" %in% constraint) {
        tag <- constraint_tags$abstract
    } else if ("title" %in% constraint & !("abstract" %in% constraint)) {
        tag <- constraint_tags$title
    } else {
        tag <- ""
    }

    if (!is.null(out)) {
        if (length(out) == 1) {
            out <- paste0(tidy_keyword(out), tag)
        } else {
            out <- paste0("(", tidy_keyword(out), tag, ")")
        }

        paste(out, collapse = " OR ")
    } else {
        NULL
    }
}
