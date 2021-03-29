#' Create a query command for a database provider
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `query()` creates and returns a query command for a specific database
#' provider.
#'
#' @details
#'
#' ## `provider` argument
#'
#' `query()` works with several database providers. At the moment, valid values
#' for the `provider` argument are:
#'
#' * `"apa"`: for [APA](https://psycnet.apa.org/) (American Psychology
#' Association).
#' * `"ebsco"`: for [EBSCO](https://search.ebscohost.com/) (Elton Bryson
#' Stephens Company).
#' * `"embase"`: for [EMBASE](https://www.embase.com/) (Excerpta Medica
#' dataBASE).
#' * `"lilacs"`: for [LILACS](https://lilacs.bvsalud.org/) (Literatura
#' Latino-americana e do Caribe em Ciencias da Saude).
#' * `"pubmed"`: for [PubMed](https://pubmed.ncbi.nlm.nih.gov/).
#' * `"scielo"`: for [SciELO](https://scielo.org/) (Scientific Electronic
#' Library Online).
#' * `"scopus"`: for [Scopus](https://www.scopus.com/).
#' * `"wos"`: for [Web of Science](https://www.webofknowledge.com/).
#'
#' ## `constraint` argument
#'
#' The `constraint` argument must be a `character` object with the exact name
#' of the constraint (_e.g._, `"Title"`, `"Abstract"`) that is used in the
#' database provider (case insensitive). Also, the following alias were included
#' to help the user: title, abstract, keyword.
#'
#' You can see all constraint names available for the query function
#' [here](https://github.com/gipsousp/sqlr/blob/master/data-raw/tags.R).
#'
#' Please note that some constraints may not be available for the database
#' you're a searching. Always read the database provider documentation before
#' building your search.
#'
#' Here are the documentation links of the database providers supported by the
#' `query()` function.
#'
#' * `"apa"`: for [APA](http://help.psycnet.org/) (American Psychology
#' Association).
#' * `"ebsco"`: for [EBSCO](http://support.ebsco.com/help/) (Elton Bryson
#' Stephens Company).
#' * `"embase"`: for [EMBASE](https://bit.ly/399d14T) (Excerpta Medica
#' dataBASE)
#' * `"lilacs"`: for [LILACS](https://bvsalud.org/en/8246-2/) (Literatura
#' Latino-americana e do Caribe em Ciencias da Saude).
#' * `"pubmed"`: for [PubMed](https://pubmed.ncbi.nlm.nih.gov/help/).
#' * `"scielo"`: for [SciELO](https://bit.ly/3lJvVnQ) (Scientific Electronic
#' Library Online).
#' * `"scopus"`: for [Scopus](https://bit.ly/2QAylcS).
#' * `"wos"`: for [Web of Science](https://bit.ly/3sj8nsz).
#'
#' ## `OR` operators
#'
#' `query()` will exclude `" OR "` operators from `character` elements in `...`.
#' This is made to facilitate the keyword set construction.
#'
#' When using `"OR"` (without spaces between words) the operator will be
#' interpreted as a keyword.
#'
#' # Creating queries from multiple domain sets
#'
#' Domains sets are a group of keyword related to a subject. You can Use the
#' boolean operators `"AND`", `"NOT"`, and `"AND NOT"` between keywords in the
#' `...` argument to get a query with multiple domains. However, it's important
#' to note that a query can only have a fixed set of constraints.
#'
#' This function was not made to produce a high level of custom programming.
#' Other operators (_e.g._, `SAME`, `NEAR`, `W/n`, `PRE/n`) are not supported.
#' To go around this, you can call `query()` several times and glue the results.
#'
#' ## Keyword tidying
#'
#' `query()` uses `tidy_keyword()` to tidy your keywords for output. See
#' `tidy_keyword()` documentation to learn more about it.
#'
#' Depending on how you set up the `query()` arguments, it can generate empty
#' sets (_e.g._ like when you use `min_chars = 100`). The function will produce
#' an error in those cases.
#'
#' @param ... One or more `character` objects with keywords.
#' @param provider A string indicating the database provider name (case
#'   insensitive)
#' @param constraint (optional) A `character` object indicating the type/types
#'   of constraint for the query (case insensitive).
#' @param clipboard (optional) A `logical` value indicating if the function must
#'   copy the output to the clipboard.
#' @param print (optional) A `logical` value indicating if the function must
#'   print the output on the console window.
#'
#' @return A string with a query for the provider indicating in `provider`.
#'
#' @family keyword functions
#' @inheritParams tidy_keyword
#' @export
#'
#' @examples
#' ## __ Creating simple queries __
#'
#' query("Lorem", "Ipsum, dolor", "sit", provider = "PubMed",
#'       constraint = c("title", "abstract"), clipboard = FALSE)
#' #> (lorem[Title/Abstract]) OR (ipsum[Title/Abstract]) OR
#' #> (dolor[Title/Abstract]) OR (sit[Title/Abstract]) # Expected
#'
#' ## __ Creating queries from multiple domains __
#'
#' query("Lorem", "AND", "Ipsum", "NOT", "dolor", provider = "embase",
#'       constraint = c("title", "abstract"), clipboard = FALSE)
#' #> (lorem:ti,ab) AND (ipsum:ti,ab) NOT (dolor:ti,ab) # Expected
#'
#' ## __ A real-life query example __
#'
#' \dontrun{
#' query(keyword_set(1, "english"), "AND", keyword_set(2, "english"),
#'       provider = "wos", constraint = c("title", "abstract", "keyword"))
#' }
query <- function(..., provider, constraint = NULL, clipboard = TRUE,
                  print = TRUE, min_chars = 1, delimiter = ",",
                  enclosure = "double quote", clean_modifiers = TRUE,
                  sort = FALSE, na_rm = TRUE, duplicate_rm = TRUE) {
    choices <- c("apa", "ebsco", "embase", "lilacs", "pubmed", "scielo",
                 "scopus", "wos")

    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_choice(tolower(provider), choices)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)
    checkmate::assert_flag(clipboard)
    checkmate::assert_flag(print)
    checkmate::assert_number(min_chars, lower = 1)
    checkmate::assert_string(delimiter, null.ok = TRUE)
    checkmate::assert_string(enclosure)
    checkmate::assert_flag(clean_modifiers)
    checkmate::assert_flag(sort)
    checkmate::assert_flag(na_rm)
    checkmate::assert_flag(duplicate_rm)

    if (!is.null(constraint)) constraint <- tolower(constraint)
    x <- chopper(..., delimiter = delimiter)

    out <- builder(x = x, provider = provider, constraint = constraint,
                   min_chars = min_chars, delimiter = delimiter,
                   enclosure = enclosure, clean_modifiers = clean_modifiers,
                   sort = sort, na_rm = na_rm, duplicate_rm = duplicate_rm)

    printer(out, print = print, clipboard = clipboard)

    invisible(out)
}

apa <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$apa, constraint)
    paste_tag(x, tag = tag, type = "global", sep = ": ")
}

ebsco <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$ebsco, constraint)
    paste_tag(x, tag = tag, type = "global", sep = " ")
}

embase <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$embase, constraint)

    if (test_has_length(tag)) {
        tag <- paste0(":", paste0(tag, collapse = ","))
    } else {
        tag <- ""
    }

    paste_tag(x, tag = tag, type = "local", sep = "")
}

lilacs <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$lilacs, constraint)
    paste_tag(x, tag = tag, type = "global", sep = ":")
}

pubmed <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    if (!is.null(constraint)) {
        if (length(constraint) == 2 &&
            any(c("title", "titles") %in% constraint) &&
            any(c("abstract", "abstracts") %in% constraint)) {
            constraint <- "title/abstract"
        } else if (length(constraint) == 3 &&
                   any(c("title", "titles") %in% constraint) &&
                   any(c("abstract", "abstracts") %in% constraint) &&
                   any(c("keyword", "keywords") %in% constraint)) {
            constraint <- "title/abstract"
        }
    }

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$pubmed, constraint)
    paste_tag(x, tag = tag, type = "local", sep = "")
}

scielo <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$scielo, constraint)
    paste_tag(x, tag = tag, type = "global", sep = ":")
}

scopus <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    if (!is.null(constraint)) {
        if (length(constraint) == 2 &&
            any(c("title", "titles") %in% constraint) &&
            any(c("abstract", "abstracts") %in% constraint)) {
            constraint <- "doc title, abstract"
        } else if (length(constraint) == 3 &&
                   any(c("title", "titles") %in% constraint) &&
                   any(c("abstract", "abstracts") %in% constraint) &&
                   any(c("keyword", "keywords") %in% constraint)) {
            constraint <- "doc title, abstract, keyword"
        } else if  (length(constraint) == 4 &&
                    any(c("title", "titles") %in% constraint) &&
                    any(c("abstract", "abstracts") %in% constraint) &&
                    any(c("keyword", "keywords") %in% constraint) &&
                    any(c("author", "authors") %in% constraint)) {
            constraint <- "doc title, abstract, keyword, author"
        }
    }

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$scopus, constraint)
    paste_tag(x, tag = tag, type = "global", sep = "")
}

wos <- function(..., constraint = NULL) {
    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)

    if (!is.null(constraint)) {
        if (length(constraint) == 3 &&
                   any(c("title", "titles") %in% constraint) &&
                   any(c("abstract", "abstracts") %in% constraint) &&
                   any(c("keyword", "keywords") %in% constraint)) {
            constraint <- "topic"
        } else if  (length(constraint) == 4 &&
                    any(c("title", "titles") %in% constraint) &&
                    any(c("abstract", "abstracts") %in% constraint) &&
                    any(c("keyword", "keywords") %in% constraint) &&
                    any(c("keyword plus") %in% constraint)) {
            constraint <- "topic"
        }
    }

    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$wos, constraint)
    paste_tag(x, tag = tag, type = "global", sep = "=")
}

chopper <- function(..., delimiter = NULL) {
    x <- unlist(list(...), use.names = FALSE)

    checkmate::assert_character(x, min.len = 1)
    checkmate::assert_string(delimiter, null.ok = TRUE)

    if (is.null(delimiter)) delimiter <- ";"

    x <- x %>%
        stringr::str_squish() %>%
        rm_na() %>%
        rm_pattern(pattern = "^OR$", ignore_case = TRUE) %>%
        stringr::str_replace_all(
            stringr::regex(" OR | OR$|^OR ", ignore_case = TRUE),
            delimiter)

    pattern <- list(
        "AND" = list(name = "AND",
                     pattern = stringr::regex(" AND |^AND | AND$",
                                              ignore_case = TRUE)),
        "NOT" = list(name = "NOT",
                     pattern = stringr::regex(" NOT |^NOT | NOT$",
                                              ignore_case = TRUE)),
        "AND NOT" = list(name = "NOT",
                         pattern = stringr::regex(
                             " AND NOT |^AND NOT | AND NOT$",
                             ignore_case = TRUE))
    )

    for (i in pattern) {
        replacement <- paste0(delimiter, i$name, delimiter)
        x <- stringr::str_replace_all(x, i$pattern, replacement)
    }

    x <- unlist(strsplit(x, delimiter))
    pattern <- "^AND$|^NOT$|^AND NOT$"
    index <- grep(pattern, x, perl = TRUE)
    operators <- x[index]

    if (test_has_length(index)) {
        if (index[1] == 1 || index[length(index)] == length(x)) {
            stop("You cannot use a boolean operator in the ",
                 "start or at the end of a query.", call. = FALSE)
        }

        if (any(diff(index) == 1)) {
            stop("Boolean operators cannot follow each other.", call. = FALSE)
        }
    }

    if (test_has_length(index)) {
        out <- cutter(x, index)
        out[[length(out) + 1]] <- operators
        names(out)[length(out)] <- "operators"

        out
    } else {
        x
    }
}

builder <- function(x, provider, constraint, min_chars, enclosure, delimiter,
                    clean_modifiers, sort, na_rm, duplicate_rm) {
    checkmate::assert_multi_class(x, c("character", "list"))
    checkmate::assert_string(provider)
    checkmate::assert_character(constraint, min.len = 1, any.missing = FALSE,
                                null.ok = TRUE)
    checkmate::assert_number(min_chars, lower = 1)
    checkmate::assert_string(delimiter, null.ok = TRUE)
    checkmate::assert_flag(clean_modifiers)
    checkmate::assert_flag(sort)
    checkmate::assert_flag(na_rm)
    checkmate::assert_flag(duplicate_rm)

    if (is.list(x)) {
        out <- character()
        operators <- x$operators
        stop_message <- paste0 ("There's no keyword left after tidying. ",
                                "Check the function arguments.")

        for (i in (seq(length(x) - 1))) {
            keyword <- tidy_keyword(x[[i]], min_chars = min_chars,
                                    delimiter = delimiter,
                                    enclosure = enclosure,
                                    clean_modifiers = clean_modifiers,
                                    sort = sort, na_rm = na_rm,
                                    duplicate_rm = duplicate_rm,
                                    quiet = FALSE)

            if (test_has_length(keyword)) {
                set <- do.call(tolower(provider),
                               list(keyword,
                                    constraint = constraint))

                if (grepl(" OR ", set) || !is.null(constraint)) {
                    set <- paste0("(", set, ")")
                }

                if (!(length(operators) == 0)) {
                    out <- append(out, paste0(set, " ", operators[1], " "))
                    operators <- operators[-1]
                } else {
                    out <- append(out, set)
                }
            } else {
                stop("One of the domains (any keyword or group of keywords ",
                     "before or after an 'AND'/'NOT'/'AND NOT') has no ",
                     "keyword left after the keyword tidying process. ",
                     "Check the function arguments.", call. = FALSE)
            }
        }

        out <- paste0(out, collapse = "")

        if (out == "") {
            stop(stop_message, call. = FALSE)
        }

        out
    } else {
        keyword <- tidy_keyword(x, min_chars = min_chars,
                                delimiter = delimiter,
                                enclosure = enclosure,
                                clean_modifiers = clean_modifiers,
                                sort = sort, na_rm = na_rm,
                                duplicate_rm = duplicate_rm,
                                quiet = FALSE)

        if (test_has_length(keyword)) {
            out <- do.call(tolower(provider), list(keyword,
                                                   constraint = constraint))
        } else {
            stop(stop_message, call. = FALSE)
        }

        out
    }
}

get_tag <- function(tags, constraint) {
    checkmate::assert_list(tags, min.len = 1)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)
    checkmate::assert_subset(constraint, names(tags), empty.ok = TRUE)

    out <- character()

    if (!is.null(constraint)) {
        for (i in constraint) {
            if (any(grepl(paste0("^", i, "$"), names(tags)))) {
                out <- append(out, tags[[i]])
            }
        }
    }

    out
}

paste_tag <- function(..., tag = NULL, type = "local", sep = "") {
    x <- unlist(list(...), use.names = FALSE)

    choices <- c("local", "global")

    checkmate::assert_character(x, min.len = 1)
    checkmate::assert_character(tag, null.ok = TRUE)
    checkmate::assert_choice(type, choices)
    checkmate::assert_string(sep)

    if (type == "local") {
        if (test_has_length(tag)) {
            out <- character()

            for (i in tag) {
                y <- paste0(x, sep, i)
                # if (length(y) > 1) y <- paste0("(", y, ")")
                y <- paste(y, collapse = " OR ")

                out <- append(out, y)
            }

            paste(out, collapse = " OR ")
        } else {
            # x <- paste0("(", x, ")")
            x <- paste(x, collapse = " OR ")

            x
        }
    } else if (type == "global") {
        if (test_has_length(tag)) {
            # x <- paste0("(", x, ")")
            x <- paste(x, collapse = " OR ")
            # if (grepl(" OR ", x)) x <- paste0("(", x, ")")
            x <- paste0("(", x, ")")

            out <- character()

            for (i in tag) {
                out <- append(out, paste0(i, sep, x))
            }

            paste(out, collapse = " OR ")
        } else {
            # x <- paste0("(", x, ")")
            x <- paste(x, collapse = " OR ")

            x
        }
    }
}
