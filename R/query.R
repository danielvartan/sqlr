#' Generate a query for a database provider
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `query()` returns a query for a specific database provider.
#'
#' @details
#'
#' ## `provider` argument
#'
#' `query()` works with several providers. At the moment, valid values for the
#' `provider` argument are:
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
#' The `constraint` argument must be a `character` object with the exact name of
#' the constraint (_e.g._, field, date) that is used in the database provider
#' (case insensitive). Also, the following alias were included to help the user:
#' title, abstract, keywords.
#'
#' Please see the database provider documentation to select values for this
#' argument.
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
#' * `"scopus"`: for [Scopus](https://bit.ly/3cj2kyG).
#' * `"wos"`: for [Web of Science](https://bit.ly/3sj8nsz).
#'
#' # `OR` operators
#'
#' `query()` will exclude `" OR "` operators from `character` objects in `...`.
#' This is made to facilitate the keyword set construction.
#'
#' When using `"OR"` (without spaces between words) the operator will be
#' interpreted as an keyword.
#'
#' # Creating queries from multiple domains
#'
#' You can Use "AND" or "NOT" between keywords in the `...` argument to get a
#' query with multiple domains.
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
#'   provider.
#'
#' @family keyword functions
#' @inheritParams tidy_keyword
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' query("Lorem", "Ipsum, dolor", "sit amet", provider = "PubMed",
#'       constraint = "Abstract")
query <- function(..., provider, constraint = NULL, clipboard = TRUE,
                  print = TRUE, min_chars = 1, delimiter = ",",
                  clean_modifiers = TRUE, sort = FALSE, na_rm = TRUE,
                  duplicate_rm = TRUE, quiet = FALSE) {
    choices <- c("apa", "ebsco", "embase", "lilacs", "pubmed", "scielo",
                 "scopus", "wos")

    checkmate::assert_character(unlist(list(...)), min.len = 1)
    checkmate::assert_choice(tolower(provider), choices)
    checkmate::assert_character(constraint, min.len = 1, null.ok = TRUE)
    checkmate::assert_flag(clipboard)
    checkmate::assert_flag(print)

    if (!is.null(constraint)) constraint <- tolower(constraint)
    x <- chopper(..., delimiter = delimiter)

    if (is.list(x)) {
        out <- character()
        operators <- x$operators

        for (i in (seq(length(x) - 1))) {
            keyword <- tidy_keyword(x[[i]], min_chars = min_chars,
                                    delimiter = delimiter,
                                    clean_modifiers = clean_modifiers,
                                    sort = sort, na_rm = na_rm,
                                    duplicate_rm = duplicate_rm,
                                    quiet = quiet)

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
                operators <- operators[-1]
            }
        }

        out <- paste0(out, collapse = "")

        if (out == "") {
            stop("There is no keyword left after tidying. Check the function ",
                 "parameters.", call. = FALSE)
        }
    } else {
        keyword <- tidy_keyword(x, min_chars = min_chars,
                                delimiter = delimiter,
                                clean_modifiers = clean_modifiers,
                                sort = sort, na_rm = na_rm,
                                duplicate_rm = duplicate_rm,
                                quiet = quiet)

        if (test_has_length(keyword)) {
            out <- do.call(tolower(provider), list(keyword,
                                                   constraint = constraint))
        } else {
            stop("There is no keyword left after tidying. Check the function ",
                 "parameters.", call. = FALSE)
        }
    }

    printer(out, print = print, clipboard = clipboard)

    invisible(out)
}

chopper <- function(..., delimiter = NULL) {
    x <- unlist(list(...), use.names = FALSE)

    checkmate::assert_character(x, min.len = 1)
    checkmate::assert_string(delimiter, null.ok = TRUE)

    if (is.null(delimiter)) delimiter <- ";"
    x <- x[which(!is.na(x))]
    x <- gsub(" OR ", delimiter, x)

    for (i in c(" AND ", " NOT ")) {
        replacement <- paste0(delimiter, trimws(i), delimiter)
        x <- gsub(i, replacement, x)
    }

    x <- unlist(strsplit(x, delimiter))
    pattern <- "^AND$|^NOT$"
    index <- grep(pattern, x, perl = TRUE)
    operators <- x[index]

    if (test_has_length(index)) {
        if (index[1] == 1 || index[length(index)] == length(x)) {
            stop("You cannot use a boolean operator in the ",
                 "start or at the end of a query.", call. = FALSE)
        }

        if (any(Reduce("-", index) == -1)) {
            stop("Boolean operators can't follow each other.", call. = FALSE)
        }
    }

    if (test_has_length(index)) {
        out <- list()

        for (i in index) {
            i_index <- grep(i, index)
            j <- length(out) + 1

            if (i == index[1]) {
                out[[j]] <- x[seq(1 , i - 1)]
                names(out)[j] <- j

                if (length(index) == 1) {
                    out[[j + 1]] <- x[seq(i + 1 , length(x))]
                    names(out) <- c(1, 2)
                } else {
                    out[[j + 1]] <- x[seq(i + 1 , index[i_index + 1] - 1)]
                    names(out) <- c(1, 2)
                }
            } else if (i == index[length(index)]) {
                out[[j]] <- x[seq(i + 1, length(x))]
                names(out)[j] <- j
            } else {
                out[[j]] <- x[seq(i + 1, index[i_index + 1] - 1)]
                names(out)[j] <- j
            }
        }

        out[[length(out) + 1]] <- operators
        names(out)[length(out)] <- "operators"

        out
    } else {
        x
    }
}

get_tag <- function(tags, constraint) {
    checkmate::assert_list(tags, min.len = 1)
    checkmate::assert_character(constraint, min.len = 1)
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
    checkmate::assert_character(tag, min.len = 1, null.ok = TRUE)
    checkmate::assert_choice(type, choices)
    checkmate::assert_string(sep)

    if (type == "local") {
        if (test_has_length(tag)) {
            out <- character()

            for (i in tag) {
                y <- paste0(x, sep, i)
                if (length(y) > 1) y <- paste0("(", y, ")")
                y <- paste(y, collapse = " OR ")

                out <- append(out, y)
            }

            paste(out, collapse = " OR ")
        } else {
            out <- paste0("(", x, ")")
            out <- paste(x, collapse = " OR ")

            out
        }
    } else {
        if (test_has_length(tag)) {
            x <- paste0("(", x, ")")
            x <- paste(x, collapse = " OR ")
            if (grepl(" OR ", x)) x <- paste0("(", x, ")")

            out <- character()

            for (i in tag) {
                out <- append(out, paste0(i, sep, x))
            }

            out <- paste(out, collapse = " OR ")
        } else {
            x <- paste0("(", x, ")")
            x <- paste(x, collapse = " OR ")

            out <- x
        }

        out
    }
}

pubmed <- function(..., constraint = NULL) {
    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$pubmed, constraint)

    checkmate::assert_character(x, min.len = 1)

    paste_tag(x, tag = tag, type = "local", sep = "")
}

embase <- function(..., constraint = NULL) {
    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$embase, constraint)

    checkmate::assert_character(x, min.len = 1)

    if (test_has_length(tag)) {
        tag <- paste0(":", paste0(tag, collapse = ","))
    } else {
        tag <- ""
    }

    paste_tag(x, tag = tag, type = "local", sep = "")
}

ebsco <- function(..., constraint = NULL) {
    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$ebsco, constraint)

    checkmate::assert_character(x, min.len = 1)

    paste_tag(x, tag = tag, type = "global", sep = " ")
}

lilacs <- function(..., constraint = NULL) {
    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$lilacs, constraint)

    checkmate::assert_character(x, min.len = 1)

    paste_tag(x, tag = tag, type = "global", sep = ":")
}

scielo <- function(..., constraint = NULL) {
    x <- unlist(list(...), use.names = FALSE)
    tag <- get_tag(sqlr::tags$scielo, constraint)

    checkmate::assert_character(x, min.len = 1)

    paste_tag(x, tag = tag, type = "global", sep = ":")
}
