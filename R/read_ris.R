#' Read citations/references from RIS files
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `read_ris()` read citations/references from RIS (Research Information
#' Systems) files. When more than one file is used, the function binds the
#' citations/references from each file.
#'
#' @details
#'
#' ## `sep` argument
#'
#' The `sep` argument is only used if the citations in the RIS file have
#' duplicated tags. In those cases, `read_ris()` will join all the same tag
#' values using the `sep` value.
#'
#' Example:
#'
#' ```
#' read_ris(file, sep = " | ")
#'
#' AU  - Wang, H
#' AU  - Chen, LL
#' AU  - Shen, D
#'
#' Joined tags: AU - Wang, H | Chen, LL | Shen, D
#' ```
#'
#' ## `lookup` argument
#'
#' `read_ris()` allows you to rename and rearrange the tags from RIS files. You
#' can create your own settings for that task or use the settings provided by
#' the `sqlr` package. Use `lookup = NULL` (default) to preserve the
#' original tag names.
#'
#' To use you own settings, you will need to assign an `data.frame` object to
#' the `lookup` argument. This `data.frame` need to have the 3 columns
#' below:
#'
#' * `tag`: a `character` column indicating the RIS tag.
#' * `order`: an `integer` column, with greater than 0 values, indicating the
#' columns order.
#' * `name`: a `character` column indicating the name to replace the tag
#' indicated in the `tag` column.
#'
#' To use the settings from the `sqlr` package, choose and assign one of the
#' following values to the `lookup` argument, accordingly to the database
#' provider from which the citation file was exported. You can see the `sqlr`
#' package settings in `sqlr::ris_tags` or at <https://bit.ly/3efSgHr>.
#'
#' * `"apa"`: for [APA](http://help.psycnet.org/) (American Psychology
#' Association).
#' * `"ebsco"`: for [EBSCO](http://support.ebsco.com/help/) (Elton Bryson
#' Stephens Company).
#' * `"embase"`: for [Embase](https://bit.ly/399d14T) (Excerpta Medica
#' dataBASE)
#' * `"pubmed"`: for [PubMed](https://pubmed.ncbi.nlm.nih.gov/help/).
#' * `"scopus"`: for [Scopus](https://bit.ly/2QAylcS).
#' * `"wos"`: for [Web of Science](https://bit.ly/3sj8nsz).
#'
#' ## Reading `zip` files
#'
#' `read_ris()` also allows you to read zip compacted files. Just assign
#' the zip file in the `file` argument.
#'
#' @param file (optional) A `character` object indicating RIS or ZIP file names.
#'   If not assigned, a dialog window will be open enabling the user to search
#'   and select a file (only for interactive sessions).
#' @param sep (optional) A string indicating the separator to be used when
#'   combining values with the same tag (default: `" | "`).
#' @param lookup (optional) A string indicating the database provider from
#'   which the citation file was exported, or a `data.frame` object containing
#'   instructions on how to rename and rearrange the tags. See the Details
#'   section to learn more (default: `NULL`).
#'
#' @return A `data.frame` object with the citations/references found in the
#'   'file' argument.
#'
#' @family citation functions
#' @inheritParams tidy_keyword
#' @export
#'
#' @examples
#' \dontrun{
#' citations <- raw_data("citation")
#' file <- citations[grep("_apa_", citations)]
#' file <- raw_data("citation", file)
#'
#' read_ris(file, "apa")}
read_ris <- function(file = file.choose(), lookup = NULL, sep = " | ",
                     quiet = FALSE) {
    checkmate::assert_character(file, min.len = 1, any.missing = FALSE,
                                all.missing = FALSE)
    checkmate::assert_file_exists(file)
    checkmate::assert_multi_class(lookup, c("character", "data.frame"),
                                  null.ok = TRUE)
    checkmate::assert_string(sep)

    if (inherits(lookup, "character")) {
        choices <- c("apa", "ebsco", "embase", "pubmed", "scopus", "wos")

        checkmate::assert_choice(lookup, choices)
    } else if (inherits(lookup, "data.frame")) {
        choices <- c("tag", "order", "name")

        checkmate::assert_data_frame(lookup, min.rows = 1, min.cols = 3)
        checkmate::assert_names(names(lookup), must.include = choices)
    }

    if (length(file) > 1 ||
        any(stringr::str_detect(file, ".zip$"), na.rm = TRUE)) {
        if (any(stringr::str_detect(file, ".zip$"), na.rm = TRUE) &&
            !require_namespace("utils", quietly = TRUE)) {
            stop("This function requires the 'utils' package ",
                 'to unzip files. You can install it by running: \n\n',
                 'install.packages("utils")', call. = FALSE)
        }

        out <- dplyr::tibble()
        shush(alert("\nThis may take a while. Please be patient.\n"), quiet)

        for (i in file) {
            if (stringr::str_detect(i, ".zip$")) {
                i <- utils::unzip(i, exdir = tempdir())
            }

            data <- read_ris(i, sep = sep, lookup = lookup,
                             quiet = TRUE)
            out <- dplyr::bind_rows(out, data)
        }

        return(out)
    }

    count <- count_ris(file)

    if (count == 0) {
        stop("No citation/reference was identified in ", single_quote_(file),
             ".", call. = FALSE)
    } else if (count > 100) {
        shush(alert("\nThis may take a while. Please be patient.\n"), quiet)
    }

    chopp_ris(file) %>%
        remove_ris_header() %>%
        lapply(join_ris_solo_lines) %>%
        lapply(stringr::str_squish) %>%
        lapply(join_ris_tag_values, sep = sep) %>%
        convert_ris_to_tibble() %>%
        rename_ris(lookup, file)
}

chopp_ris <- function(file) {
    checkmate::assert_character(file, min.len = 1, any.missing = FALSE,
                                all.missing = FALSE)
    checkmate::assert_file_exists(file)

    encoding <- readr::guess_encoding(file)
    encoding <- encoding$encoding[which(encoding$confidence ==
                                            max(encoding$confidence))]

    data <- readLines(file[1], encoding = encoding, warn = FALSE)
    index_1 <- vapply(data, stringr::str_detect, logical(1),
                      pattern = "^PMID$|^PMID |^PMID-")
    index_2 <- vapply(data, stringr::str_detect, logical(1),
                      pattern = "^ER$|^ER |^ER-")

    if (length(which(index_1 == TRUE)) >= length(which(index_2 == TRUE))) {
        index <- which(index_1 == TRUE)
        between <- "left"
    } else {
        index <- which(index_2 == TRUE)
        between <- "right"
    }

    out <- cutter(data, index, between = between) %>%
        lapply(function(x) x[!stringr::str_detect(x, "^\\s*$")]) %>%
        lapply(function(x) x[!stringr::str_detect(x, "^ER$|^ER |^ER-")])

    out[!lengths(out) == 0]
}

remove_ris_header <- function(x) {
    checkmate::assert_list(x, min.len = 1)

    if (stringr::str_detect(x[[1]][1], "^[^A-Z]") &&
        stringr::str_detect(x[[1]][1],
                            "^.*[A-Z][A-Z0-9]+(?=(-\\s+|\\s+-\\s+))")) {
        x[[1]][1] <- stringr::str_remove(x[[1]][1], "^[^A-Z]+(?=[A-Z])")
    }

    pattern_tag <- "^[A-Z][A-Z0-9]+(?=(-\\s+|\\s+-\\s+))"
    first_tag <- which(stringr::str_detect(x[[1]], pattern_tag) == TRUE)[1]

    if (!first_tag == 1) {
        x[[1]] <- unlist(cutter(x[[1]], first_tag, between = "left",
                                rm_start = TRUE))
    }

    x
}

join_ris_solo_lines <- function(x) {
    checkmate::assert_character(x, min.len = 1)

    pattern_tag <- "^[A-Z][A-Z0-9]+(?=(-\\s+|\\s+-\\s+))"
    tag_detect <- stringr::str_detect(x, pattern_tag)
    tagged_elements <- which(tag_detect == TRUE)

    if (any(!tag_detect)) {
        x <- x %>%
            cutter(index = tagged_elements, between = "left") %>%
            lapply(function(i) paste(trimws(i), collapse = " ")) %>%
            unlist()
    }

    x
}

join_ris_tag_values <- function(x, sep) {
    checkmate::assert_character(x, min.len = 1)
    checkmate::assert_string(sep)

    pattern_tag <- "^[A-Z][A-Z0-9]+(?=(-\\s+|\\s+-\\s+))"
    pattern_data <- "(?<=-).*"
    tags <- stringr::str_extract(x, pattern_tag)

    if (!anyDuplicated(rm_na(tags)) == 0) {
        duplicated_tags <- unique(tags[duplicated(tags)]) %>% rm_na()

        for (i in duplicated_tags) {
            pattern <- paste0("^", i, "\\s", "|", "^", i, "-")
            index <- which(stringr::str_detect(x, pattern))
            values <- x[index]

            tag <- paste0(i, paste(rep(" ", 4 - nchar(i)), collapse = ""),
                          "- ")
            data <- paste0(trimws(stringr::str_extract(values, pattern_data)),
                           collapse = sep)

            x[index[1]] <- paste0(tag, data)
            x <- x[-index[-1]]
        }
    }

    x
}

convert_ris_to_tibble <- function(x) {
    checkmate::assert_list(x, min.len = 1)

    out <- dplyr::tibble()
    envir <- environment()

    invisible(lapply(x, bind_ris, envir = envir))

    out
}

bind_ris <- function(x, envir) {
    checkmate::assert_character(x, min.len = 1)
    checkmate::assert_environment(envir)

    pattern_tag <- "^[A-Z][A-Z0-9]+(?=(-\\s+|\\s+-\\s+))"
    pattern_data <- "(?<=-).*"

    col <- stringr::str_extract(x, pattern_tag)
    row <- trimws(stringr::str_extract(x, pattern_data))

    if (any(stringr::str_detect(col, "^ER$"), na.rm = TRUE)) {
        index <- which(stringr::str_detect(col, "^ER$"))
        col <- col[-index]
        row <- row[-index]
    }

    if (length(col) != length(row)) {
        stop("The number of RIS tags doesn't match the number of values ",
             "in at least one record.", call. = FALSE)
    }

    if (any(is.na(col))) {
        index <- which(is.na(col))
        col <- col[-index]
        row <- row[-index]
    }

    x <- data.frame(as.list(row))
    names(x) <- col

    assign("out", dplyr::bind_rows(get("out", envir = envir), x), envir = envir)

    NULL
}

rename_ris <- function(x, lookup, file) {
    checkmate::assert_data_frame(x)
    checkmate::assert_multi_class(lookup, c("character", "data.frame"),
                                  null.ok = TRUE)
    checkmate::assert_string(file)

    if (inherits(lookup, "character")) {
        choices <- c("apa", "ebsco", "embase", "pubmed", "scopus", "wos")

        checkmate::assert_choice(lookup, choices)

        code_lookup <- list(apa = "APA", ebsco = "EBSCO", embase = "Embase",
                            pubmed = "PubMed", scopus = "Scopus",
                            wos = "Web of Science")

        provider <- code_lookup[[lookup]]
        lookup <- ris_tags[[lookup]]
    } else if (inherits(lookup, "data.frame")) {
        choices <- c("tag", "order", "name")

        checkmate::assert_data_frame(lookup, min.rows = 1, min.cols = 3)
        checkmate::assert_names(names(lookup), must.include = choices)
    } else {
        return(x)
    }

    lookup <- lookup %>% dplyr::arrange(order)
    replacement_index <- which(lookup$tag %in% names(x))
    replacement_tag <- lookup$tag[replacement_index]
    replacement_name <- lookup$name[replacement_index]

    match <- unlist(lapply(replacement_tag, match, table = names(x)))
    no_match <- which(!names(x) %in% lookup$tag)
    order <- append(match, no_match)

    x <- x[order] %>%
        dplyr::rename_with(function(x) replacement_name,
                           dplyr::all_of(replacement_tag))

    if ("provider" %in% ls()) {
        if (!"provider" %in% names(x)) {
            x %>% dplyr::mutate(provider = provider,
                                file = basename(file))
        } else {
            x <- x %>%
                dplyr::relocate("provider", .after = dplyr::last(names(x))) %>%
                dplyr::mutate(file = basename(file))
        }
    } else {
        x %>% dplyr::mutate(file = basename(file))
    }
}
