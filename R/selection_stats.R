#' Print statistics about the selection process
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __CAUTION__: This function must be used only with packages that follow the
#' `sqlr` system.
#'
#' `selection_stats()` prints statistics about a trial or the selection process
#' as a whole.
#'
#' The trial statistics are computed by downloading the trial table (requires an
#' internet connection). The full statistics are computed using the `reference`
#' data of a SQLR package, hence, the data of the package must be up-to-date.
#'
#' @param trial_id (optional) a string indicating the ID of the trial. This
#'   argument must be set only when the function must print a trial statistics
#'   (default: `NULL`).
#' @param clipboard (optional) a `logical` value indicating if the function must
#'   copy a Markdown version of the statistics to the clipboard. (default:
#'   `TRUE`).
#'
#' @family selection functions
#' @template param_a
#' @export
#'
#' @examples
#' \dontrun{
#' selection_stats()}
selection_stats <- function(package = gutils:::get_package_name(),
                            trial_id = NULL,
                            clipboard = TRUE) {
    checkmate::assert_string(package, null.ok = TRUE)
    checkmate::assert_string(trial_id, null.ok = TRUE)
    checkmate::assert_flag(clipboard)
    gutils:::assert_interactive()
    gutils:::assert_namespace(package)

    # R CMD Check variable bindings fix
    sheets <- reference <- criteria <- trial <- NULL

    gutils:::assert_data("criteria", package)
    utils::data("criteria", package = package, envir = environment())

    if (!is.null(trial_id)) {
        gutils:::assert_data("sheets", package)
        utils::data("sheets", package = package, envir = environment())

        choices <- names(sheets) %>%
            stringr::str_subset("^trial_") %>%
            stringr::str_remove("^trial_")

        checkmate::assert_choice(tolower(trial_id), choices)

        trial_data <- paste0("trial_", tolower(trial_id)) %>%
            read_sheet(package = package)
        trial_data <- trial_data$criteria_id

        cli::cli_h1("Statistics of the {.strong {toupper(trial_id)}} trial")
        cli::cat_line()

        out <- paste0("## Statistics of the ",
                      gutils:::backtick_(toupper(trial_id)),
                      " trial", "\n\n",
                      paste0(stats_builder(trial_data,
                                           match = criteria$criteria_id),
                             collapse = "\n"))
    } else {
        gutils:::assert_data("reference", package)
        utils::data("reference", package = package, envir = environment())

        gutils:::assert_data("trial", package)
        utils::data("trial", package = package, envir = environment())

        out <- character()

        cli::cli_h1("Statistics of the selection process")
        cli::cat_line()

        total_md <- paste0(
            "* ",
            gutils:::double_underline_(pretty_num(nrow(reference))), " ",
            "references were extracted from the information sources",
            ".", "\n")

        cli::cli_alert_info(paste0(
            "{.strong {cli::col_blue(pretty_num(nrow(reference)))}}", " ",
            "references were extracted from the information sources",
            "."))

        out <- out %>% append(paste0(
            "## Statistics of the selection process", "\n\n",
            total_md))

        cli::cli_h2("By trial")

        out <- out %>% append(paste0(
            "### By trial", "\n\n",
            paste0(stats_builder(reference$trial_id,
                                 match = trial$trial_id),
                   collapse = "\n"),
            "\n"))

        cli::cli_h2("By criteria")

        out <- out %>% append(paste0(
            "### By criteria", "\n\n",
            paste0(stats_builder(reference$criteria_id,
                                 match = criteria$criteria_id),
                   collapse = "\n")))
    }

    if (isTRUE(clipboard)) gutils:::clipboard(out, space_above = TRUE)

    invisible(NULL)
}

stats_builder <- function(x, match = NULL, last = TRUE, print = TRUE) {
    checkmate::assert_atomic_vector(x, min.len = 1)
    checkmate::assert_class(match, class(x), null.ok = TRUE)
    checkmate::assert_flag(last)
    checkmate::assert_flag(print)

    if (all(is.na(x))) {
        out <- "No statistics are avaliable yet."
        cli::cli_alert_warning("{out}")

        return(out)
    }

    out <- character()
    n_total <- length(x)
    unique <- gutils:::rm_na(unique(x))

    if (isTRUE(last)) {
        last_index <- max(which(!is.na(x)))
        last_percentage <- ((last_index / n_total) * 100) # nolint

        cli::cli_alert_info(paste0(
            "{.strong {cli::col_red(pretty_num(last_index))}}", " / ",
            "{.strong {pretty_num(n_total)}}", " (",
            "{.strong {cli::col_red(paste0(pretty_per(last_percentage)),
                '%')}}",
            ") ", "last tag index (from top to bottom)",
            ".")
        )
        cli::cat_line()
    }

    text <- c(
        "references were tagged with the ID",
        "references were tagged",
        "references were not tagged")

    if (!is.null(match)) unique <- unique[order(match(unique, match))]

    for (i in unique) {
        i_total <- length(which(x == i))
        i_percentage <- ((i_total / n_total) * 100)

        i_md <- paste0(
            "* ",
            gutils:::double_underline_(pretty_num(i_total)), " / ",
            pretty_num(n_total), " ",
            "(__", pretty_per(i_percentage), "%__)", " ",
            text[1], " ",
            gutils:::backtick_(i),
            ".")

        if (isTRUE(print)) {
            cli::cli_alert_info(paste0(
                "{.strong {cli::col_red(pretty_num(i_total))}}", " / ",
                "{.strong {pretty_num(n_total)}}", " (",
                "{.strong {cli::col_red(paste0(pretty_per(i_percentage)),
                '%')}}",
                ") ", text[1], " ",
                "{.strong {cli::col_blue(i)}}",
                "."))
        }

        out <- out %>% append(paste0(i_md))
    }

    na_total <- length(which(is.na(x)))
    na_percentage <- ((na_total / n_total) * 100) # nolint

    not_na_total <- n_total - na_total
    not_na_percentage <- ((not_na_total / n_total) * 100) # nolint

    for (i in list(list("not_na_total", "not_na_percentage", 2),
                   list("na_total", "na_percentage", 3))) {
        i_md <- paste0(
            "* ",
            gutils:::double_underline_(pretty_num(get(i[[1]]))), " / ",
            pretty_num(n_total), " ",
            "(__", pretty_per(get(i[[2]])), "%__)", " ",
            text[i[[3]]],
            ".")

        if (isTRUE(print)) {
            cli::cli_alert_info(paste0(
                "{.strong {cli::col_red(pretty_num(get(i[[1]])))}}", " / ",
                "{.strong {pretty_num(n_total)}}", " (",
                "{.strong {cli::col_red(paste0(pretty_per(get(i[[2]]))),
                    '%')}}",
                ") ", text[i[[3]]],
                "."))
        }

        out <- out %>% append(paste0(i_md))
    }

    out
}

pretty_num <- function(x, big_mark = ",", decimal_mark = ".") {
    format(x, big.mark = big_mark, decimal.mark = decimal_mark)
}

pretty_per <- function(x, big_mark = ",", decimal_mark = ".") {
    x %>%
        round(digits = 2) %>%
        format(
            big.mark = big_mark, decimal.mark = decimal_mark,
            scientific = FALSE, n.small = 2
            )
}
