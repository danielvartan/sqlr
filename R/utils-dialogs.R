dialog_line <- function(..., combined_styles = NULL,
                        space_above = TRUE, space_below = TRUE,
                        abort = FALSE) {
    styles <- c("reset", "bold", "blurred", "italic", "underline", "inverse",
                "hidden", "strikethrough")
    color <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan",
               "white", "silver")
    bg_colors <- c("bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue",
                   "bgMagenta", "bgCyan", "bgWhite")

    assert_has_length(list(...))
    checkmate::assert_subset(combined_styles, c(styles, color, bg_colors),
                             empty.ok = TRUE)
    checkmate::assert_flag(space_above)
    checkmate::assert_flag(space_below)
    checkmate::assert_flag(abort)

    if (!is_interactive()) return(999)
    if (isTRUE(abort)) return(999)

    line <- vapply(list(...), paste0, character(1), collapse = "")
    line <- paste0(line, collapse = "")
    line <- paste0(paste(strwrap(line), collapse = "\n"), " ")

    if (is_namespace_loaded("crayon")) {
        crayonize <- shush(crayon::combine_styles(combined_styles))
        line <- (crayonize(line))
    }

    if(isTRUE(space_above)) cat("\n")
    answer <- read_line(line)
    if(isTRUE(space_below)) cat("\n")

    answer
}

alert <- function(..., combined_styles = c("bold", "red"), abort = FALSE) {
    assert_has_length(list(...))
    checkmate::assert_character(combined_styles)
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    message <- vapply(list(...), paste0, character(1), collapse = "")
    message <- paste0(message, collapse = "")

    if (is_namespace_loaded("crayon")) {
        message <- crayonize(message)
    }

    message(message)

    invisible(NULL)
}

printer <- function(..., print = TRUE, clipboard = TRUE, abort = FALSE) {
    assert_has_length(list(...))
    checkmate::assert_flag(print)
    checkmate::assert_flag(clipboard)
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    if (isTRUE(clipboard) && !is_namespace_loaded("utils")) {
        stop("You need to have the 'utils' package installed ",
             "to copy to the clipboard. You can install it by running: \n \n",
             'install.packages("utils") \n', call. = FALSE)
    }

    log <- crayonize(..., combined_styles = c("bold", "red"))
    alert <- crayonize("[COPIED TO CLIPBOARD]", combined_styles = c("silver"))

    if (isTRUE(print)) cat(log, sep = "\n\n")

    if (isTRUE(clipboard)) {
        utils::writeClipboard(as.character(
            unlist(list(...), use.names = FALSE)))

        cat("\n", alert, sep = "")
    }

    invisible(NULL)
}

crayonize <- function(..., combined_styles = c("bold", "red"), abort = FALSE) {
    styles <- c("reset", "bold", "blurred", "italic", "underline", "inverse",
                "hidden", "strikethrough")
    color <- c("black", "red", "green", "yellow", "blue", "magenta", "cyan",
               "white", "silver")
    bg_colors <- c("bgBlack", "bgRed", "bgGreen", "bgYellow", "bgBlue",
                   "bgMagenta", "bgCyan", "bgWhite")

    assert_has_length(list(...))
    checkmate::assert_subset(combined_styles, c(styles, color, bg_colors))
    checkmate::assert_flag(abort)

    if (isTRUE(abort)) return(invisible(NULL))

    out <- unlist(list(...))
    # out <- vapply(list(...), paste0, character(1))

    if (is_namespace_loaded("crayon")) {
        crayonize <- shush(crayon::combine_styles(combined_styles))
        out <- vapply(out, crayonize, character(1), USE.NAMES = FALSE)
    }

    out
}
