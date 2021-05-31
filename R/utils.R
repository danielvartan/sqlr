is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
    if (!is.numeric(x) || !identical(x, abs(x))) {
        FALSE
    } else {
        abs(x - round(x)) < tol # Example function from `?integer`
    }
}

single_quote_ <- function(x) paste0("'", x, "'")
double_quote_ <- function(x) paste0("\"", x, "\"")
backtick_ <- function(x) paste0("`", x, "`")

enclosure <- function(x, type = "double quote") {
    choices <- c("single quote", "double quote", "round bracket",
                 "curly bracket", "square bracket")

    checkmate::assert_choice(type, choices)

    x <- as.character(x)

    if (type == "single quote") {
        paste0("'", x, "'")
    } else if (type == "double quote") {
        paste0("\"", x, "\"")
    } else if (type == "round bracket") {
        paste0("(", x, ")")
    } else if (type == "curly bracket") {
        paste0("{", x, "}")
    } else if (type == "square bracket") {
        paste0("[", x, "]")
    }
}

class_collapse <- function(x) {
    single_quote_(paste0(class(x), collapse = "/"))
}

paste_collapse <- function(x, sep = "", last = sep) {
    checkmate::assert_string(sep)
    checkmate::assert_string(last)

    if (length(x) == 1) {
        x
    } else {
        paste0(paste(x[-length(x)], collapse = sep), last, x[length(x)])
    }
}

inline_collapse <- function(x, single_quote = TRUE, serial_comma = TRUE) {
    checkmate::assert_flag(single_quote)
    checkmate::assert_flag(serial_comma)

    if (isTRUE(single_quote)) x <- single_quote_(x)

    if (length(x) <= 2 || isFALSE(serial_comma)) {
        paste_collapse(x, sep = ", ", last = " and ")
    } else {
        paste_collapse(x, sep = ", ", last = ", and ")
    }
}

shush <- function(x, quiet = TRUE) {
    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }
}

close_round <- function(x, digits = 3) {
    checkmate::assert_numeric(x)
    checkmate::assert_number(digits)

    pattern_9 <- paste0("\\.", paste(rep(9, digits), collapse = ""))
    pattern_0 <- paste0("\\.", paste(rep(0, digits), collapse = ""))

    dplyr::case_when(
        grepl(pattern_9, x) | grepl(pattern_0, x) ~ round(x),
        TRUE ~ x)
}

swap <- function(x, y) {
    a <- x
    b <- y

    x <- b
    y <- a

    list(x = x, y = y)
}

swap_if <- function(x, y, condition = "x > y") {
    choices <- c("x == y", "x < y", "x <= y", "x > y", "x >= y")
    checkmate::assert_choice(condition, choices)

    condition <- sub("x", "a", condition)
    condition <- sub("y", "b", condition)

    a <- x
    b <- y

    x <- dplyr::if_else(eval(parse(text = condition)), b, a)
    y <- dplyr::if_else(eval(parse(text = condition)), a, b)

    list(x = x, y = y)
}

count_na <- function(x) {
    length(which(is.na(x)))
}

clear_row_names <- function(x) {
    checkmate::assert_data_frame(x, min.rows = 1)
    rownames(x) <- NULL
    x
}

clear_names <- function(x) {
    names(x) <- NULL
    x
}

change_name <- function(x, new_name) {
    checkmate::assert_character(new_name, min.len = 1)
    assert_identical(names(x), new_name, type = "length")

    names(x) <- new_name

    x
}

escape_regex <- function(x) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}

get_names <- function(...) {
    out <- lapply(substitute(list(...))[-1], deparse)
    out <- vapply(out, unlist, character(1))
    out <- noquote(out)
    out <- gsub("\\\"","", out)

    out
}

get_class <- function(x) {
    foo <- function(x) {
        class(x)[1]
    }

    if (is.list(x) || is.data.frame(x)) {
        vapply(x, foo, character(1))
    } else {
        class(x)[1]
    }
}

get_package_name <- function() {
    require_pkg("rstudioapi")

    basename(rstudioapi::getActiveProject())
}

fix_character <- function(x) {
    checkmate::assert_character(x)

    x <- trimws(x)

    for (i in c("", "NA")) {
        x <- dplyr::na_if(x, i)
    }

    x
}

rm_na <- function(x) x[which(!is.na(x))]

rm_pattern <- function(x, pattern, ignore_case = TRUE) {
    x[!grepl(pattern, x, ignore.case = ignore_case)]
}

return_duplications <- function(x, rm_na = TRUE) {
    if (anyDuplicated(x) == 0) {
        NULL
    } else {
        out <- x[duplicated(x)]

        if (isTRUE(rm_na)) {
            rm_na(out)
        } else {
            x[duplicated(x)]
        }
    }
}

package_startup_message <- function(..., domain = NULL, appendLF = TRUE) {
    if (is_interactive()) {
        packageStartupMessage(..., domain = domain, appendLF = appendLF)
    }

    invisible(NULL)
}

require_pkg <- function(...) {
    out <- list(...)

    lapply(out, checkmate::assert_string,
           pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$")

    if (!identical(unique(unlist(out)), unlist(out))) {
        stop("'...' cannot have duplicated values.", call. = FALSE)
    }

    pkg <- unlist(out)
    namespace <- vapply(pkg, require_namespace, logical(1),
                        quietly = TRUE, USE.NAMES = FALSE)
    pkg <- pkg[!namespace]

    if (length(pkg) == 0) {
        invisible(NULL)
    } else {
        stop("This function requires the ", inline_collapse(pkg), " ",
             ifelse(length(pkg) == 1, "package", "packages"), " ",
             "to run. You can install ",
             ifelse(length(pkg) == 1, "it", "them"), " ",
             "by running: \n\n",
             "install.packages(",
             paste(double_quote_(pkg), collapse = ", "), ")",
             call. = FALSE)
    }
}
