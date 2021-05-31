check_any_na <- function(x, name = deparse(substitute(x))) {
    if (any(is.na(x))) {
        paste0(single_quote_(name), " cannot have any missing values")
    } else {
        TRUE
    }
}

assert_any_na <- checkmate::makeAssertionFunction(check_any_na)

check_not_all_na <- function(x, name = deparse(substitute(x))) {
    if (all(is.na(x))) {
        paste0(single_quote_(name), " cannot have all values as missing")
    } else {
        TRUE
    }
}

assert_not_all_na <- checkmate::makeAssertionFunction(check_not_all_na)

check_length_one <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (!(length(x) == 1)) {
        paste0(single_quote_(name), " must have length 1, not length ",
               length(x))
    } else {
        TRUE
    }
}

assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

test_has_length <- function(x) {
    length(x) > 0
}

check_has_length <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (!test_has_length(x)) {
        paste0(single_quote_(name), " must have length greater than zero")
    } else {
        TRUE
    }
}

assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

check_whole_number <- function(x, any.missing = TRUE, null.ok = FALSE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else  if (!all(is_whole_number(x), na.rm = TRUE)) {
        paste0(single_quote_(name), " must consist of whole numbers")
    } else {
        TRUE
    }
}

assert_whole_number <- checkmate::makeAssertionFunction(check_whole_number)

check_posixt <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else  if (!("POSIXt" %in% class(x))) {
        paste0("Must be of type 'POSIXct' or 'POSIXlt', not ",
               class_collapse(x))
    } else {
        TRUE
    }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

assert_identical <- function(..., type = "value", any.missing = TRUE,
                            null.ok = FALSE) {
    checkmate::assert_list(list(...), min.len = 2)
    checkmate::assert_choice(type, c("value", "length", "class"))
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    names <- get_names(...)
    out <- list(...)

    if (type == "length") {
        error_message <- paste0("Assertion failed: ", inline_collapse(names),
                                " must have identical lengths.")
        check <- length(unique(vapply(out, length, integer(1)))) == 1
    } else if (type == "class") {
        error_message <- paste0("Assertion failed: ", inline_collapse(names),
                                " must have identical classes.")
        check <- length(unique(lapply(out, class))) == 1
    } else {
        error_message <- paste0("Assertion failed: ", inline_collapse(names),
                                " must be identical.")
        check <- length(unique(out)) == 1
    }

    if (any(unlist(lapply(out, is.null)), na.rm = TRUE) && isTRUE(null.ok)) {
        invisible(TRUE)
    } else if (any(is.na(unlist(out))) && isFALSE(any.missing)) {
        stop(inline_collapse(names), " cannot have missing values.",
             call. = FALSE)
    } else if (any(is.null(unlist(out)), na.rm = TRUE) && isFALSE(null.ok)) {
        stop(inline_collapse(names), " cannot have 'NULL' values.",
             call. = FALSE)
    } else if (isFALSE(check)) {
        stop(error_message, call. = FALSE)
    } else {
       invisible(TRUE)
    }
}

test_namespace <- function(x) {
    checkmate::assert_string(x)
    require_namespace(x, quietly = TRUE)
}

check_namespace <- function(x, null.ok = FALSE, name = deparse(substitute(x))) {
    checkmate::assert_string(x, null.ok = TRUE)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else if (isFALSE(test_namespace(x))) {
        paste0("There's no namespace called ",  single_quote_(x))
    } else {
        TRUE
    }
}

assert_namespace <- checkmate::makeAssertionFunction(check_namespace)

test_data <- function(data, package) {
    checkmate::assert_string(data)
    checkmate::assert_string(package)
    require_pkg("utils")

    assert_namespace(package)
    shush(utils::data(list = data, package = package, envir = environment()))

    data %in% ls()
}

assert_data <- function(data, package, alert = NULL) {
    checkmate::assert_string(data)
    checkmate::assert_string(package)

    choices <- c("gipso_1", "gipso_2")
    if (!is.null(alert)) alert <- tolower(alert)
    checkmate::assert_choice(alert, choices, null.ok = TRUE)

    alert_null <- paste0(
        "There's no ",  single_quote_(data), " data in ",
        single_quote_(package), " namespace."
        )

    alert_gipso_1 <- paste0(
        "There's no ", single_quote_(data), " data in the ",
        single_quote_(package), " package namespace. ",
        "See ?write_metadata() for instructions."
    )

    alert_gipso_2 <- paste0(
        "There's no ", single_quote_(data), " data in ",
        single_quote_(package), " package namespace. ",
        "Have you forgotten to run 'write_sheet()'?."
        )

    if (isFALSE(test_data(data, package))) {
        if (is.null(alert)) alert <- "null"
        stop(get(paste0("alert_", alert)), call. = FALSE)
    } else {
        invisible(TRUE)
    }
}

assert_interactive <- function(){
    if (!is_interactive()) {
        stop("You must be in a interactive R session to use this function",
             call. = FALSE)
    }

    invisible(TRUE)
}
