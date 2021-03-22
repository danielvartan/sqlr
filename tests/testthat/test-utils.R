test_that("is_whole_number() | general test", {
    expect_false(is_whole_number(letters))
    expect_false(is_whole_number(datasets::iris))

    expect_false(is_whole_number(-1L))
    expect_false(is_whole_number(-55))
    expect_false(is_whole_number(1.58))

    expect_true(is_whole_number(0))
    expect_true(is_whole_number(as.integer(1)))
    expect_true(is_whole_number(as.double(11)))
    expect_true(is_whole_number(as.numeric(475)))
})

test_that("single_quote_() | general test", {
    test <- list("test", 1)

    for (i in test) {
        checkmate::expect_character(single_quote_(i))
        expect_equal(single_quote_(i), paste0("'", i, "'"))
    }
})

test_that("double_quote_() | general test", {
    test <- list("test", 1)

    for (i in test) {
        checkmate::expect_character(double_quote_(i))
        expect_equal(double_quote_(i), paste0("\"", i, "\""))
    }
})

test_that("backtick_() | general test", {
    test <- list("test", 1)

    for (i in test) {
        checkmate::expect_character(backtick_(i))
        expect_equal(backtick_(i), paste0("`", i, "`"))
    }
})

test_that("class_collapse() | general test", {
    test <- list("test", 1)

    for (i in test) {
        checkmate::expect_character(class_collapse(i))
        expect_equal(class_collapse(i),
                     single_quote_(paste0(class(i), collapse = "/")))
    }
})

test_that("paste_collapse() | general test", {
    x <- "test"
    expect_equal(paste_collapse(x), x)

    x <- c(1, 2, 3)
    sep <- ", "
    last <- ", and "
    object <- paste_collapse(x, sep = sep, last = last)
    expect_equal(object, "1, 2, and 3")

    # Error test
    expect_error(paste_collapse("", 1, ""))
    expect_error(paste_collapse("", "", 1))
})

test_that("inline_collapse() | general test", {
    x <- "test"
    single_quote <- FALSE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, x)

    x <- "test"
    single_quote <- TRUE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0("'", x, "'"))

    x <- c(1, 2)
    single_quote <- FALSE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0(1, " and ", 2))

    x <- c(1, 2)
    single_quote <- TRUE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0("'1'", " and ", "'2'"))

    x <- c(1, 2, 3)
    single_quote <- TRUE
    serial_comma <- TRUE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0("'1'", ", ", "'2'", ", and ", "'3'"))

    # Error test
    expect_error(inline_collapse("", "", TRUE))
    expect_error(inline_collapse("", TRUE, ""))
})

test_that("shush() | general test", {
    x <- "test"
    quiet <- FALSE
    expect_equal(shush(x, quiet = quiet), x)

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    quiet <- TRUE
    expect_equal(shush(test(), quiet = quiet), "test")

    quiet <- FALSE
    expect_warning(shush(test(), quiet = quiet))

    # Error test
    expect_error(inline_collapse("", "", TRUE))
    expect_error(inline_collapse("", TRUE, ""))
})

test_that("close_round() | general test", {
    x <- 1.999999
    digits <- 5
    expect_equal(close_round(x, digits = digits), 2)

    x <- 1.000001
    digits <- 5
    expect_equal(close_round(x, digits = digits), 1)

    x <- 1.001
    digits <- 2
    expect_equal(close_round(x, digits = digits), 1)

    x <- c(1.000001, 1.999999)
    digits <- 5
    expect_equal(close_round(x, digits = digits), c(1, 2))

    # Error test
    expect_error(close_round("", 1))
    expect_error(close_round(1, ""))
})

test_that("swap() | general test", {
    x <- 1
    y <- 2
    expect_equal(swap(x, y), list(x = y, y = x))

    x <- 1
    y <- TRUE
    expect_equal(swap(x, y), list(x = TRUE, y = 1))

    x <- c(1, 1)
    y <- ""
    expect_equal(swap(x, y), list(x = "", y = c(1, 1)))
})

test_that("swap_if() | general test", {
    x <- 2
    y <- 1
    condition <- "x > y"
    expect_equal(swap_if(x, y, condition = condition), list(x = y, y = x))

    x <- 1
    y <- 1
    condition <- "x > y"
    expect_equal(swap_if(x, y, condition = condition), list(x = x, y = y))

    # Error test
    expect_error(swap_if(1, 1, ""))
})

test_that("count_na() | general test", {
    x <- c(1, NA, 1, NA)
    expect_equal(count_na(x), 2)
})

test_that("escape_regex() | general test", {
    x <- "test.test"
    expect_equal(escape_regex(x), "test\\.test")
})

test_that("get_names() | general test", {
    object <- get_names(x, y, z)
    expect_equal(object, noquote(c("x", "y", "z")))
})

test_that("get_class() | general test", {
    foo <- function(x) {
        class(x)[1]
    }

    expect_equal(get_class(1), "numeric")

    x <- datasets::iris
    expect_equal(get_class(x), vapply(x, foo, character(1)))

    x <- list(a = 1, b = 1)
    expect_equal(get_class(x), vapply(x, foo, character(1)))
})

test_that("fix_character() | general test", {
    x <- c("1   ", "   1", "", "NA")
    expect_equal(fix_character(x), c("1", "1", NA, NA))

    # Error test
    expect_error(fix_character(1))
})

test_that("str_extract_() | general test", {
    string <- "test123"
    pattern <- "\\d+$"
    perl <- TRUE
    object <- str_extract_(string, pattern, perl = perl)
    expected <- regmatches(string, regexpr(pattern, string, perl = TRUE))
    expect_equal(object, expected)

    string <- "test123"
    pattern <- "^0$"
    perl <- TRUE
    object <- str_extract_(string, pattern, perl = perl)
    expected <- as.character(NA)
    expect_equal(object, expected)

    # Error test
    expect_error(str_extract_(1, 1, TRUE, TRUE, TRUE, TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, "", TRUE, TRUE, TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, "", TRUE, TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, "", TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, TRUE, "", TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, TRUE, TRUE, ""))
})

test_that("str_subset_() | general test", {
    string <- month.name
    pattern <- "^J.+"
    perl <- TRUE
    negate <- FALSE
    object <- str_subset_(string, pattern, perl = perl, negate = negate)
    expected <- subset(string, grepl(pattern, string, perl = perl))
    expect_equal(object, expected)

    string <- month.name
    pattern <- "^J.+"
    perl <- TRUE
    negate <- TRUE
    object <- str_subset_(string, pattern, perl = perl, negate = negate)
    expected <- subset(string, !grepl(pattern, string, perl = perl))
    expect_equal(object, expected)

    string <- month.name
    pattern <- "^z$"
    perl <- TRUE
    negate <- FALSE
    object <- str_subset_(string, pattern, perl = perl, negate = negate)
    expected <- as.character(NA)
    expect_equal(object, expected)

    # Error test
    expect_error(str_subset_(1, 1, TRUE, TRUE, TRUE, TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, "", TRUE, TRUE, TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, "", TRUE, TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, "", TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, TRUE, "", TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, TRUE, TRUE, ""))
})
