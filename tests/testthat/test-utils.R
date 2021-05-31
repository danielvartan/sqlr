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
    expect_equal(single_quote_("test"), paste0("'", "test", "'"))
    expect_equal(single_quote_(1), paste0("'", 1, "'"))
    expect_equal(single_quote_(lubridate::dhours()),
                 paste0("'", lubridate::dhours(), "'"))
})

test_that("double_quote_() | general test", {
    expect_equal(double_quote_("test"), paste0("\"", "test", "\""))
    expect_equal(double_quote_(1), paste0("\"", 1, "\""))
    expect_equal(double_quote_(lubridate::dhours()),
                 paste0("\"", lubridate::dhours(), "\""))
})

test_that("backtick_() | general test", {
    expect_equal(backtick_("test"), paste0("`", "test", "`"))
    expect_equal(backtick_(1), paste0("`", 1, "`"))
    expect_equal(backtick_(lubridate::dhours()),
                 paste0("`", lubridate::dhours(), "`"))
})

test_that("class_collapse() | general test", {
    expect_equal(class_collapse("test"),
                 single_quote_(paste0(class("test"), collapse = "/")))
    expect_equal(class_collapse(1),
                 single_quote_(paste0(class(1), collapse = "/")))
    expect_equal(class_collapse(lubridate::dhours()),
                 single_quote_(paste0(class(lubridate::dhours()),
                                      collapse = "/")))
})

test_that("paste_collapse() | general test", {
    expect_equal(paste_collapse("test"), "test")
    expect_equal(paste_collapse(c(1, 2, 3), sep = ", ", last = ", and "),
                 "1, 2, and 3")
})

test_that("paste_collapse() | error test", {
    expect_error(paste_collapse("", 1, ""), "Assertion on 'sep' failed")
    expect_error(paste_collapse("", "", 1), "Assertion on 'last' failed")
})

test_that("inline_collapse() | general test", {
    expect_equal(inline_collapse("test", FALSE, FALSE), "test")
    expect_equal(inline_collapse("test", TRUE, FALSE),
                 paste0("'", "test", "'"))
    expect_equal(inline_collapse(c(1, 2), FALSE, FALSE),
                 paste0(1, " and ", 2))
    expect_equal(inline_collapse(c(1, 2), TRUE, FALSE),
                 paste0("'1'", " and ", "'2'"))
    expect_equal(inline_collapse(c(1, 2, 3), TRUE, TRUE),
                 paste0("'1'", ", ", "'2'", ", and ", "'3'"))
})

test_that("inline_collapse() | error test", {
    expect_error(inline_collapse("", "", TRUE),
                 "Assertion on 'single_quote' failed")
    expect_error(inline_collapse("", TRUE, ""),
                 "Assertion on 'serial_comma' failed")
})

test_that("shush() | general test", {
    expect_equal(shush("test", quiet = FALSE), "test")

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    expect_equal(shush(test(), quiet = TRUE), "test")
    expect_warning(shush(test(), quiet = FALSE), "test")
})

test_that("close_round() | general test", {
    expect_equal(close_round(1.999999, 5), 2)
    expect_equal(close_round(1.000001, 5), 1)
    expect_equal(close_round(1.001, 2), 1)
    expect_equal(close_round(1.0001, 5), 1.0001)
    expect_equal(close_round(c(1.000001, 1.999999, 1.11), 5),
                 c(1, 2, 1.11))
})

test_that("count_na() | general test", {
    expect_equal(count_na(c(1, NA, 1, NA)), 2)
})

test_that("escape_regex() | general test", {
    expect_equal(escape_regex("test.test"), "test\\.test")
})

test_that("get_names() | general test", {
    expect_equal(get_names(x, y, z), noquote(c("x", "y", "z")))
})

test_that("get_class() | general test", {
    expect_equal(get_class(1), "numeric")
    expect_equal(get_class(datasets::iris),
                 vapply(datasets::iris, function(x) class(x)[1], character(1)))
    expect_equal(get_class(list(a = 1, b = 1)),
                 vapply(list(a = 1, b = 1), function(x) class(x)[1],
                        character(1)))
})

test_that("fix_character() | general test", {
    expect_equal(fix_character(c("1   ", "   1", "", "NA")),
                 c("1", "1", NA, NA))
})

test_that("fix_character() | error test", {
    expect_error(fix_character(1), "Assertion on 'x' failed")
})

test_that("package_startup_message() | general test", {
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # is_interactive <- mctq:::is_interactive

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            package_startup_message())
    }

    # mock()
    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            package_startup_message())
    }

    # mock()
    expect_null(mock())
})

test_that("require_pkg() | general test", {
    expect_null(require_pkg("base"))
    expect_error(require_pkg("test"),
                 "This function requires the 'test' package to run. ")
    expect_error(require_pkg("test1", "test2"),
                 "This function requires the 'test1' and 'test2' packages ")

    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # require_namespace <- mctq:::require_namespace

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) TRUE,
            require_pkg("test"))
    }

    # mock()
    expect_null(mock())
})

test_that("require_pkg() | error test", {
    expect_error(require_pkg(1), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg(".test"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test."), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tes_t"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tést"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test", "test"),
                 "'...' cannot have duplicated values.")
})
