test_that("check_any_na() and assert_any_na() | general test", {
    checkmate::expect_string(check_any_na(c(NA, 1)))
    expect_true(check_any_na(c(1, 1)))
    expect_equal(assert_any_na(c(1, 1)), c(1, 1))
    expect_error(assert_any_na(c(NA, 1)))
})

test_that("check_not_all_na() and assert_not_all_na() | general test", {
    checkmate::expect_string(check_not_all_na(c(NA, NA)))
    expect_true(check_not_all_na(c(1, 1)))
    expect_equal(assert_not_all_na(c(1, 1)), c(1, 1))
    expect_error(assert_not_all_na(c(NA, NA)))
})

test_that("check_length_one() and assert_length_one() | general test", {
    checkmate::expect_string(check_length_one(c(1, 1)))
    checkmate::expect_string(check_length_one(c(1, NA), any.missing = FALSE))

    expect_true(check_length_one(1))
    expect_equal(assert_length_one(1), 1)
    expect_error(assert_length_one(c(1, 1)))
})

test_that("check_has_length() and assert_has_length() | general test", {
    checkmate::expect_string(check_has_length(numeric()))
    checkmate::expect_string(check_has_length(c(1, NA), any.missing = FALSE))

    expect_true(check_has_length(1))
    expect_equal(assert_has_length(1), 1)
    expect_error(assert_has_length(numeric()))
})

test_that("check_whole_number() and assert_whole_number() | general test", {
    checkmate::expect_string(check_whole_number(c(1, 1.5)))
    checkmate::expect_string(check_whole_number(c(1, NA), any.missing = FALSE))
    checkmate::expect_string(check_whole_number(NULL, null.ok = FALSE))

    expect_true(check_whole_number(c(1, 1)))
    expect_true(check_whole_number(NULL, null.ok = TRUE))

    expect_equal(assert_whole_number(c(1, 1)), c(1, 1))
    expect_error(assert_whole_number(c(1, 1.5)))
})

test_that("check_posixt() and assert_posixt() | general test", {
    object <- c(as.POSIXct("1970-01-01 00:00:00"),
                as.POSIXct("2021-01-01 00:00:00"))

    checkmate::expect_string(check_posixt(c(1, 1)))
    checkmate::expect_string(check_posixt(c(1, NA), any.missing = FALSE))
    checkmate::expect_string(check_posixt(NULL, null.ok = FALSE))

    expect_true(check_posixt(object))
    expect_true(check_posixt(NULL, null.ok = TRUE))

    expect_equal(assert_posixt(object), object)
    expect_error(assert_posixt(c(1, 1)))
})

test_that("assert_identical() | general test", {
    expect_error(assert_identical(1))
    expect_error(assert_identical(1, type = "a"))

    expect_error(assert_identical(1, c(1, 1), type = "value"))
    expect_true(assert_identical(1, 1, type = "value"))

    expect_error(assert_identical(1, c(2, 2), type = "length"))
    expect_true(assert_identical(1, 2, type = "length"))

    expect_error(assert_identical(1, "a", type = "class"))
    expect_true(assert_identical(1, 3, type = "class"))

    expect_true(assert_identical(NULL, NULL, null.ok = TRUE))
    expect_error(assert_identical(1, NA, any.missing = FALSE))
    expect_error(assert_identical(NULL, NULL, null.ok = FALSE))
})
