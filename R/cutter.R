#' Cut a vector into pieces
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `cutter()` cut vector objects into pieces by cutting points/indexes.
#'
#' @details
#'
#' `cutter()` can perform different kinds of cuts. Here are some examples.
#'
#' ## Cutting by index values
#'
#' ```
#' cutter(seq(10), c(3, 9))
#'
#'    cut         cut
#'     |           |
#' 1 2 3 4 5 6 7 8 9 10
#'
#' Element 1: 1, 2
#' Element 2: 4, 5, 6, 7, 8
#' Element 3: 10
#' ```
#'
#' ## Cutting between index values
#'
#' ```
#' cutter(seq(10), c(3, 9), between = "left")
#'
#'   cut         cut
#'    |           |
#' 1 2 3 4 5 6 7 8 9 10
#'
#' Element 1: 1, 2
#' Element 2: 3, 4, 5, 6, 7, 8
#' Element 3: 9, 10
#' ```
#'
#' ```
#' cutter(seq(10), c(3, 9), between = "right")
#'
#'     cut         cut
#'      |           |
#' 1 2 3 4 5 6 7 8 9 10
#'
#' Element 1: 1, 2, 3
#' Element 2: 4, 5, 6, 7, 8, 9
#' Element 3: 10
#' ```
#'
#' ## Removing tips
#'
#' ```
#' cutter(seq(20), c(7, 16), rm_start = TRUE, rm_end = TRUE)
#'
#'            cut                      cut
#'             |                        |
#' 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
#' |---------|                            |---------|
#'  start tip                               end tip
#'
#' Element 1: 8, 9, 10, 11, 12, 13, 14, 15
#' ```
#'
#' @param x An [atomic vector][checkmate::test_atomic_vector()] (_e.g._,
#'   `character`, `integer`, `numeric`, `factor`, `POSIXct`).
#' @param index An [integerish][checkmate::test_integerish()] `numeric` object
#'   or an `integer` object with the indexes/cutting points.
#' @param between (optional) A string object indicating the direction of the cut
#'   (choices: `"left"`, `"right"`). This argument only need to be assigned if
#'   the cut must be performed between the indexes values (default: `NULL`).
#' @param rm_start (optional) a `logical` value indicating if the start element
#'   of the cut must be removed (default: `FALSE`).
#' @param rm_end (optional) a `logical` value indicating if the end element
#'   of the cut must be removed (default: `FALSE`).
#'
#' @return A `list` object with the cut pieces as elements.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## Cutting by index values
#'
#' cutter(seq(10), c(3, 9))
#'
#' ## Cutting between index values
#'
#' cutter(seq(10), c(3, 9), between = "left")
#'
#' cutter(seq(10), c(3, 9), between = "right")
#'
#' ## Removing start or end tips
#'
#' cutter(seq(10), c(3, 9), rm_start = TRUE)
#'
#' cutter(seq(10), c(3, 9), rm_end = TRUE)
cutter <- function(x, index, between = NULL, rm_start = FALSE,
                   rm_end = FALSE) {
    checkmate::assert_atomic_vector(x, min.len = 1)
    checkmate::assert_integerish(index, lower = 1, upper = length(x),
                                 any.missing = FALSE, all.missing = FALSE,
                                 unique = TRUE)
    checkmate::assert_choice(between, c("left", "right"), null.ok = TRUE)
    checkmate::assert_flag(rm_start)
    checkmate::assert_flag(rm_end)

    if (is.null(between)) {
        x <- cut_by(x, index)
    } else {
        x <- cut_between(x, index, between)
    }

    if ((isTRUE(rm_start) || isTRUE(rm_end)) && length(x) == 1) {
        stop("The cut process returned just one piece. In those cases, ",
             "'rm_start' and 'rm_end' cannot be 'TRUE'", call. = FALSE)
    }

    if (isTRUE(rm_start) && !length(x) == 1) {
        x <- x[-1]
    }

    if (isTRUE(rm_end) && !length(x) == 1) {
        x <- x[-length(x)]
    }

    x
}

cut_between <- function(x, index, between) {
    checkmate::assert_atomic_vector(x, min.len = 1)
    checkmate::assert_integerish(index, lower = 1, upper = length(x),
                                 any.missing = FALSE, all.missing = FALSE,
                                 unique = TRUE)
    checkmate::assert_choice(between, c("left", "right"))

    out <- list()

    if (between == "left") {
        for (i in index) {
            i_index <- which(index == i)
            j <- length(out) + 1

            if (i == index[1]) {
                if (!i == 1) {
                    out[[1]] <- x[seq(1 , i - 1)]
                    j <- 2
                }

                if (length(index) == 1) {
                    out[[j]] <- x[seq(i , length(x))]
                } else {
                    out[[j]] <- x[seq(i, index[i_index + 1] - 1)]
                }
            } else if (i == index[length(index)]) {
                out[[j]] <- x[seq(i, length(x))]
            } else {
                out[[j]] <- x[seq(i, index[i_index + 1] - 1)]
            }
        }
    } else {
        for (i in index) {
            i_index <- which(index == i)
            j <- length(out) + 1

            if (i == index[1]) {
                out[[1]] <- x[seq(1, i)]

                if (length(index) == 1) {
                    out[[2]] <- x[seq(i + 1 , length(x))]
                }
            } else if (i == index[length(index)] && !i == length(x)) {
                out[[j]] <- x[seq(index[i_index - 1] + 1, i)]
                out[[j + 1]] <- x[seq(i + 1, length(x))]
            } else {
                out[[j]] <- x[seq(index[i_index - 1] + 1, i)]
            }
        }
    }

    out
}

cut_by <- function(x, index) {
    checkmate::assert_atomic_vector(x, min.len = 1)
    checkmate::assert_integerish(index, lower = 1, upper = length(x),
                                 any.missing = FALSE, all.missing = FALSE,
                                 unique = TRUE)

    if (index[1] == 1 || index[length(index)] == length(x)) {
        stop("When 'between = NULL', an index cannot be in the ",
             "start or at the end of an object.", call. = FALSE)
    }

    if (any(Reduce("-", index) == -1)) {
        stop("When 'between = NULL', indexes must have at least ",
             "a distance of 1 between each other.", call. = FALSE)
    }

    out <- list()

    for (i in index) {
        i_index <- which(index == i)
        j <- length(out) + 1

        if (i == index[1]) {
            out[[j]] <- x[seq(1 , i - 1)]

            if (length(index) == 1) {
                out[[j + 1]] <- x[seq(i + 1 , length(x))]
            } else {
                out[[j + 1]] <- x[seq(i + 1 , index[i_index + 1] - 1)]
            }
        } else if (i == index[length(index)]) {
            out[[j]] <- x[seq(i + 1, length(x))]
        } else {
            out[[j]] <- x[seq(i + 1, index[i_index + 1] - 1)]
        }
    }

    out
}
