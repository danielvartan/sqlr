# Source the file before running the functions

# library(checkmate)
# library(stringr)
# library(usethis)


sheets <- function(write = FALSE) {
    sheets <- list(
        domain = list(
            name = "domain",
            id = "16B-JH-3QiqPPdFIiYHssKAZKFe8ehYtYVuFqFH8MnrU",
            sheet = "Dataset"),
        constraint = list(
            name = "constraint",
            id = "1Ikw73ifT09CoPaWOc7avugybEsbQ7HIkO_Fn7R0WSrU",
            sheet = "Dataset"),
        keyword = list(
            name = "keyword",
            id = "1Re3lLhjbpBmYBVCMdt37-ZwAOKmWnPdMEJD8J7Sc86k",
            sheet = "Dataset"),
        source = list(
            name = "source",
            id = "1Ef0wzRMjTcibYwWQ0T0Ap2s3c07A-DiGqCSduUSxlMY",
            sheet = "Dataset"),
        search = list(
            name = "search",
            id = "1yZCRt3G94JS73T55rGCxdgUHrKsM6QX6RHSeRUxe3J4",
            sheet = "Dataset")
    )

    if (isTRUE(write)) {
        if(!(dir.exists("./data/"))) dir.create("./data/")

        usethis::use_data(sheets, overwrite = TRUE)
    }

    invisible(sheets)
}

# sheets <- sheets()
