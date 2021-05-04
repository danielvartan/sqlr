file <- raw_data("citation", "2021-04-28 - Citations - APA - EN - 1-1150.zip")
file <- append(file, raw_data("citation",
                              "2021-04-28 - Citations - EBSCO - EN - 1-1128.zip"))
data <- read_ris(file)

pattern_doi <- "[^ ]*/[^ ]*"
doi <- stringr::str_extract(data$DO, pattern_doi)
duplicated_index <- which(!is.na(doi) & duplicated(doi))
duplicated_values <- return_duplications(doi)

x <- dplyr::tibble(DO = data$DO, doi = doi)
head(duplicated_index)
head(duplicated_values)
not_equal <- which(x$DO != x$doi)
