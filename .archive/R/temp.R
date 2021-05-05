file <- raw_data("citation", "2021-04-27_citations_apa_en_1-1150.zip")
file <- append(file, raw_data("citation",
                              "2021-04-28_citations_ebsco_en_1-1128.zip"))
data <- read_ris(file)

# https://www.findingyourway.io/blog/2019/03/13/2019-03-13_extracting-doi-from-text/
pattern_doi <- "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"
doi <- stringr::str_extract(data$DO, pattern_doi)
duplicated_index <- which(!is.na(doi) & duplicated(doi))
duplicated_values <- return_duplications(doi)

x <- dplyr::tibble(DO = data$DO, doi = doi)
head(duplicated_index)
head(duplicated_values)
not_equal <- which(x$DO != x$doi)
