devtools::load_all()

citation_files <- raw_data("citation", quiet = TRUE)

pubmed_files <- stringr::str_subset(citation_files, "pubmed")
ebscohost_files <- stringr::str_subset(citation_files, "ebscohost")
embase_files <- stringr::str_subset(citation_files, "embase")
lilacs_es_files <- stringr::str_subset(citation_files, "lilacs_es")
lilacs_pt_files <- stringr::str_subset(citation_files, "lilacs_pt")
psycnet_files <- stringr::str_subset(citation_files, "psycnet") # failed
scielo_es_files <- stringr::str_subset(citation_files, "scielo_es")
scielo_pt_files <- stringr::str_subset(citation_files, "scielo_pt")
scopus_files <- stringr::str_subset(citation_files, "scopus")
wos_files <- stringr::str_subset(citation_files, "web-of-science")

# debugonce(synthesisr:::prep_ris)
# end_rows <- which(z_dframe$ris == "ER")

file <- "C:\\Users\\Daniel\\Desktop\\PsycNET_Export.ris"
test <- synthesisr::read_ref(file)
# test <- synthesisr::read_ref(raw_data("citation", wos_files[1]), "wos")

db <- "wos"
path <- "./inst/extdata/citation/"
choices <- c("wos", "scopus", "ovid", "asp", "synthesir")
tag_namming <- if (db %in% choices) db else "best_guess"
files <- paste0(path, get(paste0(db, "_files")))
assign(db, synthesisr::read_refs(files, tag_namming))
assign(db, clear_row_names(get(db)))

doi_match <- synthesisr::find_duplicates(get(db)$doi, method = "exact")
doi_match[!(doi_match == seq_along(doi_match))]
data_unique <- synthesisr::extract_unique_references(get(db), doi_match)

# if (db == "psycnet") {
#     data <- character()
#
#     for (i in paste0(path, get(paste0(db, "_files")))) {
#         read <- readr::read_lines(i, skip = 4)
#         data <- append(data, read)
#     }
#
#     if (data[length(data)] == "") data <- data[-length(data)]
#
#     files <- tempfile()
#     writeLines(c(data), files)
# } else {
#     files <- paste0(path, get(paste0(db, "_files")))
# }

