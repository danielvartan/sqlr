# library(googlesheets4)
# library(revtools)
# library(stringr)

devtools::load_all()

pubmed_files <- stringr::str_subset(raw_data(), "PubMed")
ebscohost_files <- stringr::str_subset(raw_data(), "EBSCOhost")
embase_files <- stringr::str_subset(raw_data(), "Embase")
lilacs_es_files <- stringr::str_subset(raw_data(), "LILACS - ES") # failed
lilacs_pt_files <- stringr::str_subset(raw_data(), "LILACS - PT") # failed
psycnet_files <- stringr::str_subset(raw_data(), "PsycNET") # failed
scielo_es_files <- stringr::str_subset(raw_data(), "SciELO - ES")
scielo_pt_files <- stringr::str_subset(raw_data(), "SciELO - PT")
scopus_files <- stringr::str_subset(raw_data(), "Scopus")
wos_files <- stringr::str_subset(raw_data(), "Web Of Science")

db <- "pubmed"
path <- "./inst/extdata/citation/"
files <- paste0(path, get(paste0(db, "_files")))
assign(db, revtools::read_bibliography(files))

doi_match <- revtools::find_duplicates(get(db), match_variable = "doi",
                                        match_function = "exact")
data_unique <- revtools::extract_unique_references(get(db), doi_match)

id <- "1Re3lLhjbpBmYBVCMdt37-ZwAOKmWnPdMEJD8J7Sc86k"
sheet <- "Export"
test <- googlesheets4::read_sheet(id, sheet = sheet)

ss <- "15cleOLZ9QB-vZPQ1I4bhbkXGNmpDLY5s33vLI-8Y-DA"
sheet <- "test"
data <- dplyr::tibble(a = 1:10, b = 11:20)
# googlesheets4::range_write(ss, data, sheet, "A1")

id <- googlesheets4::gs4_get(ss)
googlesheets4::range_write(id, data, sheet, "A1")
googlesheets4::sheet_write(chickwts, id)
googlesheets4::sheet_append(id, data.frame(a = 30, b = 20), sheet = "test")
