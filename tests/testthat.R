library(testthat)
library(sqlr)

test_check("sqlr")

# # For development use only (comment the code after use (Ctrl + Shift + C))
#
# .rs.restartR()
# devtools::install()
# devtools::document()
# devtools:test()
# devtools::check()
# covr::package_coverage()
# covr::codecov(token = "")
# pkgdown::build_site(preview = TRUE)
# pkgdown::build_favicons(overwrite = TRUE)
# pkgdown::build_article("mctq")
# pkgdown::build_reference(preview = FALSE)
# codemetar::write_codemeta()
# codemetar::give_opinions()
# urlchecker::url_check()
# goodpractice::gp()
# spelling::spell_check_package()
# spelling::update_wordlist()
# usethis::use_tidy_description()
# usethis::use_coverage()
# usethis::use_pkgdown_github_pages()
# usethis::use_github_action_check_full()
# usethis::use_github_action_check_standard()
# usethis::use_logo("./.archive/ai/hex-logo.png")
# normalizePath(readClipboard(), "/", mustWork = FALSE)
#
# # Read references
# file <- "2021-04-05 - Citations - PubMed - EN - 1-2821.txt"
# path <- "C:\\Users\\Daniel\\Desktop\\"
# file <- paste0(path, file)
# test <- synthesisr::read_ref(file)
#
# path <- "C:\\Users\\Daniel\\Desktop\\TEMP\\"
# files <- paste0(path, dir(path))
# test <- synthesisr::read_refs(files)
#
# # Normalize file names
# path <- "./inst/extdata/citation/"
# path <- "./inst/extdata/search_history/"
# new_name <- tolower(dir(path))
# new_name <- stringr::str_replace_all(new_name, " - ", "_")
# new_name <- stringr::str_replace_all(new_name, " ", "-")
# file.rename(paste0(path, dir(path)), paste0(path, new_name))
