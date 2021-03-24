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
# normalizePath(readClipboard(), "/", mustWork = FALSE)
# usethis::use_logo("./.archive/ai/hex-logo.png")

# # Normalize file names
# path <- "./inst/extdata/citation/"
# path <- "./inst/extdata/search_history/"
# new_name <- tolower(dir(path))
# new_name <- stringr::str_replace_all(new_name, " - ", "_")
# new_name <- stringr::str_replace_all(new_name, " ", "-")
# file.rename(paste0(path, dir(path)), paste0(path, new_name))
