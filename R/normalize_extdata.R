#' Normalize file names from the `extdata` folder
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `normalize_extdata()` normalize file names founded in the `./inst/extdata/`
#' folder of a package. __This function must be used only with
#' [RStudio](https://www.rstudio.com/) IDE__.
#'
#' The function lowercase the names, substitute `" - "` characters for `"_"`,
#' and substitute `" "` characters for `"-"`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' normalize_files()}
normalize_extdata <- function() {
    if (!require_namespace("rstudioapi", quietly = TRUE)) {
        stop("This function requires the 'rstudioapi' package to run. ",
             "You can install it by running: \n\n",
             'install.packages("rstudioapi")' , call. = FALSE)
    }

    extdata <- paste0(rstudioapi::getActiveProject(), "/inst/extdata")
    path <- list.dirs(extdata)

    for (i in path) {
        i <- paste0(i, "/")

        if (length(list.files(i)) == 0) {
            next
        } else {
            new_name <- tolower(list.files(i))
            new_name <- stringr::str_replace_all(new_name, " - ", "_")
            new_name <- stringr::str_replace_all(new_name, " ", "-")
            file.rename(paste0(i, list.files(i)), paste0(i, new_name))
        }
    }

    invisible(NULL)
}
