.onAttach <- function(...) {
    tip <- "Learn how to use 'sqlr' at gipsousp.github.io/sqlr ."
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
