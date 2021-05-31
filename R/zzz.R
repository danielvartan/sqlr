.onAttach <- function(...) {
    tip <- "Learn how to use 'sqlr' at gipsousp.github.io/sqlr ."
    package_startup_message(paste(strwrap(tip), collapse = "\n"))
}
