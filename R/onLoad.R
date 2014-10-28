.onAttach <- function(libname, pkgname) {
  grokit <<- new.env()
  grokit$alias <- list()
  grokit$expressions <- list()
  grokit$libraries <- c("base", "statistics")
  grokit$tasks <- list()
  grokit$waypoints <- character()
  grokit$outputs <- character()
  grokit$cluster <- list()

  ## Reading the various schema
  grokit$schemas <- get.schema()

  ## These are used for the testing interface
  grokit$tests <- character()
}

.onDetach <- function(libpath) {
  rm(grokit, envir = .GlobalEnv)
}

.reset <- function() {
  grokit$alias <- list()
  grokit$expressions <- list()
  grokit$libraries <- c("base", "statistics")
  grokit$tasks <- list()
  grokit$waypoints <- character()
  grokit$outputs <- character()
  grokit$cluster <- list()
}
