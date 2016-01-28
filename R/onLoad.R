.onAttach <- function(libname, pkgname) {
  grokit <<- new.env()

  ## A list of alias names that have been generated. It is kept in the form of
  ## type -> vector pairs. Tame should be the type of name being generated, such
  ## as waypoint, input, output, etc. Each vector should be named and numeric,
  ## where a given name is mapped to the number of times it has been used. Each
  ## time it is used, its mapped value is increment so that the generated names
  ## are name_1, name_2, name_3, and so forth. Names should avoid ending in an
  ## underscore followed by a number to avoid clashing.
  grokit$names <- list()
  grokit$alias <- list()

  ## A list of name -> expressions of inputs.
  grokit$expressions <- list()

  ## The set of Grokit PHP/C++ libraries to load for queries being ran.
  grokit$libraries <- c("base", "statistics")

  grokit$tasks <- list()

  ## A character vector of alias names in the order of which they were created.
  grokit$waypoints <- character()

  ## A character vector of unique output names.
  grokit$outputs <- character()

  grokit$cluster <- list()

  ## Reading the various schema
  grokit$schemas <- get.schema()

  ## These are used for the testing interface
  grokit$tests <- character()

  ## Plug-ins are functions that automatically alter the query plan.
  ## See `plugins` for more information.
  grokit$plugins <- list()
  for (class in c("Filter", "Generated", "GF", "GI", "GIST", "GLA", "Load", "Join"))
    for (stage in c("before", "after"))
      grokit$plugins[[class]][[stage]] <- list()
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
