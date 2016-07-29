View <- function(data, ...) UseMethod("View")

View.default <- function(data, title) utils::View(data, title)

View.data <- function(data, ...) {
  if (!exists("grokit.jobid"))
    return(as.object(data, ...))

  expressions <- as.list(substitute(list(...)))[-1]
  atts <- name.exprs(expressions, data)

  file <- GetResult(data, "json", atts, limit = TRUE)
  code <- system2("grokit-cli", args = c("view", grokit.jobid, file))
  if (code != 0)
    stop("View not completed.")
  quit(save = "no")
}

WriteCSV <- function(data, file, ...) {
  expressions <- as.list(substitute(list(...)))[-1]
  atts <- name.exprs(expressions, data)
  invisible(GetResult(data, "csv", atts, file))
}


as.data.frame.data <- function(data, ...) {
  expressions <- as.list(substitute(list(...)))[-1]
  atts <- name.exprs(expressions, data)

  file <- GetResult(data, "csv", atts)
  result <- read.csv(file, sep = "|", stringsAsFactors = FALSE)
  if (!getOption("keep.files", TRUE))
    file.remove(file)
  result
}

as.object <- function(data, ...) {
  expressions <- as.list(substitute(list(...)))[-1]
  atts <- name.exprs(expressions, data)

  file <- GetResult(data, "json", atts)
  result <- fromJSON(file = file)
  if (!getOption("keep.files", TRUE))
    file.remove(file)
  result
}

Test <- function(data, message, ...) {
  expressions <- as.list(substitute(list(...)))[-1]
  inputs <- name.exprs(expressions, data)

  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  piggy <- Translate.Print(data, inputs, "json", "dummy")
  if (getOption("show.piggy", TRUE))
    cat(gsub("\t", "  ", piggy))
  cat(piggy, file = pgy)
  code <- system2("grokit", args = c("-t", "-e", err, "run", pgy))
  if (!getOption("keep.files", TRUE))
    file.remove(file)
  file.remove(pgy)

  if (code == 1)
    result <- "There is already a query in progress. Unable to run at this time."
  else if (code == 2)
    result <- fromJSON(file = err)$message
  else
    result <- "Passed"
  names(result) <- message
  grokit$tests <- c(grokit$tests, result)
}

GetResult <- function(data, type, inputs, result, limit = FALSE) {
  ## Creating piggy should not change any fields of grokit permanently
  copy <- as.environment(as.list(grokit, all.names = TRUE))
  on.exit(grokit <- as.environment(as.list(copy, all.names = TRUE)))

  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err") ## for the error
  if (missing(result))
    result <- paste0(file, ".", type)

  waypoints <- Translate(Process(data, list()))
  waypoints <- waypoints[order(match(names(waypoints), grokit$waypoints))]
  piggy <- paste0(Translate.ID(), Translate.Libraries(), "\n",
                  paste(c(waypoints, Translate.Print(data, inputs, type, result, limit = limit)), collapse = "\n"))

  RunQuery(piggy, pgy, err)
  result
}

RunQuery <- function(piggy, pgy, err) {
  if (getOption("show.piggy", TRUE))
    cat(paste0(gsub("\t", "  ", piggy), "\n"))
  cat(piggy, file = pgy)
  args <- c("-e", err, "run", pgy)
  offlineMode <- Sys.getenv("mode") == "offline"
  if (offlineMode) {
    args <- c("-b", args)
  }
  code <- system2("grokit", args = args)
  if (!getOption("keep.files", TRUE))
    file.remove(pgy)

  if (code == 1) {
    stop("There is already a query in progress. Unable to run at this time.")
  } else if (code == 2) {
    if (exists("grokit.jobid")) {
      error <- fromJSON(file = err)
      if (!getOption("keep.files", TRUE))
        file.remove(pgy)
      throw.error(error)
     } else {
      stop("Internal: ", fromJSON(file = err)$message)
    }
  } else {
    ## Query completed. Update schema information. Normally this would only be
    ## needed when running a DDL query, but because those are not run until after
    ## a query with actual data flow, we update the meta information every time.
    grokit$schemas <- get.schema()
  }
}

Store <- function(data, relation, ..., .overwrite = FALSE) {
  ## Basic error handling.
  if (!inherits(data, "data"))
    stop("data must be a data object.")
  if (exists("grokit.jobid"))
    stop("Store is not allowed to be called from the web interface.")

  ## The data is compacted immediately prior to the store. This is because the
  ## Store does not check if a tuple is turned off. The compacter fixes this by
  ## creating new chunks containing only tuples that are turned on.
  data <- Compact(data)

  ## A relation must be specified.
  if (missing(relation))
    stop("Store: no relation given.")

  ## The relation can either be specified as a character or as a symbol. It is
  ## converted to a character regardless.
  ischar <- tryCatch(is.character(relation) && length(relation) == 1,
                     error = identity)
  if (inherits(ischar, "error"))
    ischar <- FALSE
  if (!ischar)
    relation <- as.character(substitute(relation))
  assert(is.character(relation) && length(relation) == 1,
         "'relation' should be a name or a length-one character vector")

  ## If the specified relation does not exist, an error is thrown.
  assert(is.relation(relation),
         "invalid relation given: ", deparse(relation))

  ## The following behavior matches the attributes in the input data to those in
  ## the specified relation.
  schema <- get.attributes(relation)

  ## The user-specified matchings are processed.
  atts <- substitute(c(...))
  names <- names(atts)[-1]
  check.atts(atts)
  atts <- convert.atts(atts)

  ## These are the relation columns not explicitly matched by the user.
  missing <- setdiff(schema, names)

  ## If the user specifies attributes, the argument names must be given.
  if (length(atts) != 0 && (is.null(names) || any(names == "")))
    stop("missing attribute names")
  ## Argument names for a given matching must be a valid relation attribute.
  if (any(bad <- !(names %in% schema)))
    stop("relation attributes not found: ", paste(names[bad], collapse = ", "))
  ## The values for the given matchings must be attributes in the data.
  if (any(bad <- !(atts %in% names(data$schema))))
    stop("data attributes not found: ", paste(atts[bad], collapse = ", "))
  ## Any relation attribute not specified must match a short name in the data.
  if (any(bad <- !(missing %in% names(data$schema))))
    stop("relation attributes not filled: ", paste(missing[bad], collapse = ", "))

  ## The missing columns have the same names in both the data and the relation.
  atts <- c(atts, missing)
  names <- c(names, missing)

  ## The query creating the input data is tranlated to PIGGY.
  waypoints <- Translate(Process(data, list()))
  waypoints <- waypoints[order(match(names(waypoints), grokit$waypoints))]

  ## The PIGGY command for the Store statement is created.
  store <- paste0("STORE ", data$alias,
                  "\nAS",
                  paste0("\n\t", backtick(relation), ".", backtick(names), " = ",
                         lapply(atts, Translate.Expr.name, data),
                         collapse = ","),
                  "\nINTO ", relation, if (.overwrite) " OVERWRITE", ";\n")

  ## The entire PIGGY query is composed.
  piggy <- paste0(Translate.ID(), Translate.Libraries(), "\n",
                  paste(c(waypoints, store), collapse = "\n"))

  ## The PIGGY file is written to a file and ran.
  pgy <- tempfile("Q", fileext = ".pgy")
  cat(piggy, file = pgy)
  if (getOption("show.piggy", TRUE))
    cat(gsub("\t", "  ", piggy))
  code <- system2("grokit", args = c("-w run", pgy))

  ## The various error codes are processed.
  if (code == 1)
    stop("There is already a query in progress. Unable to run at this time.")
  else if (code != 0)
    stop("Write not completed.")
}
