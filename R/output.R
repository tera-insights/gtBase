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

  GetResult(data, "csv", atts, file)
}


as.data.frame.data <- function(data, ...) {
  expressions <- as.list(substitute(list(...)))[-1]
  atts <- name.exprs(expressions, data)

  file <- GetResult(data, "csv", atts)
  result <- read.csv(file, sep = "|")
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

  file <- tempfile("Q", getwd(), ".")
  pgy <- paste0(file, "pgy")
  err <- paste0(file, "err")
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
  file <- tempfile("Q", getwd(), ".")
  pgy <- paste0(file, "pgy")
  err <- paste0(file, "err") ## for the error
  if (missing(result))
    result <- paste0(file, type)
  waypoints <- Translate(data)
  waypoints <- waypoints[order(match(names(waypoints), grokit$waypoints))]
  piggy <- paste0(Translate.ID(), Translate.Libraries(), "\n",
                  paste(c(waypoints, Translate.Print(data, inputs, type, result, limit = limit)), collapse = "\n"))
  run(piggy, pgy, err)
  result
}

run <- function(piggy, pgy, err) {
  if (getOption("show.piggy", TRUE))
    cat(gsub("\t", "  ", piggy))
  cat(piggy, file = pgy)
  args <- c("-e", err, "run", pgy)
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
  }
}

## TODO: Add generate, expressions, and type checking with USING clause
Store <- function(data, relation, ..., overwrite = FALSE) {
  if (!inherits(data, "data"))
    stop("data must be a data object.")
  if (exists("grokit.jobid"))
    stop("Store is not allowed to be called from the web interface.")
  relation <- substitute(relation)
  if (!is.symbol(relation))
    stop("relation should be a symbol literal naming an existing relation.")

  relation <- as.character(relation)
  catalog <- grokit$schemas$catalog
  relations <- unlist(lapply(catalog, `[[`, "name"))
  if (!(relation %in% relations))
    stop("unavailable relation: ", relation)
  index <- which(relations == relation)
  schema <- unlist(lapply(catalog[[index]]$attributes, `[[`, "name"))

  file <- tempfile("Q", ".", ".")
  pgy <- paste0(file, "pgy")
  overwrite <- if (overwrite) " OVERWRITE" else ""

  atts <- substitute(c(...))
  names <- names(atts)[-1]
  check.atts(atts)
  atts <- convert.atts(atts)

  if (length(atts) != 0 && (is.null(names) || any(names == "")))
    stop("missing attribute names")
  if (any(bad <- names %nin% schema))
    stop("relation attributes not found: ", pase(bad, collapse = ", "))
  if (any(bad <- atts %nin% names(data$schema)))
    stop("data attributes not found: ", paste(bad, collapse = ", "))
  if (any(bad <- subtract(schema, names) %nin% names(data$schema)))
    stop("relation attributes not filled: ", paste(bad, collapse = ", "))

  atts <- data$schema[c(atts, subtract(schema, names))]
  names <- c(names, subtract(schema, names))

  store <- paste0("STORE ", data$alias, "\n",
                  "AS\n",
                  paste0("\t", backtick(relation), ".", backtick(names), " = ", backtick(atts),
                         collapse = ",\n"), "\n",
                  "INTO ", relation, overwrite, ";")
  libraries <- paste0("USING ", grokit$libraries, ";", collapse = "\n")
  piggy <- paste(libraries,
                 Translate(data), "\n",
                 store,
                 sep = "\n")
  cat(piggy, file = pgy)
  if (getOption("show.piggy", TRUE))
    cat(gsub("\t", "  ", piggy))
  code <- system2("grokit", args = c("-w run", paste0(getwd(), "/", pgy)))
  if (code != 0)
    stop("Write not completed.")
}
