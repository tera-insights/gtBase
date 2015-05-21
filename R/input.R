#' Load a relation.
#'
#' \code{Load} loads a binary relation from the disc.
#'
#' @param relation Usually, a name or character string specifying the relation
#'   to load. A character string (enclosed in explicit single or double quotes)
#'   is always taken the relation name.
#'
#'   If the value of \code{relation} is a length-one character vector the name
#'   of the relation is taken to be the value of the only element. Otherwise,
#'   \code{relation} must be a name or character string.
#' @return A \code{\link{waypoint}} object whose schema is determined by the
#'   relation being loaded.
#' @details
#' An error is thrown if \code{relation} does not specify a relation that exists
#' and can be read by the user.
#'
#' \code{Read} is simply an alias for \code{Load} that exists for compatibility.

Load <- function(relation) {
  relation <- substitute(relation)
  if (!is.symbol(relation))
    stop("relation should be given as a symbol.")
  relation <- as.character(relation)
  catalog <- get.catalog(relation)
  alias <- create.alias(relation)

  schema <- unlist(lapply(catalog$attributes, `[[`, "name"))
  schema <- set.names(paste0(alias, ".", schema), schema)

  if (!is.null(catalog$cluster)) {
    cluster <- paste0(alias, ".", catalog$cluster)
    index <- which(schema == cluster)
    type <- catalog$attributes[[index]]$type$node_data
    if (!is.character(type)) ## dealing with templated type.
        type <- type$name
    grokit$cluster[[cluster]] <- list(lower = -Inf, upper = Inf,
                                      type = convert.typename(type))
  } else {
    cluster <- NULL
  }

  data <- list(relation = relation, alias = alias, schema = schema, cluster = cluster)
  class(data) <- c("Load", "data")
  data
}

#' @rdname Load
#' @usage Read(relation)
Read <- Load

ReadCSV <- function(files, attributes, skip = 0, sep = ",", simple = FALSE,
                    chunk = NULL, nullable = FALSE, nrows = 100, header = FALSE, ...) {
  assert(is.numeric(skip) && skip >= 0 && skip == floor(skip),
         "skip should be a non-negative integer.")
  assert(is.character(sep) && length(sep) == 1,
         "sep should be a length 1 character vector.")
  assert(is.character(files) && length(files) > 0,
         "files should be a character vector specifying the file path(s).")
  assert(is.logical(header) && length(header) == 1,
         "header should be a boolean value.")
  assert(is.numeric(nrows) && nrows >= 0 && nrows == floor(nrows),
         "nrows should be a non-negative integer.")

  sample <- read.csv(files[[1]], sep = sep, skip = skip, header = header, nrows = nrows, ...)

  skip <- skip + header

  attributes <- substitute(attributes)

  keys <- names(attributes)[-1]
  replace <- if (is.null(keys)) rep(TRUE, length(attributes) - 1) else keys == ""
  types <- ifelse(replace, sample[1, ], as.list(attributes)[-1])
  types <- convert.types(types)

  attributes[-1][!replace] <- as.symbols(keys[!replace])
  check.atts(attributes, FALSE)
  schema <- convert.atts(attributes)
  names(schema) <- schema

  assert(ncol(sample) >= length(schema),
         "number of attributes specified exceeds number of data columns.")

  if (!is.logical(nullable) || nullable)
    nullable <- lapply(nullable, function(pair) {
      list(attr = set.class(pair$attr, "attribute"), null = pair$null)
    })

  n <- if (missing(nrows)) -1 else nrows

  alias <- create.alias("read")
  gi <- GI(base::CSVReader, skip = skip, sep = sep, simple = simple, nullable = nullable, n = n)

  data <- Input(files = files, alias = alias, gi = gi, schema = schema, types = types)
  set.class(c(data, chunk = chunk), c("ReadFile", class(data)))
}

ReadFile <- function(file, gi, attributes, chunk = NULL, ...) {
  if (length(file) != 1 || !is.character(file))
    stop("File must be a length 1 character vector specifying the file path.")

  if (substr(file, 1, 1) != "/")
    file <- paste0(getwd(), "/", file)

  if (!inherits(gi, "Template") || gi$type != "GI")
    stop("invalid GI: ", deparse(gi))

  attributes <- substitute(attributes)

  keys <- names(attributes)[-1]
  replace <- if (is.null(keys)) rep(TRUE, length(attributes) - 1) else keys == ""
  types <- ifelse(replace, sample[1, ], as.list(attributes)[-1])

  attributes[-1][!replace] <- as.symbols(keys[!replace])
  check.atts(attributes, FALSE)
  schema <- convert.atts(attributes)
  names(schema) <- schema

  types <- convert.types(types)

  alias <- create.alias("read")
  class(file) <- c("file")

  data <- Input(file = file, alias = alias, gi = gi, schema = schema, chunk = chunk)
  set.class(c(data, chunk = chunk), c("ReadFile", class(data)))
}

## file should be a character specifying file path
## relation should be a symbol literal
ReadRelation <- function(files, relation, sep = ",", simple = FALSE, skip = 0, nullable = FALSE) {
  relation <- substitute(relation)
  if (!is.symbol(relation))
    stop("relation should be given as a symbol.")
  relation <- as.character(relation)

  catalog <- get.catalog(relation)
  schema <- unlist(lapply(catalog$attributes, `[[`, "name"))

  if (!is.logical(nullable) || nullable)
    nullable <- lapply(nullable, function(pair) {
      list(attr = set.class(pair$attr, "attribute"), null = pair$null)
    })

  alias <- create.alias("read")
  schema <- set.names(paste0(relation, ".", schema), schema)
  gi <- GI(base::CSVReader, skip = skip, sep = sep, simple = simple, nullable = nullable)

  set.class(list(files = files, relation = relation, alias = alias, gi = gi, schema = schema, relation = relation),
            c("ReadRelation", "data"))
}

as.data <- function(x, types) {
  if (!is.data.frame(x))
    x <- as.data.frame(x)

  file <- tempfile("DF-", fileext = ".csv")
  write.table(x, file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  class(file) <- c("file")

  schema <- names(x)
  names(schema) <- schema

  if (missing(types))
    types <- as.list(x)
  else
    types <- substitute(types)
  types <- convert.types(types)

  alias <- create.alias("read")

  gi <- GI(base::CSVReader, skip = 0, simple = TRUE, sep = "tab")

  data <- Input(file = file, alias = alias, gi = gi, schema = schema, types = types)
  add.class(data, "ReadFile")
}
