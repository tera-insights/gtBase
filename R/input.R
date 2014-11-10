## relation should be a symbol literal
Read <- function(relation) {
  relation <- substitute(relation)
  if (!is.symbol(relation))
    Stop("relation should be given as a symbol.")
  relation <- as.character(relation)
  alias <- get.alias(relation)

  catalog <- grokit$schemas$catalog
  relations <- unlist(lapply(catalog, `[[`, "name"))
  if (!(relation %in% relations))
    Stop("unavailable relation ", relation)
  index <- which(relations == relation)
  catalog <- catalog[[index]]
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

ReadCSV <- function(file, attributes, types, skip = 0, sep = ",",
                    simple = FALSE, chunk = NULL, nullable = FALSE,
                    header = FALSE, nrows = 10000, ...) {
  if (!is.numeric(skip) || skip < 0 || skip != floor(skip))
    Stop("skip in ReadCSV must be a non-negative integer.")
  if (length(sep) != 1 || !is.character(sep))
    Stop("sep should be a length 1 character vector.")
  if (length(file) != 1 || !is.character(file))
   Stop("file must be a length 1 character vector specifying the file path.")

  if (substr(file, 1, 1) != "/")
    file <- paste0(getwd(), "/", file)

  attributes <- substitute(attributes)
  check.atts(attributes)
  schema <- convert.atts(attributes)

  sample <- read.csv(file, sep = sep, skip = skip, header = header, nrows = nrows, ...)
  if (length(names(sample)) != length(schema))
    Stop("number of attribute names given does not match number of data columns.")

  if (missing(types))
    types <- as.list(sample)
  else
    types <- substitute(types)
  types <- convert.types(types)

  names(schema) <- schema
  alias <- get.alias("read")
  gi <- GI(base::CSVReader, skip = skip, sep = sep, simple = simple, nullable = nullable)
  class(file) <- c("file")

  data <- list(file = file, alias = alias, gi = gi, schema = schema, types = types, chunk = chunk)
  class(data) <- c("ReadFile", "data")
  data
}

ReadFile <- function(file, gi, attributes, chunk, ...) {
  if (length(file) != 1 || !is.character(file))
    Stop("File must be a length 1 character vector specifying the file path.")
  if (substr(file, 1, 1) != "/")
    file <- paste0(getwd(), "/", file)
  attributes <- substitute(attributes)
  check.atts(attributes)
  if (!inherits(gi, "Template") || gi$type != "GI")
    Stop("invalid GI: ", deparse(gi))
  if (is.auto(attributes))
    Stop("'AUTO' is not allowed for ReadFile as the input is too general.\n",
         "It is recommended that you use ReadCSV if possible.")
  sample <- read.file(file, ...)

  schema <- convert.atts(attributes)
  names(schema) <- schema
  alias <- get.alias("read")
  class(file) <- c("file")
  data <- list(file = file, alias = alias, gi = gi, schema = schema, chunk = chunk)
  class(data) <- c("ReadFile", "data")
  data
}

## file should be a character specifying file path
## relation should be a symbol literal
ReadRelation <- function(file, relation, sep = ",", simple = FALSE) {
  relation <- substitute(relation)
  if (!is.symbol(relation))
    Stop("relation should be given as a symbol.")
  relation <- as.character(relation)

  catalog <- grokit$schemas$catalog
  relations <- unlist(lapply(catalog, function(x) {x$name}))
  if (!(relation %in% relations))
    Stop("unvailable relation: ", relation)

  index <- which(relations == relation)
  schema <- unlist(lapply(catalog[[index]]$attributes, `[[`, "name"))
  alias <- get.alias("read")
  schema <- set.names(paste0(alias, ".", schema), schema)
  gi <- GI(base::CSVReader, skip = 0, sep = ", ", simple = simple)
  class(file) <- c("file")
  data <- list(file = file, relation = relation, alias = alias, gi = gi, schema = schema)
  class(data) <- c("ReadRelation", "data")
  data
}

as.data <- function(x, types) {
  if (!is.data.frame(x))
    x <- as.data.frame(x)

  file <- tempfile("DF-", getwd(), ".csv")
  write.table(x, file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  class(file) <- c("file")

  schema <- names(x)
  names(schema) <- schema

  if (missing(types))
    types <- as.list(x)
  else
    types <- substitute(types)
  types <- convert.types(types)

  alias <- get.alias("read")

  gi <- GI(base::CSVReader, skip = 0, simple = TRUE, sep = "tab")

  data <- list(file = file, alias = alias, gi = gi, schema = schema, types = types)
  class(data) <- c("ReadFile", "data")
  data
}
