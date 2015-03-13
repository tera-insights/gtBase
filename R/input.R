## relation should be a symbol literal
Load <- Read <- function(relation) {
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

ReadCSV <- function(file, attributes, skip = 0, sep = ",", simple = FALSE,
                    chunk = NULL, nullable = FALSE, nrows = 10000, ...) {
  if (!is.numeric(skip) || skip < 0 || skip != floor(skip))
    stop("skip in ReadCSV must be a non-negative integer.")
  if (length(sep) != 1 || !is.character(sep))
    stop("sep should be a length 1 character vector.")
  if (length(file) != 1 || !is.character(file))
    stop("file must be a length 1 character vector specifying the file path.")

  if (substr(file, 1, 1) != "/")
    file <- paste0(getwd(), "/", file)

  sample <- read.csv(file, sep = sep, skip = skip, header = FALSE, nrows = nrows, ...)

  attributes <- substitute(attributes)

  keys <- names(attributes)[-1]
  replace <- if (is.null(keys)) rep(TRUE, length(attributes) - 1) else keys == ""
  types <- ifelse(replace, sample[1, ], as.list(attributes)[-1])

  attributes[-1][!replace] <- as.symbols(keys[!replace])
  check.atts(attributes, FALSE)
  schema <- convert.atts(attributes)
  names(schema) <- schema

  if (length(names(sample)) < length(schema))
    stop("number of attributes specified exceeds number of data columns.")

  types <- convert.types(types)

  alias <- create.alias("read")
  gi <- GI(base::CSVReader, skip = skip, sep = sep, simple = simple, nullable = nullable)
  class(file) <- c("file")

  data <- Input(file = file, alias = alias, gi = gi, schema = schema, types = types)
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
ReadRelation <- function(file, relation, sep = ",", simple = FALSE, skip = 0, nullable = FALSE) {
  relation <- substitute(relation)
  if (!is.symbol(relation))
    stop("relation should be given as a symbol.")
  relation <- as.character(relation)

  catalog <- grokit$schemas$catalog
  relations <- unlist(lapply(catalog, function(x) {x$name}))
  if (!(relation %in% relations))
    stop("unvailable relation: ", relation)

  index <- which(relations == relation)
  schema <- unlist(lapply(catalog[[index]]$attributes, `[[`, "name"))

  alias <- create.alias("read")
  schema <- set.names(paste0(alias, ".", schema), schema)
  gi <- GI(base::CSVReader, skip = skip, sep = sep, simple = simple, nullable = nullable)
  class(file) <- c("file")

  data <- list(file = file, relation = relation, alias = alias, gi = gi, schema = schema)
  set.class(c(data, relation = relation), c("ReadRelation", class(data)))
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

  alias <- create.alias("read")

  gi <- GI(base::CSVReader, skip = 0, simple = TRUE, sep = "tab")

  data <- Input(file = file, alias = alias, gi = gi, schema = schema, types = types)
  add.class(data, "ReadFile")
}
