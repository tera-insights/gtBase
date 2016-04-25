 #' Waypoint Construction
#'
#' These functions are responsible for constructing waypoints, the building
#' blocks for every query.
#'
#' In the GrokIt system, a waypoint is the most basic level of data processing,
#' each of which is a coupling of an operator, inputs, and outputs. Essentially,
#' each waypoint consists of a task to perform, what to perform this task on,
#' and what to produce.
Aggregate <- function(data, gla, inputs = character(), outputs = character(),
                      states = NULL) {
  schema <- setNames(convert.outputs(outputs), outputs)
  gla <- convert.args(gla, schema)
  check.inputs(data, inputs)
  alias <- create.alias("gla")

  if (is.data(states))
    states <- list(states)

  aggregate <- list(data = data, alias = alias, gla = gla, inputs = inputs,
                    schema = schema, states = states)
  class(aggregate) <- c("GLA", "data")
  aggregate
}

Cache <- function(data) {
  alias <- create.alias(paste0("cache", "_", base.name(data$alias)))
  cache <- list(alias = alias, data = data, schema = data$schema)
  class(cache) <- c("Cache", class(data))
  cache
}

Transition <- function(gist, outputs, states) {
  alias <- create.alias("gist")

  schema <- setNames(convert.outputs(outputs), outputs)

  if (is.data(states))
    states <- list(states)

  transition <- list(alias = alias, gist = gist, schema = schema, states = states)
  class(transition) <- c("GIST", "data")
  transition
}

Transform <- function(data, gt, inputs, outputs, states = NULL, overwrite = TRUE) {
  check.inputs(data, inputs)

  alias <- create.alias("gt")

  if (any(bad <- outputs %in% names(data$schema)) && !overwrite)
    stop("cannot perform transform due to the following name clashes:\n",
         paste0(outputs[bad], collapse = ", "))

  outputs <- setNames(convert.outputs(outputs), outputs)
  schema <- data$schema
  schema[names(outputs)] <- outputs

  gt <- convert.args(gt, schema)

  if (is.data(states))
    states <- list(states)

  transform <- list(data = data, alias = alias, gt = gt, inputs = inputs,
                    schema = schema, states = states, outputs = outputs)
  class(transform) <- c("GT", "data")
  transform
}

Generate <- function(data, ..., .overwrite = FALSE) {
  args <- as.list(substitute(list(...)))[-1]
  atts <- names(args)
  if (is.null(atts) || any(atts == ""))
    stop("There are missing names for the generated attributes.")
  if (any(bad <- atts %in% names(data$schema)) && !.overwrite)
    stop("cannot perform generation due to the following name clashes:\n",
         paste0("\t", atts[bad], collapse = "\n"))

  exprs <- unlist(lapply(args, convert.exprs, data))
  check.inputs(data, exprs)

  generated <- convert.outputs(exprs)
  schema <- data$schema
  schema[atts] <- generated

  alias <- create.alias("generate")
  generator <- list(data = data, alias = alias, schema = schema, generated = generated)
  class(generator) <- c("Generated", "data")
  generator
}

## schema should either be a character naming a relation or a named list of type objects
Input <- function(files, gi, outputs, chunk = NULL) {
  assert(isTRUE(is.relation(outputs)) || is.list(outputs),
         "illegal outputs argument")

  if (isTRUE(is.relation(outputs))) {
    schema <- get.attributes(outputs)
    schema <- setNames(paste0(outputs, ".", schema), schema)
  } else {
    assert(!is.null(names(outputs)) && all(names(outputs) != ""),
           "outputs has missing names.")
    schema <- names(outputs)
    schema <- setNames(convert.outputs(schema), schema)
  }

  assert(all(good <- file_test("-f", files)),
         "missing files: ", paste(files[!good], collapse = ", "))
  files <- normalizePath(files)

  gi <- convert.args(gi, schema)

  alias <- create.alias("gi")

  if (!(is.null(chunk) || (is.numeric(chunk) && length(chunk) == 1 && chunk > 0)))
    stop("chunk size should a single positive number.")
  chunk <- as.integer(chunk)

  structure(list(files = files, alias = alias, gi = gi,
                 schema = schema, outputs = outputs, chunk = chunk),
            class = c("GI", "data"))
}

Filter <- function(data, gf, inputs = character(), states = NULL) {
  check.inputs(data, inputs)

  schema <- data$schema

  if (is.data(states))
    states <- list(states)

  alias <- create.alias("gf")
  filter <- list(data = data, alias = alias, gf = gf, schema = schema,
                 inputs = inputs, states = states)
  class(filter) <- c("GF", "data")
  filter
}

#' Load a relation.
#'
#' \code{Load} loads a relation from the disc.
#'
#' An error is thrown if \code{relation} does not specify a relation that exists
#' and can be read by the user.
#'
#' \code{Read} is simply an alias for \code{Load} that exists for compatibility.
#'
#' @param relation Usually, a name or character string specifying the relation
#'   to load. A character string (enclosed in explicit single or double quotes)
#'   is always taken as the relation name.
#'
#'   If the value of \code{relation} is a length-one character vector the name
#'   of the relation is taken to be the value of the only element. Otherwise,
#'   \code{relation} must be a name or character string.
#' @return A \code{waypoint} object whose schema is determined by the
#'   relation being loaded.
Load <- function(relation) {
  ischar <- tryCatch(is.character(relation) && length(relation) == 1,
                     error = identity)
  if (inherits(ischar, "error"))
    ischar <- FALSE
  if (!ischar)
    relation <- as.character(substitute(relation))
  assert(is.character(relation) && length(relation) == 1,
         "'relation' should be a name or a length-one character vector")

  catalog <- get.catalog(relation)
  alias <- create.alias(relation)

  schema <- unlist(lapply(catalog$attributes, `[[`, "name"))
  schema <- setNames(paste0(alias, ".", schema), schema)

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


#' Basic Filtering of Waypoints
#'
#' Filter a waypoint based on a boolean expression.
#'
#' \code{condition} is evaluated for each tuple in \code{data} independently.
#' Only those tuples for which \code{condition} evaluates to TRUE are passed
#' through the filter.
#'
#' @param data A \code{\link{waypoint}}.
#' @param condition A boolean valued \code{\link{expression}}.
#' @return A \code{\link{waypoint}} with a subset of the tuples in \code{data}.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
`[.data` <- function(data, condition) {
  condition <- substitute(condition)
  if (condition[[1]] == "c")
    stop("Condition is not allowed to be a listt of expressions.")
  check.exprs(condition)
  condition <- convert.exprs(condition, data)
  check.inputs(data, condition)
  alias <- create.alias("filter")
  schema <- data$schema
  filter <- list(data = data, alias = alias, schema = schema, condition = condition)
  class(filter) <- c("Filter", "data")
  filter
}
