## Inputs and outputs should both be un-named character vectors.
Aggregate <- function(data, gla, inputs, outputs, states = NULL) {
  schema <- set.names(convert.outputs(outputs), outputs)
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

Transition <- function(data, gist, outputs, states) {
  alias <- create.alias("gist")

  outputs <- set.names(convert.outputs(outputs), outputs)
  schema <- data$schema
  schema[names(outputs)] <- outputs

  if (is.data(states))
    states <- list(states)

  transition <- list(data = data, alias = alias, gist = gist, schema = schema,
                     states = states)
  class(transition) <- c("GIST", "data")
}

Transform <- function(data, gt, inputs, outputs, states = NULL, overwrite = F) {
  check.inputs(data, inputs)

  alias <- create.alias("gt")

  if (any(bad <- outputs %in% names(data$schema)) && !overwrite)
    stop("cannot perform transform due to the following name clashes:\n",
         paste0("\t", atts[bad], collapse = "\n"))

  outputs <- set.names(convert.outputs(outputs), outputs)
  schema <- data$schema
  schema[names(outputs)] <- outputs

  if (is.data(states))
    states <- list(states)

  transform <- list(data = data, alias = alias, gt = gt, inputs = inputs,
                    schema = schema, states = states, outputs = outputs)
  class(transform) <- c("GT", "data")
  transform
}

Generate <- function(data, ..., overwrite = F) {
  args <- as.list(substitute(list(...)))[-1]
  atts <- names(args)
  if (is.null(atts) || any(atts == ""))
    stop("There are missing names for the generated attributes.")
  if (any(bad <- atts %in% names(data$schema)) && !overwrite)
    stop("cannot perform generation due to the following name clashes:\n",
         paste0("\t", atts[bad], collapse = "\n"))

  exprs <- unlist(lapply(args, convert.exprs, data))
  check.inputs(data, exprs)

  generated <- convert.outputs(exprs)
  schema <- data$schema
  schema[atts] <- generated

  alias <- create.alias("projection")
  generator <- list(data = data, alias = alias, schema = schema, generated = generated)
  class(generator) <- c("Generated", "data")
  generator
}

Input <- function(file, alias, gi, schema, types = NULL, relation = NULL) {
  schema <- set.names(convert.outputs(schema), schema)
  input <- list(file = file, alias = alias, gi = gi, schema = schema,
                types = types, relation = relation)
  class(input) <- c("GI", "data")
  input
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
