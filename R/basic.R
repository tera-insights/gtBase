Sum <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  agg <- Aggregate(data, GLA(Sum), inputs, outputs)
  agg
}

Average <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  agg <- Aggregate(data, GLA(Average), inputs, outputs)
  agg
}

Count <- function(data, outputs = count) {
  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    outputs <- "count"
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  gla <- GLA(Count)
  agg <- Aggregate(data, gla, character(), outputs)
  agg
}

Min <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  agg <- Aggregate(data, GLA(Min), inputs, outputs)
  agg
}

Max <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  agg <- Aggregate(data, GLA(Max), inputs, outputs)
  agg
}

Distinct <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  agg <- Aggregate(data, GLA(Distinct), inputs, outputs)
  agg
}

CountDistinct <- function(data, inputs = AUTO, outputs = count) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    outputs <- "count"
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  agg <- Aggregate(data, GLA(CountDistinct), inputs, outputs)
  agg
}
