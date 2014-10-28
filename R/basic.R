Sum <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    outputs <- paste0("V", 0:(length(inputs) - 1))
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    Stop("There must be exactly one output specified per input.")

  agg <- Aggregate(data, GLA(Sum), inputs, outputs)
  agg
}

Average <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    outputs <- paste0("V", 0:(length(inputs) - 1))
  else
    outputs <- convert.atts(outputs)

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
    Stop("There must be exactly one output specified.")

  gla <- GLA(Count)
  agg <- Aggregate(data, gla, character(), outputs)
  agg
}
