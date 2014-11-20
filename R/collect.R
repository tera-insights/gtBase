Collect <- function(data, inputs = AUTO, outputs) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  if (!missing(outputs)) {
    outputs <- substitute(outputs)
    check.atts(outputs)
    if (is.auto(outputs))
      Stop("outputs is not allowed to be AUTO.")
    else
      outputs <- convert.atts(outputs)
    if (length(outputs) != 1)
      Stop("There must be exactly one output specified.")
  } else {
    outputs <- NULL
  }

  Aggregate(data, GLA(statistics::Collect), inputs, outputs)
}
