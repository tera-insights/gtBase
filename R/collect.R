Collect <- function(data, inputs = AUTO, outputs) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs)

  if (!missing(outputs)) {
    outputs <- substitute(outputs)
    check.atts(outputs)
    if (is.auto(outputs))
      stop("outputs is not allowed to be AUTO.")
    else
      outputs <- convert.atts(outputs)
    if (length(outputs) != 1)
      stop("There must be exactly one output specified.")
  } else {
    outputs <- NULL
  }

  Aggregate(data, GLA(statistics::Collect), inputs, outputs)
}
