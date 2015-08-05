Collect <- function(data, inputs, outputs, size = NULL) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  inputs <- convert.exprs(inputs)

  if (!missing(outputs)) {
    outputs <- substitute(outputs)
    check.atts(outputs)
    outputs <- convert.atts(outputs)
    if (length(outputs) != 1)
      stop("There must be exactly one output specified.")
  } else {
    outputs <- NULL
  }

  Aggregate(data, GLA(statistics::Collect, size = size), inputs, outputs)
}
