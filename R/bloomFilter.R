BloomFilter <- function(data, inputs = AUTO, outputs = count, exponent = 16) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  else
    inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs is not allowed to be AUTO.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  Aggregate(data, GLA(BloomFilter, exponent = exponent), inputs, outputs)
}
