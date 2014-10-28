BloomFilter <- function(data, exponent = 16, inputs = AUTO, outputs = count) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema[[1]])
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs is not allowed to be AUTO.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  agg <- Aggregate(data, GLA(BloomFilter, exponent = exponent), inputs, outputs)
  agg
}
