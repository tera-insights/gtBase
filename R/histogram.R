Histogram <- function(data, length, normalize = FALSE, p = 1, inputs = AUTO, outputs = histogram) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  gla <- GLA(statistics::Histogram, length = length, normalize = normalize, p = p)

  Aggregate(data, gla, inputs, outputs)
}
