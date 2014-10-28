CountDistinct <- function(data, inputs = AUTO, outputs = count, init.size = 65536) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs is not allowed to be AUTO.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  gla <- GLA(CountDistinct, init.size = init.size)
  agg <- Aggregate(data, gla, inputs, outputs)
  agg
}

Distinct <- function(data, inputs = AUTO, outputs = AUTO, init.size = 65536) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (!all(is.symbols(get.exprs(inputs))))
      Stop("AUTO used illegally for outputs.")
    else
      outputs <- as.character(get.exprs(inputs))
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    Stop("There must be the same number of outputs as inputs.")

  gla <- GLA(Distinct, init.size = init.size)
  agg <- Aggregate(data, gla, inputs, outputs)
  agg
}
