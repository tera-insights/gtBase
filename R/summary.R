Summary <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (!all(is.symbols(get.exprs(inputs))))
      Stop("AUTO illegally used for outputs.")
    else
      outputs <- c(as.character(get.exprs(inputs)), "count")
  else
    outputs <- convert.atts(outputs)

  if (length(outputs) != length(inputs) + 1)
    Stop("incorrect number of outputs specified.")

  l <- length(outputs)

  outputs <- c(paste0(outputs[-l], "_mean"),
               paste0(outputs[-l], "_std"),
               paste0(outputs[-l], "_range"),
               paste0(outputs[-l], "_min"),
               paste0(outputs[-l], "_max"),
               outputs[[l]])

  agg <- Aggregate(data, GLA(statistics::Summary), inputs, outputs)
  agg
}
