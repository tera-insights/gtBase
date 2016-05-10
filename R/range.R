Range <- function(data, distance, inputs, outputs, increment = 1, initial = 0) {
  if (missing(inputs))
    inputs <- convert.schema(names(data$schema))
  else
    inputs <- convert.exprs(substitute(inputs))

  if (missing(outputs)) {
    outputs <- convert.names(inputs)
    missing <- which(outputs == "")
    exprs <- grokit$expressions[inputs[missing]]
    if (all(is.symbols(exprs)))
      outputs[missing] <- as.character(exprs)
    else
      stop("no name given for complex inputs:",
           paste("\n\t", lapply(exprs, deparse), collapse = ""))
  } else {
    if (!is.null(names(inputs)))
      warning("both outputs and named inputs given. outputs used.")
    outputs <- convert.atts(substitute(outputs))
  }

  gt <- GT(Range, distance, increment, initial)

  Transform(data, gt, inputs, outputs)
}
