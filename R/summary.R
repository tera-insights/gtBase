Summary <- function(data, inputs = AUTO, outputs = AUTO) {
  if (missing(inputs)) {
    inputs <- convert.schema(names(data$schema))
  } else {
    inputs <- substitute(inputs)
    check.exprs(inputs)
    inputs <- convert.exprs(inputs)
  }

  if (missing(outputs)) {
    outputs <- convert.names(inputs)
    missing <- which(outputs == "")
    exprs <- grokit$expressions[inputs[missing]]
    if (all(is.symbols(exprs)))
      outputs[missing] <- as.character(exprs)
    else
      stop("no name given for complex inputs:",
           paste("\n\t", lapply(exprs, deparse), collapse = ""))
    outputs <- c(outputs, "count")
  } else {
    if (!is.null(names(inputs)))
      warning("both outputs and named inputs given. outputs used.")
    outputs <- convert.atts(substitute(outputs))
  }

  if (length(outputs) != length(inputs) + 1)
    stop("incorrect number of outputs specified.")

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
