Join <- function(x, xAtts, y, yAtts) {
  xAtts <- substitute(xAtts)
  yAtts <- substitute(yAtts)
  check.atts(xAtts, FALSE)
  check.atts(yAtts, FALSE)
  xAtts <- convert.atts(xAtts, x)
  yAtts <- convert.atts(yAtts, y)
  if (length(xAtts) != length(yAtts))
    stop("xAtts and yAtts must specify the same number of attributes.")
  check.schema(x, xAtts)
  check.schema(y, yAtts)

  ## Check if joining attributes have clashes with any other attributes.
  ## A direct clash is one between 2 attributes that are being matched, which is ok.
  direct <- xAtts == yAtts
  xClashed <- xAtts %in% names(y$schema) & !direct
  xJoinPassed <- x$schema[xAtts[!xClashed]]
  xClashed <- x$schema[xAtts[xClashed]]
  yClashed <- yAtts %in% names(x$schema) & !direct
  yJoinPassed <- setNames(x$schema[xAtts[!yClashed]], yAtts[!yClashed])
  yClashed <- y$schema[yAtts[yClashed]]

  xNames <- x$schema[xAtts]
  yNames <- y$schema[yAtts]

  ## Attributes passed through the join that aren't being joined on
  xPassed <- subtract(x$schema, xNames)
  yPassed <- subtract(y$schema, yNames)

  xInvisible <- xPassed[names(xPassed) %in% names(y$schema)]
  yInvisible <- yPassed[names(yPassed) %in% names(x$schema)]

  xVisible <- xPassed[names(xPassed) %nin% names(y$schema)]
  yVisible <- yPassed[names(yPassed) %nin% names(x$schema)]

  invisible <- c(xClashed, xInvisible, yClashed, yInvisible)
  visible <- c(xJoinPassed, xVisible, yJoinPassed, yVisible)

  schema <- c(visible, setNames(invisible, make.unique(names(invisible), names(c(visible, invisible)), TRUE)))

  alias <- create.alias("join")
  join <- list(alias = alias, schema = schema, x = x, y = y, xSchema = xAtts, ySchema = yAtts)
  class(join) <- c("Join", "data")
  join
}

Join2 <- function(x, xAtts, y, yAtts, yPassed) {
  xAtts <- substitute(xAtts)
  yAtts <- substitute(yAtts)
  check.atts(xAtts, FALSE)
  check.atts(yAtts, FALSE)
  xAtts <- convert.exprs(xAtts, x)
  yAtts <- convert.atts(yAtts, y)
  if (length(xAtts) != length(yAtts))
    stop("xAtts and yAtts must specify the same number of attributes.")
  check.inputs(x, xAtts)
  check.schema(y, yAtts)

  if (missing(yPassed)) {
    yPassed <- subtract(names(y$schema), c(yAtts, names(x$schema)))
  } else {
    yPassed <- substitute(yPassed)
    check.atts(yPassed, FALSE)
    yPassed <- convert.atts(yPassed)
  }

  group <- convert.schema(yAtts)
  inner <- do.call(call, c("Gather", inputs = convert.schema(yPassed)), TRUE)
  right <- eval(call("GroupBy", y, group, inner, use.mct = FALSE))
  Transform(x, GT(Join), xAtts, yPassed, list(right))
}

Gather <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  Aggregate(data, GLA(Gather), inputs, outputs)
}
