Join <- function(x, xAtts, y, yAtts, left.outer = FALSE) {
  xAtts <- substitute(xAtts)
  yAtts <- substitute(yAtts)
  check.atts(xAtts)
  check.atts(yAtts)
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

  xVisible <- xPassed[!names(xPassed) %in% names(y$schema)]
  yVisible <- yPassed[!names(yPassed) %in% names(x$schema)]

  invisible <- c(xClashed, xInvisible, yClashed, yInvisible)
  visible <- c(xJoinPassed, xVisible, yJoinPassed, yVisible)

  schema <- c(visible, setNames(invisible, make.unique(names(invisible), names(c(visible, invisible)), TRUE)))

  alias <- create.alias("join")
  join <- list(alias = alias, schema = schema, x = x, y = y, xSchema = xAtts, ySchema = yAtts,
               left.outer = left.outer)
  class(join) <- c("Join", "data")
  join
}

JoinSafe <- function(x, xJoin, y, yJoin, yPass = c()) {
  xJoin <- convert.exprs(substitute(xJoin))
  yJoin <- convert.exprs(substitute(yJoin))
  yPass <- convert.exprs(substitute(yPass))

  outputs <- convert.names(yPass)
  missing <- which(outputs == "")
  exprs <- grokit$expressions[yPass[missing]]
  if (all(is.symbols(exprs)))
    outputs[missing] <- as.character(exprs)
  else
    stop("no name given for complex inputs:",
         paste("\n\t", lapply(exprs, deparse), collapse = ""))

  split <- length(yJoin)  ## The template argument for the Hash
  gla <- GLA(Multi_Hash, split)
  right <- Aggregate(y, gla, c(yJoin, yPass), character())

  Transform(x, GT(Join), xJoin, outputs, list(right))
}

Gather <- function(data, inputs, outputs, init.size = 0, use.array = FALSE) {
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
  } else {
    if (!is.null(names(inputs)))
      warning("both outputs and named inputs given. outputs used.")
    outputs <- convert.atts(substitute(outputs))
  }
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  Aggregate(data, GLA(Gather, init.size, use.array), inputs, outputs)
}

Hash <- function(data, keys, vals) {
  keys <- convert.exprs(substitute(keys))
  vals <- convert.exprs(substitute(vals))
  inputs <- c(keys, vals)

  outputs <- "state"

  gla <- GLA(Multi_Hash, split = length(keys))
  Aggregate(data, gla, inputs, outputs)
}
