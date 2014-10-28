Join <- function(x, xAtts, y, yAtts) {
  xAtts <- substitute(xAtts)
  yAtts <- substitute(yAtts)
  check.atts(xAtts)
  check.atts(yAtts)
  if (is.auto(xAtts) || is.auto(yAtts))
    Stop("AUTO is not supported for joins.")
  xAtts <- convert.atts(xAtts, x)
  yAtts <- convert.atts(yAtts, y)
  if (length(xAtts) != length(yAtts))
    Stop("xAtts and yAtts must specify the same number of attributes.")
  check.schema(x, xAtts)
  check.schema(y, yAtts)

  ## Check if joining attributes have clashes with any other attributes.
  ## A direct clash is one between 2 attributes that are being matched, which is ok.
  direct <- xAtts == yAtts
  xClashed <- xAtts %in% names(y$schema) & !direct
  xJoinPassed <- x$schema[xAtts[!xClashed]]
  xClashed <- x$schema[xAtts[xClashed]]
  yClashed <- yAtts %in% names(x$schema) & !direct
  yJoinPassed <- set.names(x$schema[xAtts[!yClashed]], yAtts[!yClashed])
  yClashed <- y$schema[yAtts[yClashed]]

  xAtts <- x$schema[xAtts]
  yAtts <- y$schema[yAtts]

  ## Attributes passed through the join that aren't being joined on
  xPassed <- subtract(x$schema, xAtts)
  yPassed <- subtract(y$schema, yAtts)

  xInvisible <- xPassed[names(xPassed) %in% names(y$schema)]
  yInvisible <- yPassed[names(yPassed) %in% names(x$schema)]

  xVisible <- subtract(xPassed, xInvisible)
  yVisible <- subtract(yPassed, yInvisible)

  invisible <- c(xClashed, xInvisible, yClashed, yInvisible)
  visible <- c(xJoinPassed, xVisible, yJoinPassed, yVisible)

  schema <- c(visible, set.names(invisible, make.unique(names(invisible), names(c(visible, invisible)), TRUE)))

  alias <- get.alias("join")
  join <- list(alias = alias, schema = schema, x = x, y = y, xSchema = xAtts, ySchema = yAtts)
  class(join) <- c("Join", "data")
  join
}
