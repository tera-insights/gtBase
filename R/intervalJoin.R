IntervalJoin <- function(x, xJoin, y, yJoin, yPass = c(), use.array = FALSE, keep.missing = FALSE) {
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

  gla <- GLA(Interval_Map, use.array)
  right <- Aggregate(y, gla, c(yJoin, yPass), character())

  Transform(x, GT(Interval_Join, keep.missing), xJoin, outputs, list(right))
}
