IntervalJoin <- function(x, xJoin, y, yJoin, yPass = c(), use.array = FALSE, keep.missing = FALSE) {
  ## Parsing the 3 sets of input expressions.
  xJoin <- convert.exprs(substitute(xJoin))
  yJoin <- convert.exprs(substitute(yJoin))
  yPass <- convert.exprs(substitute(yPass))

  ## Interpeting the names for the expressions used for the join on the right.
  join.outputs <- convert.names(yJoin)
  missing <- which(join.outputs == "")
  exprs <- grokit$expressions[yJoin[missing]]
  if (all(is.symbols(exprs)))
    join.outputs[missing] <- as.character(exprs)
  else
    stop("no name given for complex inputs:",
         paste("\n\t", lapply(exprs, deparse), collapse = ""))

  ## Interpreting names for the passed through expressions.
  pass.outputs <- convert.names(yPass)
  missing <- which(pass.outputs == "")
  exprs <- grokit$expressions[yPass[missing]]
  if (all(is.symbols(exprs)))
    pass.outputs[missing] <- as.character(exprs)
  else
    stop("no name given for complex inputs:",
         paste("\n\t", lapply(exprs, deparse), collapse = ""))

  ## There should always be 1 more join attribute on the right than the left.
  if (length(xJoin) + 1 != length(yJoin))
    stop("join attributes are mismatched.")

  outputs <- c(join.outputs, pass.outputs)

  ## GroupBy is used if there are any direct key matches.
  if (length(yJoin) > 2) {
    ## Setting up the call for the GroupBy GLA state.
    keys <- tail(yJoin, -2)
    inputs <- as.call(c(quote(c), unname(grokit$expressions[c(head(yJoin, 2), yPass)])))
    gla <- as.call(c(quote(IntervalMap), inputs = inputs, use.array = use.array))
    group <- as.call(c(quote(c), unname(grokit$expressions[keys])))
    right <- eval(call("GroupBy", data = quote(y), group = group, gla))

    Transform(x, GT(Group_By_Interval_Join, keep.missing), xJoin, outputs, list(right))
  } else {
    right <- Aggregate(y, GLA(Interval_Map, use.array), c(yJoin, yPass), character())
    Transform(x, GT(Interval_Join, keep.missing), xJoin, outputs, list(right))
  }
}

IntervalMap <- function(data, inputs, outputs = state, use.array = FALSE) {
  inputs <- convert.exprs(substitute(inputs))
  outputs <- convert.atts(substitute(outputs))
  Aggregate(data, GLA(Interval_Map, use.array), inputs, outputs)
}
