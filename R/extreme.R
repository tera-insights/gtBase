ExtremeTuples <- function(data, ..., inputs = AUTO, outputs = AUTO) {
  constructor <- ExtremeTuplesMake(...)
  exprs <- grokit$expressions[constructor$inputs]
  atts <- as.character(exprs[as.logical(lapply(exprs, is.symbol))])

  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(subtract(data$schema, atts))
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(as.logical(lapply(grokit$expressions[inputs], is.symbol))))
      outputs <- unlist(lapply(grokit$expressions[unique(inputs)], as.character))
    else
      stop("Outputs is not allowed to be AUTO if expressions are present in inputs.")
  else
    outputs <- convert.atts(outputs)

  inputs <- c(constructor$inputs, inputs)
  outputs <- c(constructor$outputs, outputs)

  if (length(inputs) != length(outputs))
    stop("The number of inputs and outputs must be the same.")

  Aggregate(data, constructor$GLA, inputs, outputs)
}

ExtremeTuplesMake <- function(...) {
  args <- as.list(substitute(list(...)))[-1]
  names <- names(args)
  ordering <- lapply(args, function(arg) {
    if (length(arg) != 2 || !is.symbol(arg[[1]]))
      stop("Error in ", deparse(arg), "\n",
           "Each input must be a call of min or max on an expression.")
    order <- arg[[1]]
    expr <- arg[[2]]
    check.exprs(expr)
    list(order, expr)
  })
  exprs <- lapply(ordering, `[[`, 2)
  directions <- as.character(lapply(ordering, `[[`, 1))

  if (!all(directions %in% c("min", "max")))
    stop("directional specifiers must be either min or max.",
         "The following are erroneous:\n",
         paste0("\t", subtract(directions, c("min", "max")), collapse = "\n"))

  atts <- unlist(lapply(exprs, convert.exprs, data = x))
  directions <- as.list(directions)
  names(directions) <- atts

  indices <-
    if (is.null(names))
      1:length(exprs)
    else
      which(names == "")
  for (i in indices)
    names[[i]] <-
      if (is.symbol(grokit$expressions[[atts[[i]]]]))
        as.character(grokit$expressions[[atts[[i]]]])
      else
        paste0("_orderAtt", i)

  list(GLA = GLA(ExtremeTuples, extremes = directions), inputs = atts, outputs = names)
}


