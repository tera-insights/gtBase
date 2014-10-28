OrderBy <- function(data, ..., inputs = AUTO, outputs = AUTO) {
  constructor <- OrderByMake(...)
  exprs <- get.exprs(constructor$inputs)
  atts <- as.character(exprs[is.symbols(exprs)])

  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(subtract(names(data$schema), atts))
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(as.logical(lapply(grokit$expressions[inputs], is.symbol))))
      outputs <- unlist(lapply(grokit$expressions[unique(inputs)], as.character))
    else
      Stop("Outputs is not allowed to be AUTO if expressions are present in inputs.")
  else
    outputs <- convert.atts(outputs)

  inputs <- c(constructor$inputs, inputs)
  outputs <- c(constructor$outputs, outputs)

  if (length(inputs) != length(outputs) - constructor$rank)
    Stop("The number of inputs and outputs must be the same.")

  Aggregate(data, constructor$GLA, inputs, outputs)
}

OrderByMake <- function(..., limit = 0, rank = NULL) {
  args <- as.list(substitute(list(...)))[-1]
  names <- names(args)
  ordering <- lapply(args, function(arg) {
    if (length(arg) != 2 || !is.symbol(arg[[1]]))
      Stop("Error in ", deparse(arg), "\n",
           "Each input must be a call of asc or dsc on an expression.")
    order <- arg[[1]]
    expr <- arg[[2]]
    check.exprs(expr)
    list(order, expr)
  })
  exprs <- lapply(ordering, `[[`, 2)
  directions <- as.character(lapply(ordering, `[[`, 1))

  if (!all(directions %in% c("asc", "dsc")))
    Stop("Directional specifiers must be either asc or dsc.",
         "The following are erroneous:\n",
         paste0("\t", subtract(directions, c("asc", "dsc")), collapse = "\n"))

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

  rank <- substitute(rank)
  if (!is.null(rank)) {
    check.atts(rank)
    rank <- convert.atts(rank)
    names <- c(rank, names)
  }

  if (!is.null(rank))
    class(rank) <- "attribute"

  GLA <- GLA(
      OrderBy,
      order = directions,
      limit = limit,
      rank = rank
      )

  list(GLA = GLA,
       inputs = atts,
       outputs = names,
       rank = as.integer(!is.null(rank)))
}
