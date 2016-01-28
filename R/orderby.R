#' Order By
#'
#' Orders the data based on simple comparison between rows.
#'
#' Given a list of expressions and directions, the data is sorted by evaluating
#' the expressions for each row and placing the rows according to the given
#' directions. The expressions are evaluated in order, with earlier expressions
#' given more precdence. If there is a tie for all expressions for two distinct
#' rows, then the order in which they appear in the result is random.
#'
#' The direction for each expression is given as either 'asc' or 'dsc'. The
#' combination of the direction and a corresponding expression is specified as a
#' call, with the direction being the function and the only argument being the
#' expression.
#'
#' For example, if the comparison operators are specified as
#' \code{dsc(a), asc(b)}, then rows with larger values for \code{a} appear
#' before those with smaller values. In the case of a tie, rows with smaller
#' values for \code{b} appear before those with larger values.
#'
#' @param data A \code{\link{waypoint}}.
#' @param \dots Specification of ordering. See \sQuote{details} for more
#'   information.
#' @param limit The maximum number of tuples to include. If this is given as a
#'   positive integer, then only the first \code{limit} rows are kept.
#'
#'   Because ties are randomly broken, some rows may be discarded while rows
#'   that tie with them are kept.
#' @param rank Either NULL or an attribute name. In the latter case, a column is
#'   included in the result whose value is the numeric rank of that row, e.g. 1,
#'   2, ...
#' @param inputs A named list of expressions, with the names being used as the
#'   corresponding outputs. These expressions are outputted in addition to those
#'   used to specify the sorting.
#'
#'   If no name is given and the corresponding  expression is simply an
#'   attribute, then said attribute is used as the name. Otherwise an error is
#'   thrown, as there is no reason to include an extra input if corresponding
#'   output column cannot be referenced later.
#'
#'   If this is not given, then each attribute of \code{data} that is not used
#'   exactly as an expression for comparison is included.
#' @param outputs The usual way to specify the outputs. If both this and names
#'   for the \code{inputs} are given, a warning is given and \code{outputs} is
#'   used.
#' @return A \code{\link{waypoint}} with the designated columns and rows.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
#' @seealso \code{\link{ExtremeTuples}} for a similarly functioning GLA.

OrderBy <- function(data, ..., inputs, outputs) {
  constructor <- OrderByMake(..., data = data)
  exprs <- get.exprs(constructor$inputs)
  atts <- as.character(exprs[is.symbols(exprs)])

  if (missing(inputs)) {
    inputs <- convert.schema(subtract(names(data$schema), atts))
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
    if (!is.null(attr(inputs, "names")))
      warning("both outputs and named inputs given. outputs used.")
    outputs <- convert.atts(substitute(outputs))
  }

  inputs <- c(constructor$inputs, inputs)
  outputs <- c(constructor$outputs, outputs)

  if (length(inputs) != length(outputs) - constructor$rank)
    stop("The number of inputs and outputs must be the same.")

  Aggregate(data, constructor$GLA, inputs, outputs)
}

#' @rdname OrderBy
OrderByMake <- function(..., limit = 0, rank = NULL, data) {
  args <- as.list(substitute(list(...)))[-1]
  names <- names(args)
  ordering <- lapply(args, function(arg) {
    if (length(arg) != 2 || !is.symbol(arg[[1]]))
      stop("Error in ", deparse(arg), "\n",
           "Each input must be a call of asc or dsc on an expression.")
    order <- arg[[1]]
    expr <- arg[[2]]
    check.exprs(expr)
    list(order, expr)
  })
  exprs <- lapply(ordering, `[[`, 2)
  directions <- as.character(lapply(ordering, `[[`, 1))

  if (!all(directions %in% c("asc", "dsc")))
    stop("Directional specifiers must be either asc or dsc.",
         "The following are erroneous:\n",
         paste0("\t", subtract(directions, c("asc", "dsc")), collapse = "\n"))

  atts <- unlist(lapply(exprs, convert.exprs, data = data))
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
