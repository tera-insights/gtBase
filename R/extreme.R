#' Extreme Tuples
#'
#' The data is filtered to only include tuples that contains the extremities of
#' the given expressions.
#'
#' The extremities to use for the filtering should be provided as a list of
#' arguments, each of which is in the form \code{fun(expr)} where \code{fun}
#' is either \code{max} or \code{min} and \code{expr} is an
#' \code{\link{expression}}.
#'
#' Precedence is based on the order in which the arguments are specified. The
#' result of this GLA is only the tuples whose attributes matched the given
#' extremities for the given attributes. For example, if the extremities
#' provided were \code{min{att1}, max{att2}} then the GLA would first filter
#' the data to include those tuples whose value of \code{att1} was minimized.
#' Of them, only those whose value of \code{att2} was the maximum on that
#' subset would be returned.
#'
#' Each extremity expression is included in the result. If a name is provided
#' in the argument list for a corresponding expression, the column is given that
#' corresponding name. Otherwise, if the expression is a single attribute then
#' the column is given that attribute name. If not, then the column for that
#' expression is given a constructed name that is hidden from the user and
#' guaranteed to not conflict with other column names.
#'
#' @param data A \code{\link{waypoint}}.
#' @param \dots Specification of extremities. See \sQuote{details} for more
#'   information.
#' @param inputs A named list of expressions, with the names being used as the
#'   corresponding outputs. These expressions are outputted in addition to those
#'   used to specify the extremities.
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
#' @seealso \code{\link{OrderBy}} for a similarly functioning GLA.
#' @examples
#'
#' ## One attribute test
#' data <- Read(lineitem100g)
#' agg <- ExtremeTuples(data, min(l_extendedprice))
#' result <- as.data.frame(agg)
#'
#' ## Three attribute test
#' ## Despite being secondary, l_extendedprice still achieves its global
#' ## minimum on the tuples where l_partkey was maximized. However, l_tax
#' ## does not, as the value in the result is 0.03 and in the overall data
#' ## the maximum is 0.08.
#' data <- Read(lineitem100g)
#' agg <- ExtremeTuples(data, max(l_partkey), min(l_extendedprice), max(l_tax))
#' result <- as.data.frame(agg)
ExtremeTuples <- function(data, ..., inputs, outputs) {
  constructor <- ExtremeTuplesMake(...)
  exprs <- grokit$expressions[constructor$inputs]
  atts <- as.character(exprs[as.logical(lapply(exprs, is.symbol))])

  if (missing(inputs)) {
    inputs <- convert.schema(subtract(names(data$schema), atts))
  } else {
    if (inherits(tryCatch(is.inputs(inputs)), "error")) {
      inputs <- substitute(inputs)
      check.exprs(inputs)
      inputs <- convert.exprs(inputs)
    } else {
      inputs <- convert.inputs(inputs)
    }
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

  inputs <- c(constructor$inputs, inputs)
  outputs <- c(constructor$outputs, outputs)

  if (length(inputs) != length(outputs))
    stop("The number of inputs and outputs must be the same.")

  Aggregate(data, constructor$GLA, inputs, outputs)
}

ExtremeTuplesMake <- function(...) {
  args <- alist(...)
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


