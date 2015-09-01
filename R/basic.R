#' Univariate Statistics
#'
#' These GLAs compute various univariate statistics separately for each input.
#'
#' The result of each GLA is a waypoint with one column per input and a single
#' row whose value is the specified univariate statistic for the corresponding
#' expression.
#'
#' With the exception of finding the median, all of these aggregates are fairly
#' straightforward, require \eqn{O(k)} space, and run in \eqn{O(n \cdot k)}
#' time, where \eqn{k} is the number of inputs and \eqn{n} is the number of
#' tuples.
#'
#' The median algorithm relies on a iterative binning algorithm, based on the
#' Tibshirani paper. This algorithm requires two parameters: the number of bins
#' to use (\eqn{b}) and the threshold at which to sort (\eqn{t}). During the
#' first iteration, the range of the input is found. This interval is then split
#' into \eqn{b} equal parts. Each input is then sorted into bins and the bin
#' that must contain the median is then sub-divided into \eqn{b} equal parts.
#' This recursive sub-division continues until less than \eqn{t} elements are in
#' a bin that contains the median. These elements are then sorted and the median
#' is outputted. As such, this algorithm requires \eqn{O(k \cdot b)} spaces and
#' runs in \eqn{O(k \cdot (n \cdot \log_b n + t \log t))} time.
#'
#' @name univariate
#' @param data A \code{\link{waypoint}}.
#' @param input A named list of expressions, with the names being used as the
#'   corresponding outputs. These expressions are outputted in addition to those
#'   used to specify the extremities.
#'
#'   If no name is given and the corresponding  expression is simply an
#'   attribute, then said attribute is used as the name. Otherwise an error is
#'   thrown, as there is no reason to include an extra input if corresponding
#'   output column cannot be referenced later.
#' @param outputs The usual way to specify the outputs. If both this and names
#'   for the \code{inputs} are given, a warning is given and \code{outputs} is
#'   used.
#' @return A \code{\link{waypoint}} with a single row. See \sQuote{details} for
#'   more information.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
NULL

#' @rdname univariate
Sum <- function(data, inputs, outputs) {
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
  Aggregate(data, GLA(Sum), inputs, outputs)
}

#' @rdname univariate
Mean <- function(data, inputs = AUTO, outputs = AUTO) {
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
  Aggregate(data, GLA(Average), inputs, outputs)
}

#' @rdname univariate
Min <- function(data, inputs = AUTO, outputs = AUTO) {
  if (missing(inputs)) {
    inputs <- convert.schema(setdiff(names(data$schema), atts))
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
  Aggregate(data, GLA(Min), inputs, outputs)
}

#' @rdname univariate
Max <- function(data, inputs = AUTO, outputs = AUTO) {
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
  Aggregate(data, GLA(Max), inputs, outputs)
}

#' @rdname univariate
#' @param number.bins The number of bins to use in the binning algorithm.
#' @param sort.threshold The maximum number of items on which to manually sort.
#' @references
#' href{http://www.stat.cmu.edu/~ryantibs/papers/median.pdf}{Tibshirani} for
#' details regarding the binning algorithm.
Median <- function(data, inputs = AUTO, outputs = result,
                   number.bins = 1000, sort.threshold = 1000) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  gla <- GLA(statistics::Median_Binning,
             number.bins = number.bins,
             sort.threshold = sort.threshold)
  Aggregate(data, gla, inputs, outputs)
}
