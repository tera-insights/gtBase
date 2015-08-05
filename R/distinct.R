#' Remove Duplication Combinations
#'
#' Filters out repeated combination of inputs.
#'
#' This GLA returns the distinct combinations of the given inputs using a full
#' hashing of the distinct combinations. As such, it requires \eqn{O(k)} space,
#' where \eqn{k} is the number of distinct combinations. The run time is
#' \eqn{O(n + k)}, where \eqn{n} is the number of rows in \code{data}. The
#' second term is a result of having to merge hashes between different states.
#' Having a large number of distinct values leads to significant slowdown
#' because of this.
#'
#' @param inputs A named list of expressions, with the names being used as the
#'   corresponding outputs. These expressions are outputted in addition to those
#'   used to specify the extremities.
#'
#'   If no name is given and the corresponding  expression is simply an
#'   attribute, then said attribute is used as the name. Otherwise an error is
#'   thrown, as there is no reason to include an extra input if corresponding
#'   output column cannot be referenced later.
#' @param outputs The usual way to specify the outputs. If both this and names
#'   for the inputs are given, an error is thrown.
#' @return A \code{\link{waypoint}}.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
Distinct <- function(data, inputs, outputs) {
  if (missing(inputs)) {
    inputs <- convert.schema(names(data$schema))
  } else {
    is.input <- tryCatch(is.inputs(inputs), error = identity)
    if (inherits(is.input, "error"))
      is.input <- FALSE
    if (!is.input) {
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

  assert(length(outputs) == length(inputs),
         "There must be exactly one output specified per input.")

  Aggregate(data, GLA(Distinct), inputs, outputs)
}
