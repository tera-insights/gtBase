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

Distinct <- function(data, inputs = AUTO, outputs = AUTO) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  inputs <- convert.exprs(inputs, data)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    if (all(is.symbols(grokit$expressions[inputs])))
      outputs <- unlist(lapply(grokit$expressions[inputs], as.character))
    else
      stop("outputs can only be AUTO when inputs are all attributes.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != length(inputs))
    stop("There must be exactly one output specified per input.")

  Aggregate(data, GLA(Distinct), inputs, outputs)
}
