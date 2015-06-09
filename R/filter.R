#' Basic Filtering of Waypoints
#'
#' Filter a waypoint based on a boolean expression.
#'
#' \code{condition} is evaluated for each tuple in \code{data} independently.
#' Only those tuples for which \code{condition} evaluates to TRUE are passed
#' through the filter.
#'
#' @param data A \code{\link{waypoint}}.
#' @param condition A boolean valued \code{\link{expression}}.
#' @return A \code{\link{waypoint}} with a subset of the tuples in \code{data}.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
`[.data` <- function(data, condition) {
  condition <- substitute(condition)
  if (condition[[1]] == "c")
    stop("Condition is not allowed to be a listt of expressions.")
  check.exprs(condition)
  condition <- convert.exprs(condition, data)
  check.inputs(data, condition)
  alias <- create.alias("filter")
  schema <- data$schema
  filter <- list(data = data, alias = alias, schema = schema, condition = condition)
  class(filter) <- c("Filter", "data")
  filter
}
