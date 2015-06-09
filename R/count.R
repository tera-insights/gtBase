#' Number of Rows in a Waypoint.
#'
#' Counts the total number of tuples in the given waypoints.
#'
#' @param data A \code{\link{waypoint}} object.
#' @param outputs The column name of the result.
#' @return A \code{\link{waypoint}} containing a single row and column whose
#'   name is give by \code{output}.
#' @seealso \code{\link{BloomFilter}} for a similar GLA.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
Count <- function(data, outputs = count) {
  outputs <- substitute(outputs)
  check.atts(outputs)
  outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  gla <- GLA(Count)
  Aggregate(data, gla, character(), outputs)
}

#' Count Distinct Combinations
#'
#' Counts the number of distinct combinations for the given expressions.
#'
#' This GLA counts the number of distinct combinations of the given inputs
#' using a full hashing of the distinct combinations. As such, it requires
#' \eqn{O(k)} space, where \eqn{k} is the number of distinct combinations. The
#' run time is \eqn{O(n + k)}, where \eqn{n} is the number of rows in
#' \code{data}. The second term is a result of having to merge hashes between
#' different states. Having a large number of distinct values leads to
#' significant slowdown because of this; the \code{\link{BloomFilter}} is
#' recommended for these queries.
#'
#' @param data A \code{\link{waypoint}} object.
#' @param inputs The expressions whose distinct combinations are counted.
#' @param outputs The column name of the result.
#' @return A \code{\link{waypoint}} containing a single row and column whose
#'   name is given by \code{output}.
#' @seealso \code{\link{BloomFilter}} for a similar GLA.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
#' @seealso \code{\link{BloomFilter}} for a similarly functioning GLA.
#' @examples
#'
#' ## result is equal to total number of tuples, no repitiions
#' data <- Read(lineitem100g)
#' agg <- CountDistinct(data, inputs = c(l_tax, l_quantity, l_partkey))
#' result <- as.data.frame(agg)
#'
#' ## result is equal number of possible values for l_partkey as given
#' ## in the specifications of TPC-H
#' data <- Read(lineitem100g)
#' agg <- CountDistinct(data, inputs = l_partkey)
#' result <- as.data.frame(agg)
#'
CountDistinct <- function(data, inputs, outputs = count) {
  if (missing(inputs)) {
    inputs <- convert.schema(names(data$schema))
  } else {
    inputs <- substitute(inputs)
    check.exprs(inputs)
    inputs <- convert.exprs(inputs, data)
  }

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    outputs <- "count"
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  agg <- Aggregate(data, GLA(CountDistinct), inputs, outputs)
  agg
}
