#' Counting using a Bloom Filter
#'
#' \code{BloomFilter} approximates the number of distinct combinations of the
#' given inputs.
#'
#' This GLA functions similarly to \code{\link{CountDistinct}} but it uses a
#' Bloom filter to check for repeats rather than a full hash table. By doing so
#' it requires much less space at the cost of accuracy. The error can be
#' minimized by ensuring that exponent is set correctly.

#' The number of bits should be at least a tenth of the number of distinct
#' elements. If not, then it is likely that hash will completely fill up. If
#' this happens, the count of the data is returned as a worst case estimate.
#' Further increasing the number of bits serves to minimize the worst case eror.
#' If there are 10 bits per distinct element, then the estimate is guaranteed to
#' be within 1\% of the true value.

#' In addition to saving space, the Bloom Filter is potentially much faster than
#' CountDistinct for queries with a large number of distinct values. Merging
#' states is done via bitwise OR on the Bloom Filter data structure and
#' therefore the run time is practically independent of the size of the hashes,
#' as it dominated by the time spent processing each tuple. In theory, the
#' runtime is \eqn{O(n + k)}, where \eqn{n} is the number of rows in \code{data}
#' and \eqn{k} is the number of distinct elements.
#'
#' @param data A \code{\link{waypoint}} object.
#' @param inputs The expressions whose distinct combinations are counted.
#' @param outputs The column name of the result.
#' @param exponent The size of the filter is \eqn{2^{exponent}} bits.
#' @return A \code{\link{waypoint}} containing a single row and column whose
#'   name is given by \code{outputs}.
#' @seealso \code{\link{CountDistinct}} for a similar GLA.
#' @references
#' href{http://en.wikipedia.org/wiki/Bloom_filter}{Wikipedia} for more
#' information regarding the Bloom filter.
#' @examples
#' ## number of bits is too small, count returned
#' data <- Read(lineitem100g)
#' agg <- BloomFilter(data, exponent = 20, inputs = l_partkey)
#' result <- as.data.frame(agg)

#' ## number of bits is sufficient.
#' ## result is 19 999 077. True value is 20 000 000.
#' data <- Read(lineitem100g)
#' agg <- BloomFilter(data, exponent = 24, inputs = l_partkey)
#' result <- as.data.frame(agg)
#' @author Jon Claus, \email{jonterainsights@@gmail.com}, Tera Insights, LLC.
BloomFilter <- function(data, inputs, outputs, exponent = 16) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(names(data$schema))
  else
    inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    stop("outputs is not allowed to be AUTO.")
  else
    outputs <- convert.atts(outputs)
  if (length(outputs) != 1)
    stop("There must be exactly one output specified.")

  Aggregate(data, GLA(BloomFilter, exponent = exponent), inputs, outputs)
}
