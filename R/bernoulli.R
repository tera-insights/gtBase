#' Bernoulli Sampling
#'
#' The data is filtered randomly using a Bernoulli trial for each tuple.
#'
#' @param data A \code{\link{waypoint}}.
#' @param p The probability that a tuple passes the filter.
#' @param rng A length-one character vector specifying the random number
#'   generator used. It should correspond to an engine in the C++ header
#'   <random>.
#' @return A \code{\link{waypoint}} that has been filtered randomly.
#' @family sampling methods
#' @references
#' href{http://en.cppreference.com/w/cpp/numeric/random}{C++ reference} for a
#' list of random number generators.
#' href{http://en.wikipedia.org/wiki/Bernoulli_sampling}{Wikipedia} for
#' information about Bernoulli sampling.
#' @section author:
#' Jon Claus, \email{jonterainsights@@gmail.com}, Tera Insights, LLC.

Bernoulli <- function(data, p = 0.5, rng = "mt19937_64") {
  gf <- GF(BernoulliSample, p = p, rng = rng)
  Filter(data, gf)
}
