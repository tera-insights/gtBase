\name{BloomFilter}
\alias{BloomFilter}
\title{
  Counting with a Bloom Filter
}
\description{
  Approximates the number of distinct combinations for the given
  attributes.
}
\usage{
BloomFilter(data, exponent = 16, inputs = AUTO, outputs = count)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{exponent}{
    the size of the hash is \eqn{2^n} bits.
  }
  \item{inputs}{
    which attributes of \code{data} to perform the GLA on.
  }
  \item{outputs}{
    the desired column names of the result.
  }
}
\details{
  This GLA functions similarly to \code{\link{CountDistinct}} but it
  uses a Bloom Filter to check for repeats rather than a full hash. By
  doing so, it requires much less space at the cost of accuracy. The
  error can be minimized by ensuring that exponent is set correctly.

  The number of bits should be at least a tenth of the number of
  distinct elements. If not, then it is likely that hash will completely
  fill up. If this happens, the count of the data is returned as a worst
  case   estimate. Further increasing the number of bits serves to
  minimize the worst case eror. If there are 10 bits per distinct
  element, then the estimate is guaranteed to be within 1\% of the true.

  In addition to saving space, the Bloom Filter is potentially much
  faster than CountDistinct for queries with a large number of distinct
  values. Merging states is done via bitwise OR on the Bloom Filter data
  structure and therefore the run time is practically independent of the
  size of the hashes, as it dominated by the time spent processing each
  tuple. In theory, the runtime is \eqn{O(n + k)}, where \eqn{n} is the
  number of rows in \code{data} and \eqn{k} is the number of distinct
  elements.
}
\section{AUTO}{
  In the case of \code{inputs = AUTO}, all attributes of the data are
  used.

  \code{outputs} is not allowed to be \code{AUTO}.
}
\value{
  An object of class \code{"\link{data}"} with a single attribute. Upon
  conversion to a data frame, it will contain a single row.
}
\seealso{
  \code{\link{CountDistinct}} for a similarly functioning GLA.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## number of bits is too small, count returned
data <- Read(lineitem100g)
agg <- BloomFilter(data, exponent = 20, inputs = l_partkey)
result <- as.data.frame(agg)

## number of bits is sufficient.
## result is 19, 999, 077. True value is 20, 000, 000.
data <- Read(lineitem100g)
agg <- BloomFilter(data, exponent = 24, inputs = l_partkey)
result <- as.data.frame(agg)
}
