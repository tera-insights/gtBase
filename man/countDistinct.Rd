\name{CountDistinct}
\alias{CountDistinct}
\title{
  Count Distinct Elements
}
\description{
  Counts the number of distinct combinations for the given attributes.
}
\usage{
CountDistinct(data, inputs = AUTO, outputs = count)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{inputs}{
    which attributes of \code{data} to perform the GLA on.
  }
  \item{outputs}{
    the desired column name of the result.
  }
}
\details{
  This GLA counts the number of distinct combinations of the given
  inputs using a full hashing of the distinct combinations. As such, it
  requires \eqn{O(k)} space, where \eqn{k} is the number of distinct
  combinations. The run time is \eqn{O(n + k)}, where \eqn{n} is the
  number of rows in \code{data}. The second term is a result of having
  to merge hashes between different states. Having a large number of
  distinct values leads to significant slowdown because of this; the
  \code{\link{BloomFilter}} is recommended for these queries.
}
\section{AUTO}{
  In the case of \code{inputs = AUTO}, all attributes of the data are
  used.
}
\value{
  An object of class \code{"\link{data}"} exactly one row element. Upon
  conversion to a data frame, it will contain a single row.
}
\seealso{
  \code{\link{BloomFilter}} for a similarly functioning GLA.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## result is equal to total number of tuples, no repitiions
data <- Read(lineitem100g)
agg <- CountDistinct(data, inputs = c(l_tax, l_quantity, l_partkey))
result <- as.data.frame(agg)

## result is equal number of possible values for l_partkey as given
## in the specifications of TPC-H
data <- Read(lineitem100g)
agg <- CountDistinct(data, inputs = l_partkey)
result <- as.data.frame(agg)
}