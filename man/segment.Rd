\name{Segmenter}
\alias{Segmenter}
\title{
  Segmenter
}
\description{
  Given a set of grouping attributes, the data is aggregated for each
  unique combination of grouping attributes.
}
\usage{
GroupBy(data, segment = AUTO, passes = 1, num.segments = 64)
}
\arguments{
  \item{data}{
    an object of class \code{"GLA"}.
  }
  \item{segment}{
    an expression to produce the segments on.
  }
  \item{passes}{
    a list of sub-ggregates. See \sQuote{details} for more information.
  }
  \item{num.segments}{
    the number of segments to split the input GLA into. This should be
    at least the number of real CPUs times the number of passes,
    preferably twice that amount.
  }
}
\details{
  This GLA can only be placed on top of other GLAs, not arbitrary
  waypoints like most GLAs, and alters the way in which the input GLA is
  computed. Rather than performing the input GLA on all of the data at
  once, the data is split into \code{num.segments} separate pieces and
  the input GLA processes each segment separately, combining them
  afterwards. For GLAs who use \eqn{O(n)} space or worse, where \eqn{n}
  is the number of input tuples, this can serve to reduce the memory
  needed by a factor up to \eqn{num.segments * passes} at the cost of
  performance.
}
\section{AUTO}{
  If \code{segment = AUTO}, the first input expression of the input GLA
  is used. If the input GLA contains no inputs, such as \code{Count}, an
  error is thrown. This should be used with caution, as typically
  \code{segment} should be resolved as a single attribute.
}
\value{
  The result of the input GLA.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## Adapted from TPCH Query 1
library(gtBase)

data <- Read(lineitem10g)

agg <- GroupBy(data, group = c(l_returnflag, l_linestatus),
               sum_disc_price = Sum(l_extendedprice * (1 - l_discount)))

segmented <- Segmenter(agg)

View(segmented)
}
