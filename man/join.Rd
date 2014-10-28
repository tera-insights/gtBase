\name{Join}
\alias{Join}
\title{
Equi-join
}
\description{
Joins two tables by columns on the basis of equality of attributes.
}
\usage{
Join(x, xAtts, y, yAtts)
}
\arguments{
  \item{x}{
    an object of class \code{"\link{data}"} to be used on the left hand
    side of the join. It is more efficient for this to be the larger of the
    two tables.
}
  \item{xAtts}{
    the attributes of \code{x} to be used in the equality
    condition. Specified in the format of \code{\link{inputs}}.
}
  \item{y}{
    an object of class \code{"\link{data}"} to be used on the right hand
    side of the join.
}
  \item{yAtts}{
    the attributes of \code{y} to be used in the equality
    condition. Specified in the format of \code{\link{inputs}}.
  }
}
\details{
  The specified attributes are matched in pair. For example, given
  \code{xAtts = c(att1, att2)} and \code{yAtts = c(att3, att4)}, the
  condition of the join is \code{att1 == att3 && att2 == att4}. This
  format necessiatates that \code{xAtts} and \code{yAtts} have the
  same length.
}
\section{AUTO}{
  Neither \code{xAtts} nor \code{yAtts} is allowed to be \code{AUTO} due
  to the context - this would lead to duplication of entire rows.
}
\value{
  The result of the join, i.e. the subset of the cross product that
  satisfies the given equality conditions.
}
\author{
  Jon Claus at Tera Insights LLC
}
\seealso{
\code{\link{Filter}}
}
\examples{
nation <- Read(nation100g)
region <- Read(region100g)
j1 <- Join(nation, n_regionkey, region, r_regionkey)
}
