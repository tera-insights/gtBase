\name{basic}
\alias{Sum}
\alias{Average}
\alias{Count}
\title{Basic GLAs}
\description{
  Computes the given basic statistics for each of the specified columns.
}
\usage{
Sum(data, inputs = AUTO, outputs = AUTO)

Average(data, inputs = AUTO, outputs = AUTO)

Count(data, inputs = AUTO, outputs = AUTO)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{inputs}{
    which attributes of \code{data} to perform the GLA on.
  }
  \item{outputs}{
    the desired column names of the result.
  }
}
\details{
  If \code{outputs} is not set to \code{AUTO}, then the attributes names
  of the result will be those specified by outputs.
}
\section{AUTO}{
  In the case of \code{AUTO} for \code{inputs}, all attributes of the
  data  are used. If \code{outputs} is \code{AUTO}, the names of the
  result are \code{V0}, \code{V1}, and so forth.
}
\value{
  An object of class \code{"\link{data}"} with attributes names as
  discussed above. Upon conversion to a data frame, there will be a
  single row.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\seealso{
  \code{\link{Summary}}
}
\examples{
data <- Read(lineitem100g)
agg <- Sum(data, inputs = c(l_discount, l_tax), outputs =
  c(sum_discout, sum))
result <- as.data.frame(agg)
}
