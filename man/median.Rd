\name{Median}
\alias{median.data}
\alias{Median}
\title{
Finding Median Values
}
\description{
Computes the median value of the given columns using a recursive binning
algorithm.
}
\usage{
median.data(x, ...) Median(x, ...)

Median(data, number.bins = 1000, sort.threshold = 1000, inputs = AUTO,
outputs = result)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, data}{
    an object of class \code{"\link{data}"}.
  }
  \item{number.bins}{
    the number of bins to use in the binning algorithm.
  }
  \item{sort.threshold}{
    the threshold for halting the binning algorithm.
  }
  \item{inputs}{
    the names of the columns that the GLA is performed on.
  }
  \item{outputs}{
    the name of the result. If not length 1, an error is thrown.
  }
}
\details{
  For the purposes of consistency with the \code{base} library
  implementation of \code{median}, an S3 method version of \code{median}
  was created, with the default simply referring to the built-in
  version; \code{median.data} merely passes the call to \code{Median}
  and should not be used within other GLAs such as
  \code{\link{GroupBy}}.

  The worst case complexity of the algorithm is \eqn{O(n \log_b
  \frac{n}{s} + s \log s)}{O(n * log(n / s) / log(b) + s log s)}, where
  \var{n} is the number of rows of \code{x}; \var{b},
  \code{number.bins}; \var{s}, \code{sort.threshold}. The average
  complexity is \eqn{O(n + s \log s)}{O(n + s log s)}. Additionally, the
  space complexity of the algorithm is \eqn{O(c \cdot b)}{O(c * b)},
  where \var{c} is the number of attributes used.
}
\section{AUTO}{
  In the case of \code{inputs = AUTO}, all attributes of the data are
  used.
}
\value{
  An object of class \code{"\link{data}"} with a single attribute.
}
\references{
Tibshirani, R. J. (2008)
  \href{http://www.stat.cmu.edu/~ryantibs/papers/median.pdf}{\var{Fast
  Computation of the Median by Successive Binning}}. Stanford University.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
}
