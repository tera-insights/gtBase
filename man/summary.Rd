\name{Summary}
\alias{Summary}
\title{Computing Summary Statistics}
\description{
  Computes the mean, standard deviation, variance, min, and max of the
  values for each of the specified columns, as well as the count.
}
\usage{
Summary(x, inputs = AUTO, outputs = result, force.frame = FALSE)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}
  }
  \item{inputs}{
    the names of the columns that the GLA is performed
    on.
  }
  \item{outputs}{
    the name of the result. Only relevant when using \code{\link{View}}.
  }
  \item{force.frame}{
    a logical value indicating whether or not to produce a data frame. See
    \sQuote{Details} for more information.
  }
}
\details{
  Generally, this GLA will run in the same time as the more basic GLAs,
  such \code{\link{Sum}} or \code{\link{Average}} because the runtime is
  limited by IO rather than actual computation.
}
\section{AUTO}{
  In the case of \code{AUTO} for \code{inputs}, all attributes of the
  data  are used.
}
\value{
  Returns either \code{NULL} or an object of class
  \code{"\link{data.frame}"}.
}
\author{
  Jon Claus at Tera Insights LLC
}

\examples{

}
