\name{result}
\alias{as.data.frame.data}
\alias{View.data}
\alias{as.object}
\title{
Extracting Results
}
\description{
Because obects of class \code{"\link{data}"} are abstractions, they
contain no actual data. Instead, the results of queries must be
extracted using one of the function below.
}
\usage{
as.data.frame.data(data, ...)

View(data, ...)

as.object(data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{\dots}{
    additional arguments to describe the format of the result.
  }
}
\details{
  Arguments passed via the ellipsis should be in the format of
  \code{name = expression} pairs, where \code{name} is the
  desired name for the column in the result and \code{expression} is an
  \code{\link{expression}} used to construct the result.

  \code{as.data.frame} is used to produce a data frame for use in R
  after the query has been executed.

  \code{View} interacts with the online GrokIt system to visualize the
  result. In addition, it will call \code{\link{quit}} to end the
  query.

  \code{as.object} represents the object in a list format corresponding
  to JSON notation. It is similar in structure to \code{\link{fromJSON}}
  in the \code{rjson} library.
}
\section{AUTO}{
  If no arguments are specified within the ellipsis, then each attribute
  of the data is put into a column with the same name.
}
\value{
  \code{as.data.frame} produces a data frame; \code{as.object}, a list;
  \code{View} will return \code{NULL} and quit R because the
  encapsulated information appears on the visualizations page.
}
\author{
  Jon Claus at Tera Insights LLC
}
\examples{
}
