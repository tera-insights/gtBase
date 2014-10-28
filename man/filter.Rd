\name{[.data}
\alias{[.data}
\title{
Filtering data
}
\description{
The subset operator on an object of class \code{"\link{data}"} filters
the data given a condition on the attributes of each row.
}
\usage{
data[condition]
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}
  }
  \item{condition}{
    an \code{\link{expression}} whose value can be coerced to a boolean.
  }
}
\details{
  The conditional expression is evaluated for each row independently.
}
\value{
  An object of class \code{"\link{data}"} that is the subset of \code{data}
  consisting of the rows of data where the condition evaluated to
  \code{TRUE}.
}
\author{
  Jon Claus at Tera Insights LLC
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
part <- Read(part100g)
selpart <- part[p_type \%match\% ".*BRASS" && p_size == 15]

region <- Read(region100g)
selregion <- region[r_name == "EUROPE"]
}
