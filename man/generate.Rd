\name{Generate}
\alias{Generate}
\title{
  Attribute Creation
}
\description{
Generate appends columns to each row of the data by evaluating the given expression
}
\usage{
Generate(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    an object of class \code{"\link{data}"}
  }
  \item{\dots}{
    a list of \code{name = expression} pairs. It is required that every
    argument to be named, as that name is used as the column name for
    the generated data.
  }
}
\details{
  It is important to note that generation adds overhead to the
  evaluation process. Creating an additional attribute solely for use in
  a single GLA or filtering is discouraged because of
  this. \code{Generate} is more appropiately used in conjunction with
  either joining, writing to the disc, or several actions using the same
  new attribute. See \sQuote{examples} below for more information.
}
\value{
  An object of class \code{"\link{data}"} with the additonal columns.
}
\author{
  Jon Claus at Tera Insights LLC
}
\examples{
## inefficient usage
data <- Read(lineitem100g)
data <- Generate(data, cost = l_extended_price * (1 + l_tax) * (1 -
l_discount))
data <- data[cost < 100]

## this is also inefficient
data <- Read(lineitem100g)
data <- Generate(data, cost = l_extended_price * (1 + l_tax) * (1 -
l_discount), stock = l_quantity * l_extended_price)
data <- data[cost < 100]
data <- data[stock > 10]

## advised usage
supplier <- Read(supplier100g)
lineitem <- Read(lineitem100g)
lineitem <- Generate(lineitem, stock = l_quantity * l_extended_price)
data <- Join(supplier, s_acctbal, lineitem, stock)

}