\name{ExtremeTuples}
\alias{ExtremeTuples}
\title{
  Extreme Tuples
}
\description{
  The data is filtered to only include tuples that contains the
  extremities of given attributes.
}
\usage{
ExtremeTuples(data, ..., inputs = AUTO, outputs = AUTO)

ExtremeTuplesMake(...)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{inputs}{
    which attributes of the \code{data} to include in the result in addition to
    those used in the ordering.
  }
  \item{outputs}{
    possible re-namings of the \code{inputs}.
  }
  \item{\dots}{
    For \code{ExtremeTuples}: additional arguments passed to ExtremeTuplesMake.

    For \code{ExtremeTuplesMake}: a list of extremities to use for the
    filtering, with precedence going to those seen earlier in the
    list. In the case that names are provided, these names are used as
    column names in the result. See \sQuote{details} for more information.
  }
}
\details{
  The extremities to use for the filtering is provided as a list of
  arguments, each of which is in the form \code{fun(expr)} where
  \code{fun} is either \code{max} or \code{min} and \code{expr} is an
  \code{\link[=expressions]{expression}}.

  Precedence is based on the order in which the arguments are
  specified. The result of this GLA is only the tuples whose attributes
  matched the given extremities for the given attributes. For example,
  if the extremities provided were \code{min{att1}, max{att2}} then
  the GLA would first filter the data to include those tuples whose
  value of \code{att1} was minimized. Of them, only those whose value of
  \code{att2} was the maximum on that subset would be returned.

  Each extremity expression is included in the result. If a name is
  provided in the argument list for a given expression, the column is
  given that corresponding name. Otherwise, if the expression is a
  single attribute then the column is given that attribute name. If not,
  then the column for that expression is given a constructed name that
  is hidden from the user and guaranteed to not conflict with other
  column names.
}
\section{AUTO}{
  In the case that \code{inputs = AUTO}, each attribute of the
  \code{data} that was not used expressly as an extremity is included in
  the result. For example, if \code{data} contains attributes
  \code{att1, att2, att3} and the ordering is \code{min(att1), max(att2
  + att3)}, then the result will contain 4 columns with names
  \code{att1, gen, att2, att3}, where \code{gen} is a placeholder for a
  generated name and whose values are \code{att2 + att3}.

  If \code{outputs = AUTO}, then the names of the \code{inputs} in the result
  are left unchanged from \code{data}. If some of the \code{inputs} were
  not attributes of \code{data}, an error is thrown.
}
\value{
  An object of class \code{"\link{data}"}, with the attribute names and
  rows as discussed above.
}
\seealso{
  \code{\link{OrderBy}} for a similarly functioning GLA.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## One attribute test
## Returns one tuple with l_extendedprice = 900.05.
data <- Read(lineitem100g)
agg <- ExtremeTuples(data, min(l_extendedprice))
result <- as.data.frame(agg)

## Three attribute test
## Despite being secondary, l_extendedprice still achieves its global
## minimum on the tuples where l_partkey was maximized. However, l_tax
## does not, as the value in the result is 0.03 and in the overall data
## the maximum is 0.08.
data <- Read(lineitem100g)
agg <- ExtremeTuples(data, max(l_partkey), min(l_extendedprice), max(l_tax))
result <- as.data.frame(agg)
}