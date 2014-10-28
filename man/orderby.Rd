\name{OrderBy}
\alias{OrderBy}
\title{
  Order By
}
\description{
  The data is sorted according to a given ordering.
}
\usage{
OrderBy(data, ..., inputs = AUTO, outputs = AUTO)

OrderByMake(..., limit = 0, rank = NULL)
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
    possible re-namings of the .
  }
  \item{limit}{
    the number of rows to include in the result. A value of \code{0}
    specifies that all rows are kept. Setting \code{limit = k}
    effectively functions as a top-k selection.
  }
  \item{rank}{
    if non-\code{NULL}, an additional column is included that functions
    as the row number. This is typically not needed as the row number is
    otherwise provided. The name of this column is the value provided as
    \code{rank}.
  }
  \item{\dots}{
    For \code{OrderBy}: additional arguments passed to OrderByMake.

    For \code{OrderByMake}: a list of natural orderings to use, with
    precedence going to those seen earlier in the list. In the case that
    names are provided, these names are used as column names in the
    result. See \sQuote{details} for more information.
  }
}
\details{
  The ordering schema is provided as a list of arguments, each of which
  is in the form \code{fun(expr)} where \code{fun} is either \code{asc}
  or \code{dsc} and \code{expr} is an \code{\link[=expressions]{expression}}.

  Precedence is based on the order in which the arguments are
  specified. For example, in the case of \code{asc(att1), dsc(att2)},
  the ordering is primarily ascending with respect to \code{att1} with
  ties being broken by \code{att2} in a descending manner. Ties are
  broken arbitrarily, including the case in which only some tuples that
  are tied are included in the result based on \code{limit}.

  Each ordering expression is included in the result. If a name is
  provided in the argument list for a given expression, the column is
  given that corresponding name. Otherwise, if the expression is a
  single attribute then the column is given that attribute name. If not,
  then the column for that expression is given a constructed name that
  is hidden from the user and guaranteed to not conflict with other
  column names.
}
\section{AUTO}{
  In the case that \code{inputs = AUTO}, each attribute of the
  \code{data} that was not used expressly as an ordering attribute is
  included in the result. For example, if \code{data} contains
  attributes \code{att1, att2, att3} and the ordering is
  \code{asc(att1), dsc(att2 + att3)}, then the result will contain 4
  columns with names \code{att1, gen, att2, att3}, where \code{gen} is a
  placeholder for a generated name and whose values are \code{att2 +
    att3}.
}
\value{
  An object of class \code{"\link{data}"}, with the attribute names as
  discussed above. Upon conversion to a data frame, there will be
  \eqn{\min(l, n)}{max(l, n)}, where \eqn{l} is \code{limit} and \eqn{n}
  is the number of rows in \code{data}.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## TPCH Query 1
data <- Read(lineitem10g)
filter <- data[l_shipdate <= .(as.Date("1998-12-01")) - 90]
agg <- GroupBy(
  filter,
  groupAtts = c(rf = l_returnflag, ls = l_linestatus),
  sum_disc_price = Sum(l_extendedprice * (1 - l_discount)),
  sum_charge = Sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)),
  avg_qty = Average(l_quantity),
  count_order = Count(1),
  sum_qty = Sum(l_quantity),
  avg_price = Average(l_extendedprice),
  sum_base_price = Sum(l_extendedprice),
  avg_disc = Average(l_discount)
)
agg <- OrderBy(
  agg,
  asc(rf),
  dsc(ls),
  rank = rank
)
result <- as.data.frame(agg)
}