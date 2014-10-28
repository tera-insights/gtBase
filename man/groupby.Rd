\name{GroupBy}
\alias{GroupBy}
\title{
  Group By
}
\description{
  Given a set of grouping attributes, the data is aggregated for each
  unique combination of grouping attributes.
}
\usage{
GroupBy(data, groupAtts, ...)
}
\arguments{
  \item{data}{
    an object of class \code{"\link{data}"}.
  }
  \item{group}{
    a set of \link{expressions} to be used for grouping.
  }
  \item{\dots}{
    a list of sub-ggregates. See \sQuote{details} for more information.
}
}
\details{
  Each sub-aggregate provided should resemble a typical call to an
  aggregate, except for the following:

  \enumerate{

    \item \code{data} should not provided as an argument to any of these inner
    calls. It has already been specified by the argument to \code{GroupBy}.

    \item \code{AUTO} is no-where supported, regardless of whether it is
    supported for that sub-aggregate outside of \code{GroupBy}
  }

  Each sub-aggregate should be provided a list of inputs and outputs of
  appropriate length. These inputs and outputs are then combined and
  used for the overall aggregate, with each output appearing in the
  final result.

  \code{group} can either be a list of expressions encapsulated by a
  call to \code{c()} or a single expression. In the former case, names
  can be provided to the expressions, such as \code{c(name1 = expr1)}.

  Each grouping expression is included in the result. If a name is
  included in the argument list within \code{c()}, then the
  corresponding column is given that name. Otherwise, if the grouping
  expression is merely an attribute of the \code{data}, the column is
  named after that attribute. If not, then a name is generated for the
  column, which is hidden from the user and guaranteed not to create
  naming conflicts with other columns.
}
\section{AUTO}{
  \code{AUTO} is supported neither for grouping attributes nor for the
  inner GLAs, regardless of whether or not it is supported for the
  associated functions outside of \code{GroupBy}.
}
\value{
  An object of class \code{"\link{data}"}. It will have a column for
  each grouping expression and output specified by a sub-aggregate. For
  each unique combination of group attributes witnessed by the data, the
  given sub-aggregates will be calculated using only tuples whose
  attributes match that unique combination.

  In the case that the sub-aggregates produce results with varying
  number of rows for some group, then the results with less rows will be
  repeated sufficiently to match the number of rows of the longest
  result, which is similarly done by various R processes.

  For example, if an \link{OrderBy} and a \code{Sum} are performed in
  which the \code{OrderBy} produces four rows for some group, then the
  value produced by \code{Sum} will be repeated four times for that
  group.
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
