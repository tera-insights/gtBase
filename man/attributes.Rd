\name{attributes}
\alias{attributes}
\alias{inputs}
\alias{outputs}
\title{
Model Attributes
}
\description{
Attributes are specified using literal symbols.
}
\usage{
inputs = c(...)
outputs = expr
inputs = AUTO
}
\arguments{
  \item{...}{
    A list expressions to be used as inputs for the current action. See
    \code{\link{expressions}} for more information.
  }
  \item{expr}{
    An expression in the same manner as above.
  }
  \item{AUTO}{
    A special keyword in the R-grokit system. It is interpreted
    differently in individual contexts; in general, it acts much like
    the SQL \code{*} construct for inputs.
  }
}
\details{
  Model formulae for GLAs are specified in an atypical manner in the
  Grokit system. Rather than using the built-in formula operator,
  \code{~}, formulae are specified using the combine function \code{c}
  whenever expressions are processed independently and concurrently by the
  GLA. For example, in the GLA \code{\link{Sum}}, operating on two
  columns at once produces the same result as if you had executed a
  separate GLA for each column.

  Additionally,  the call to \code{c} can be omitted for a single
  expression, e.g. \code{inputs = c(att)} and \code{inputs = att} are
  equivalent.

  In general, \code{inputs} is the argument in the GLA that specifies
  which columns of the data that the GLA will operate on and
  \code{outputs} is list of names for the columns in the result.

  \code{AUTO} is a reserved word in the Grokit system that is used to
  infer inputs or outputs. Although the exact interpretation depends on
  the GLA, \code{inputs = AUTO} typically selects all attributes in the
  relation and \code{outputs = AUTO} names the result in the same manner
  as \code{read.csv}, i.e. \code{"V0"}, \code{"V1"}, ...

  For most actions that take inputs and outputs, there will be a section
  in the help describing precisely how \code{AUTO} behaves.

  It should be note that these constructs are only valid in the
  specification of arguments to an appropiate GLA. See \sQuote{Examples}
  for more information.
}
\note{
  The expressions for \code{inputs} and \code{outputs} are never
  evaluated. Instead, they are parsed and the resulting abstract syntax
  tree is deconstructed.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}

\seealso{
\code{\link{expression}} for information on how to specify expressions.
}
\examples{
## Proper specification of inputs and outputs
data <- Read(lineitem100g)
result <- Sum(data, inputs = c(l_discount, l_tax), outputs =
c(sum_discout, sum), force.frame = TRUE)
print(result)

## Usage of AUTO for both inputs and outputs
data <- Read(lineitem100g)
result <- Sum(data, force.frame = TRUE)
result

## Improper assignment
data <- Read(lineitem100g)
inputs <- c(l_discount, l_tax) ## error thrown here
result <- Sum(data, inputs = inputs, outputs = c(sum_discout, sum),
force.frame = TRUE)
}
