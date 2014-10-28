\name{expression}
\alias{expressions}
\title{
Literal Expressions in Grokit
}
\description{
Expressions are used as inputs to GLAs, as well as for other tasks such
as generation and filtering.
}

\details{
  An expression specified in the following manner:

  \itemize{
    \item Only the following operators are allowed:
    \enumerate{
      \item Urnary: + - !
      \item Arithmetic: + - * / \% << >>
      \item Logical: == != && ||
      \item Regex: \%match\%
    }
    \item \%match\% is a logical valued operator used for regex
    matching. Evaluates to \code{TRUE} if and only if the first operand
    matches the second operand, a regular expression given as a string.
    \item The only function call allowed is \code{.()}. This is used for
    partial evaluation at compile time. In particular, this is useful
    for calling R functions as well as refering to name-value bindings in
    the R environment.
    \item Outside of calls to \code{.()}, only attribute names of the
    corresponding data and constants are allowed.
  }
}
\note{
  For the most part, the expression is not evaluated at all. Instead, it
  is parsed and its abstract syntax tree is visited and translated.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## Basic expression
(att1 + att2) * att3

## Use of .()
a <- 1
(att1 + .(a)) * att3
}
