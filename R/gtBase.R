#' Abstract Data Objects
#'
#' Large scale data is represented abstractly in the GrokIt system. Rather than
#' manipulating the data directly, the R interface sets up a description of the
#' query using a waypoint system.
#'
#' Due to the scale of the data that the GrokIt system is intended to use, it
#' is impossible to fully import the data into R because of memory limitations.
#' Instead, the data is represented abstractly as an S3 object of class
#' \code{"data"} built upon a list, with only the most relevant information
#' kept.
#'
#' The only elements that every \code{"data"} object is guaranteed to contain
#' are labeled \code{"schema"} and \code{"origin"}. The latter is used only for
#' book-keeping and optimization and is not of much interest. The former is of
#' much more import; it is used to ensure that the user only performs
#' operations on attributes the data actually contains. As such, to find which
#' attributes an object \code{x} contains, simply call \code{x$schema} to
#' produce a character vector giving the names of the attributes.
#'
#' @note Although the above are the only two elements that every \code{"data"}
#' object is guaranteed to have, there will always be additional elements.
#' These are used to structure the overall query and relate it to the system;
#' as such, the list structure grows rapidly and should be ignored by the
#' average user.
#' @name waypoint
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC
#' @examples
#'
#' ## Proper specification of inputs and outputs
#' data <- Read(lineitem100g)
#' data$schema ## produces a character vector
#' str(data) ## fully details the structure for illustration's sake
#'
NULL

#' Expressions
#'
#' Expressions are used as inputs for various waypoints.
#'
#' @aliases expressions
#' @note For the most part, the expression is not evaluated at all. Instead, it
#' is parsed and its abstract syntax tree is visited and translated.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC
#' @name expressions
#' @aliases expresssion
#' @examples
#'
#' ## Basic expression
#' (att1 + att2) * att3
#'
#' ## Use of .()
#' a <- 1
#' (att1 + .(a)) * att3
#'
NULL

#' Model Attributes
#'
#' Attributes are specified using literal symbols.
#'
#' Model formulae for GLAs are specified in an atypical manner in the Grokit
#' system. Rather than using the built-in formula operator, \code{~}, formulae
#' are specified using the combine function \code{c} whenever expressions are
#' processed independently and concurrently by the GLA. For example, in the GLA
#' \code{\link{Sum}}, operating on two columns at once produces the same result
#' as if you had executed a separate GLA for each column.
#'
#' Additionally, the call to \code{c} can be omitted for a single expression,
#' e.g. \code{inputs = c(att)} and \code{inputs = att} are equivalent.
#'
#' In general, \code{inputs} is the argument in the GLA that specifies which
#' columns of the data that the GLA will operate on and \code{outputs} is list
#' of names for the columns in the result.
#'
#' It should be note that these constructs are only valid in the specification
#' of arguments to an appropiate GLA. See \sQuote{Examples} for more
#' information.
#'
#' @name attributes
#' @aliases attributes inputs outputs
#' @param ...  A list expressions to be used as inputs for the current action.
#' See \code{\link{expressions}} for more information.
#' @param expr An expression in the same manner as above.
#' differently in individual contexts; in general, it acts much like the SQL
#' \code{*} construct for inputs.
#' @note The expressions for \code{inputs} and \code{outputs} are never
#' evaluated. Instead, they are parsed and the resulting abstract syntax tree
#' is deconstructed.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC
#' @seealso \code{\link{expression}} for information on how to specify
#' expressions.
#' @examples
#'
#' ## Proper specification of inputs and outputs
#' data <- Read(lineitem100g)
#' result <- Sum(data, inputs = c(l_discount, l_tax),
#' outputs = c(sum_discout, sum))
#' print(result)
#'
#' ## Improper assignment
#' data <- Read(lineitem100g)
#' inputs <- c(l_discount, l_tax) ## error thrown here
#' result <- Sum(data, inputs = inputs, outputs = c(sum_discout, sum))
NULL

