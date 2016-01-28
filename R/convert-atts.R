## This function takes in a quoted expression representing a set of attributes.
## The expression should either be a symbol, a call to the @ operator where both
## inputs are symbols, or a call to `c` whose inputs are all in those two forms.
## Examples include "x", "x@y", and "c(x, y@z)".
convert.atts <- function(atts, ...) UseMethod("convert.atts")

convert.atts.character <- function(atts, data = NULL, env = parent.frame()) {
  ## First the input is parsed into an expression.
  expr <- parse(text = atts)
  if (length(expr) != 1)
    stop("convert.atts expects a single deparsed expr.\nreceived: ", atts)

  ## The expression is then decoded.
  is.atts <- tryCatch({value <- eval(expr, env)
                       is.atts(value)},
                      error = identity)
  if (inherits(is.atts, "error"))
    is.atts <- FALSE
  if (!is.atts)
    convert.atts(substitute(expr[[1]], env))
  else
    value
}

convert.atts.default <- function(attributes, data = NULL) {
  if (is.call.to(attributes, "c"))
    unlist(lapply(as.list(attributes)[-1], convert.atts, data = data))
  else if (is.call.to(attributes, "@"))
    if (is.null(data))
      stop("attribute reference used incorrectly: ", deparse(attributes))
    else
      as.character(long.name(attributes, data))
  else if (is.symbol(attributes))
    as.character(attributes)
  else
    stop("attribute specified incorrectly: ", deparse(attributes))
}
