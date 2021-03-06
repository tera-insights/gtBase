#' Convert syntax.
#'
#' Converts R syntax into objects usable by GrokIt.
#'
#' This function is used as a master function to convert abstract user syntax
#' into the necessary R objects used by various functions in RGrokIt. This is
#' used throughout the system to transform expressions that are syntactically
#' correct but not able to be evaluated. Usually this is done to provide syntax
#' sugars to the front-end users, such as allowing them to specify inputs to
#' waypoints as R expressions, rather than quoted strings.
#'
#' This is done in the following manner:
#' \enumerate{
#' \item \code{syntax} is converted to an \link{expression} using
#' \code{\link{as.expression}} unless \code{syntax} is given as a character,
#' in which case \code{\link{parse}} is used.
#' \item The expression is then evaluated in the provided environment. If the
#' result is an object with the correct class, then that value is returned.
#' Otherwise \code{convert.[class]} is called, given an expression, the value,
#' and \code{envir}. The expression is the result of substitute \code{syntax}
#' in \code{envir}. The value is result of the evaluation, unless an error
#' was thrown during evaluation, in which case \code{NULL} is used. The
#' \code{\dots} arguments are also passed through after those three arguments.
## TODO: Scrap all of this. Use ~ formula operator. To substitute values into a
## formula, just use do.call("substitute", list(f, env)) where env is a list/env
## mapping of symbol names to symbols or values.
convert <- function(syntax, class, envir) {
  if (is.character(syntax))
    syntax <- parse(text = syntax)
  else
    syntax <- as.expression(syntax)
}

## convert.args is used to rename template args that make use of inputs names to include the long names.
convert.args <- function(arg, renaming) UseMethod("convert.args")

convert.args.attribute <- function(att, renaming) {
  if (att %in% names(renaming))
    set.class(renaming[[att]], "attribute")
  else
    att
}

convert.args.default <- function(arg, renaming) arg

convert.args.list <- function(list, renaming) lapply(list, convert.args, renaming)

convert.args.mapping <- function(mapping, renaming) {
  set.class(setNames(as.character(lapply(mapping, convert.args.attribute, renaming)), names(mapping)), "mapping")
}

convert.args.Template <- function(template, renaming) {
  template$args <- lapply(template$args, convert.args, renaming)
  template
}


convert.exprs <- function(expressions, data, atts = NULL) UseMethod("convert.exprs")

convert.exprs.default <- function(expressions, data, atts = NULL) {
  convert.exprs(as.list(expressions), data, atts)
}

convert.exprs.if <- function(expressions, data, atts = NULL)
  convert.exprs(list(expressions), data, atts)

convert.exprs.call <- function(expressions, data, atts = NULL) {
  if (is.call.to(expressions, "c"))
    convert.exprs(as.list(expressions)[-1], data, atts)
  else
    convert.exprs(list(expressions), data, atts)
}

convert.exprs.list <- function(expressions, data, atts = NULL) {
  if (length(expressions) == 0)
    return(character())

  ## Do evaluation of .() constructs now. Lazy evaluation is bad because re-binding.
  expressions <- lapply(expressions, eval., data = data)

  if (is.null(atts))
    atts <- paste0("expr", (length(grokit$expressions) + 1):(length(grokit$expressions) + length(expressions)))
  else if (length(atts) != length(expressions))
    stop("in convert.exprs, atts must be the same length as expressions.")
  grokit$expressions[atts] <- expressions
  setNames(atts, names(expressions))
}

convert.exprs.name <- function(expressions, data, atts = NULL) {
  if (expressions == "")
    character()
  else
    convert.exprs(list(expressions), data, atts)
}

## This function takes previously constructed inputs, i.e a character vector
## of names referenced in grokit$expressions, and re-converts them as if they
## had been entered again into another waypoint.
convert.inputs <- function(inputs) {
  assert(is.inputs(inputs), "convert.inputs passed invalid inputs.")
  convert.exprs(grokit$expressions[inputs])
}

## Given inputs with a "names" attribute, this returns the names attribute.
## If null, the attribute is converted to a character vector with one empty
## string per input. If no "names" attributes is present, it is treated as
## null per the standard behavior of `attr`.
convert.names <- function(inputs) {
  names <- names(inputs)
  if (is.null(names))
    rep("", length(inputs))
  else
    names
}

convert.outputs <- function(names, update = TRUE) {
  if (length(repeats <- which(duplicated(names))) != 0)
    stop("repeated column names: ", paste(names[repeats], collapse = ", "))
  as.character(lapply(names, function(name) {
    while (name %in% grokit$outputs)
      name <- paste0("_", name)
    if (update)
      grokit$outputs <- c(grokit$outputs, name)
    name
  }))
}

## This is used to treat a character vector of attributes name as inputs.
convert.schema <- function(schema) {
  assert(is.character(schema), "illegal input: ", schema)
  convert.exprs(lapply(schema, as.symbol))
}

convert.typename <- function(type) {
  characters <- strsplit(type, "")[[1]]
  if (all(characters != ":"))
    type <- paste0("base::", type)
  tolower(type)
}
