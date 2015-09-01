## attributes should be language
check.atts <- function(attributes) {
  if (is.call.to(attributes, "c") && !all(is.attributes(as.list(attributes)[-1])))
    stop("attributes specified incorrectly: ", deparse(attributes))
  else if (!is.call.to(attributes, "c") && !is.attributes(list(attributes)))
    stop("attribute specified incorrectly:", deparse(attributes))
}

check.exprs <- function(expressions) {
  if (!(length(expressions) == 1 || is.call(expressions)))
    stop("expressions specified incorrectly: ", deparse(expressions))
}

check.inputs <- function(x, schema) {
  expressions <- grokit$expressions[schema]
  atts <- unlist(lapply(expressions, function(expression) {
    unlist(lapply(extract.symbols(expression), as.character))
  }))
  missing <- setdiff(atts, names(x$schema))
  if (length(missing) != 0)
    stop("the following attributes are missing:\n",
         paste0("\t", missing, collapse = "\n"))
}

check.missing <- function() {
  expected <- formals(fun = sys.function(-1))
  actual <- sys.call(-1)
}

check.schema <- function(x, schema) {
  atts <- x$schema[schema]
  if (any(bad <- !(schema) %in% names(x$schema)))
    stop("missing attributes: ", paste(schema[bad], collapse = ", "))
}
