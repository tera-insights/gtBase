check.alias <- function(alias) {
  if (alias %in% grokit$waypoints)
    alias <- get.alias(strsplit(alias, "_", TRUE)[[1]][[1]])
  grokit$waypoints <- c(grokit$waypoints, alias)
  alias
}

## attributes should be language
check.atts <- function(attributes, auto = TRUE) {
  if (is.call.to(attributes, "c") && !all(is.attributes(as.list(attributes)[-1])))
    Stop("attributes specified incorrectly.")
  else if (!is.call.to(attributes, "c") && !is.attributes(list(attributes)))
    Stop("attribute specified incorrectly.")
  else if (is.auto(attributes) && !auto)
    Stop("AUTO used illegally.")
}

check.exprs <- function(expressions, auto = TRUE) {
  if (!(length(expressions) == 1 || is.call(expressions)))
    Stop("expressions specified incorrectly.")
  else if (is.auto(expressions) && !auto)
    Stop("AUTO used illegally.")
}

check.inputs <- function(x, schema) {
  expressions <- grokit$expressions[schema]
  atts <- unlist(lapply(expressions, function(expression) {
    as.character(extract.symbols(expression))
  }))
  missing <- subtract(atts, names(x$schema))
  if (length(missing) != 0)
    Stop("the following attributes are missing:\n",
         paste0("\t", missing, collapse = "\n"))
}

check.missing <- function() {
  expected <- formals(fun = sys.function(-1))
  actual <- sys.call(-1)
}

check.schema <- function(x, schema) {
  atts <- x$schema[schema]
  if (any(bad <- !(schema) %in% names(x$schema)))
    Stop("missing attributes: ", paste(schema[bad], collapse = ", "))
}
