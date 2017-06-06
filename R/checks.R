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
  missing <- subtract(atts, names(x$schema))
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

check.choices <- function(choices, allowed) {
  if (!is.call.to(choices, "c"))
    stop("Incorrect coice specification: ", deparse(choices))
  if (!is.call.to(allowed, "c"))
    stop("Incorrect list of allowed choices: ", deparse(allowed))
  
  c <- as.list(choices)[-1];
  a <- as.list(allowed)[-1];
  # check for duplicates in choices
  if (anyDuplicated(c) != 0)
    stop("Duplicated choices not allowed: ", deparse(choices))
  if (anyDuplicated(a) != 0)
    stop("Duplicated allowed choices not allowed: ", deparse(allowed))
  # check that all choices appear in allowed
  if (length(setdiff(c,a)) != 0)
    stop("Choices not on allowed list. Choices: ", deparse(choices), 
      "Allowed: ", deparse(allowed))
}