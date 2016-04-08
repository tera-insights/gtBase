## This takes in a formula or list of them and processes them by:
## 1. Interpreting the names. The list names are used over the left-hand side of
##    formulas. If neither are given, "expr" is used.
## 2. Long names and .() constructs are processed.
convert.exprs <- function(exprs, data) {
  ## A single formula is converted to a list containing it.
  if (is(exprs, "formula"))
    exprs <- list(exprs)

  ## The validity of the inputs is checked.
  if (!is.list(exprs) && all(sapply(exprs, inherits, "formula")))
    stop("expected a list of formulas.")

  ## The names are deduced.
  has.lhs <- sapply(exprs, length) == 3
  if (any(sapply(exprs[has.lhs], function(formula) !is.symbol(formula[[2]]))))
    stop("left hand side of formulas should only be single symbols")
  names <- convert.names(exprs)
  has.names <- names != ""
  if (any(has.names & has.lhs))
    warning("both formula and list names given. formula names ignored.")
  use.lhs <- !has.names & has.lhs
  names[use.lhs] <- sapply(exprs[use.lhs], function(x) as.character(x[[2]]))

  ## Removing the left hand side from the formulas.
  exprs[has.lhs] <- lapply(exprs[has.lhs], `[`, c(1, 3))

  ## The long names and .() constructs are processed.
  lapply(exprs, eval.)
}

## This function takes in a list of expressions and substitutes the shorthand
## names with the full names specified by schema.
convert.shorthand <- function(exprs, schema) {
  naming <- as.environment(setNames(lapply(schema, as.symbol), names(schema)))
  exprs <- lapply(exprs, function(expr) {
    eval(do.call("substitute", list(expr, naming)))
  })
}

## This function is simply a helper function on top of convert.exprs to ensure
## that the inputs only name attributes.
convert.atts <- function(atts) {
  result <- convert.exprs(atts)
  if (!all(sapply(result, is.symbol)))
    stop("expressions used instead of attributes.")
  result
}

## This function takes previously constructed inputs and re-names them such that
## they may be used in another waypoint.
convert.inputs <- function(exprs) {
  setNames(exprs, sapply(names(exprs), create.name, type = "input"))
}

## Given inputs with a "names" attribute, this returns the names attribute.
## If null, the attribute is converted to a character vector with one empty
## string per element. If no "names" attributes is present, it is treated as
## null per the standard behavior of `attr`.
convert.names <- function(inputs) {
  names <- names(inputs)
  if (is.null(names))
    rep("", length(inputs))
  else
    names
}

convert.outputs <- function(names) {
  if (length(repeats <- which(duplicated(names))) != 0)
    stop("repeated column names: ", paste(names[repeats], collapse = ", "))
  sapply(names, create.name, type = "output")
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
