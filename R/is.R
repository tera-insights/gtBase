is.attributes <- function(attributes) {
  unlist(lapply(attributes, function(attribute) {
    all(is.symbols(as.list(attribute))) && (length(attribute) == 1 || is.call.to(attribute, "@"))
  }))
}

is.auto <- function(attributes) {
  is.symbol(attributes) && attributes == "AUTO"
}

is.bracket <- function(expr) {
  brackets <- c("[", "(", "{")
  is.symbol(expr) && as.character(expr) %in% brackets
}

is.call.to <- function(exprs, name) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  as.logical(lapply(exprs, function(expr) {
    is.call(expr) && is.symbol(expr[[1]]) && expr[[1]] == name
  }))
}

is.calls <- function(exprs) {
  as.logical(lapply(exprs, is.call))
}

is.data <- function(x) "data" %in% class(x)

## First character is alphabetical, rest are alpha-numeric or underscore.
is.identifier <- function(names) {
  if (length(names) == 0)
    logical()
  else
    1:length(names) %in% grep("[[:alpha:]_]", substring(names, 1, 1)) &
      !(1:length(names) %in% grep("[^[:alnum:]_]", substring(names, 2)))
}

is.method <- function(expr) is.call.to(expr, "$")

is.operator <- function(expr) {
  operators <- c("$", "@", "[", "[[", "^", "-", "+", ":", "*",
                 "/", "<", ">", "<=", ">=", "==", "!=", "!", "&",
                 "&&", "|", "||", "~", "<-", "<<-", "=", "?")
  is.symbol(expr) && (as.character(expr) %in% operators
                      || (substring(as.character(expr), 1, 1) == "%"
                          && "%" == substring(as.character(expr), nchar(as.character(expr)))))
}

is.symbols <- function(exprs) {
  if (!is.list(exprs))
    exprs <- list(exprs)
  as.logical(lapply(exprs, is.symbol))
}

is.typedef <- function(names) {
  unlist(lapply(names, as.character)) %in% grokit$typedefs
}

is.UDF <- function(expr) is.call.to(expr, "[")

is.whole <- function(x) is.numeric(x) && length(x) == 1 && (round(x) - x == 0)
