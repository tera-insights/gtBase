## No checking done on validity of conditional expression.
## TODO: Use error codes or parsing in R to determine correctness.
`[.data` <- function(data, condition) {
  condition <- substitute(condition)
  if (condition[[1]] == "c")
    Stop("Condition is not allowed to be a listt of expressions.")
  check.exprs(condition)
  condition <- convert.exprs(condition, data)
  check.inputs(data, condition)
  alias <- get.alias("filter")
  schema <- data$schema
  filter <- list(data = data, alias = alias, schema = schema, condition = condition)
  class(filter) <- c("Filter", "data")
  filter
}
