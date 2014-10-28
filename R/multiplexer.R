Multiplexer <- function(data, ...) {
  aggs <- MultiplexerMake(..., data = data)
  Aggregate(data, aggs$GLA, aggs$inputs, aggs$outputs)
}

MultiplexerMake <- function(..., data) {
  aggs <- as.list(substitute(list(...)))[-1]
  values <- if (is.null(names(aggs))) rep("", length(aggs)) else names(aggs)
  aggs <- mapply(MultiplexerItem, aggs, values, MoreArgs = list(data = data), SIMPLIFY = FALSE)

  inputs <- as.character(unlist(lapply(aggs, `[[`, "inputs")))
  check.inputs(data, inputs)
  outputs <- unlist(lapply(aggs, `[[`, "outputs"))
  aggregate <- GLA(Multiplexer, glas = aggs)
  list(inputs = inputs, outputs = outputs, GLA = aggregate)
}

MultiplexerItem <- function(agg, value, data) {
  agg <- c(as.list(agg), data = list(data))
  if (value != "")
    agg <- c(agg, outputs = as.symbol(value))
  grokit.copy <- as.list(grokit)[c("alias", "outputs")]
  agg <- eval(as.call(agg))
  mapply(assign, names(grokit.copy), grokit.copy, MoreArgs = list(envir = grokit))
  list(inputs = as.symbols(agg$inputs),
       outputs = add.class(names(agg$schema), "mapping"),
       gla = agg$gla)
}
