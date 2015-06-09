#' Compute Multiple Aggregates Simultaneously
#'
#' Performs multiple aggregates on the data and returns the individual results
#' together in one waypoint.
#'
#' The inner GLAs should be specified as a list of calls to other aggregate
#' functions, such as Sum or Mean. In each of these calls, the \code{data}
#' argument should be omitted, as it is inferred to be the \code{data} passed to
#' \code{GroupBy}. Additionally, each argument specifying an inner GLA may be
#' named. If so, that name is taken to be the output of the corresponding GLA.
#' This is purely a stylistic shortcut and the normal method of specifying the
#' outputs can still be used instead.
#'
#' The outputs of these inner GLAs should avoid name clashing both with each
#' other.
#'
#' In the case that one inner GLA produces multiple rows and the rest produce a
#' single row, each of the single row outputs are repeated accordingly.
#'
#' If more than one inner GLA produces multiple rows, an error is thrown.
#'
#' @param data A \code{\link{waypoint}}.
#' @param \dots Specification of the inner GLAs. See \sQuote{details} for more
#'   information.
#' @return A \code{\link{waypoint}}.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
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
       outputs = if (is.null(names(agg$schema))) character() else add.class(names(agg$schema), "mapping"),
       states = agg$states,
       gla = agg$gla)
}
