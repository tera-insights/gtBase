#' Compute Aggregates of Data Subsets
#'
#' Splits the data into subsets based on grouping expressions, performs the
#' given aggregates per subset, and returns the results in a convenient form.
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
#' other and those for the grouping expressions.
#'
#' In the case that one inner GLA produces multiple rows and the rest produce a
#' single row, each of the single row outputs are repeated accordingly.
#'
#' If more than one inner GLA produces multiple rows, an error is thrown.
#'
#' The output for each group is then concatenated and returned.
#'
#' @param data A \code{\link{waypoint}}.
#' @param group A named list of expressions, with the names being used as the
#'   corresponding outputs. These expressions are outputted in addition the
#'   results of the inner GLAs.
#'
#'   If no name is given and the corresponding  expression is simply an
#'   attribute, then said attribute is used as the name. Otherwise, the column
#'   for that expression is hidden from the user.
#' @param \dots Specification of the inner GLAs. See \sQuote{details} for more
#'   information.
#' @param fragment.size The number of tuples returned per fragment. This should
#'   only be changed from its default value by experienced users.
#' @param init.size The number of groups that space is initially allocated for.
#' @param use.mct Should the MCT hash function be used.
#' @param debug Should debugging information be printed to standard output.
#' @param states Additional states to pass through.
#' @return A \code{\link{waypoint}}.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
GroupBy <- function(data, group, ..., fragment.size = 2000000, init.size = 1024,
                    use.mct = TRUE, debug = 0, states = list()) {
  group <- substitute(group)
  keys <- names(group)[-1]
  check.exprs(group)
  if (is.auto(group))
    stop("group is not allowed to be AUTO.")
  group <- convert.exprs(group, data)

  ## key name used if given, else the key if said key is a symbol, else hidden name.
  keys <- ifelse(if (is.null(keys)) rep(TRUE, length(group)) else keys == "",
                 ifelse(is.symbols(get.exprs(group)),
                        as.character(get.exprs(group)),
                        paste0("_groupAtt", 1:length(group))),
                 keys)

  names(keys) <- group
  class(keys) <- "mapping"

  ## Multiplexer is removed if there is a single inner GLA
  GLAs <- MultiplexerMake(..., data = data)
  if (length(GLAs$GLA$args$glas) == 1)
    aggregate <- GLAs$GLA$args$glas[[1]]$gla
  else
    aggregate <- GLAs$GLA

  inputs <- c(group, GLAs$inputs)
  outputs <- c(keys, GLAs$outputs)
  fun <- GLA(GroupBy, group = keys, aggregate = aggregate, debug = debug,
             fragment.size = fragment.size, init.size = init.size, use.mct = use.mct)

  Aggregate(data, fun, inputs, outputs, states)
}
