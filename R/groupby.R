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
