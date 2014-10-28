GroupBy <- function(data, group, ..., init.size = 65536, use.mct = TRUE, debug = 0) {
  group <- substitute(group)
  keys <- names(group)[-1]
  check.exprs(group)
  if (is.auto(group))
    Stop("group is not allowed to be AUTO.")
  group <- convert.exprs(group)

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
  fun <- GLA(GroupBy, group = keys, aggregate = aggregate, debug = debug, init.size = init.size, use.mct = use.mct)

  Aggregate(data, fun, inputs, outputs)
}
