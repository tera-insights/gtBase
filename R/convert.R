convert.args <- function(arg, renaming) UseMethod("convert.args")

convert.args.attribute <- function(att, renaming) {
  if (att %in% names(renaming))
    set.class(renaming[[att]], "attribute")
  else
    att
}

convert.args.default <- function(arg, renaming) arg

convert.args.list <- function(list, renaming) lapply(list, convert.args, renaming)

convert.args.mapping <- function(mapping, renaming) {
  set.class(set.names(as.character(lapply(mapping, convert.args.attribute, renaming)), names(mapping)), "mapping")
}

convert.args.Template <- function(template, renaming) {
  template$args <- lapply(template$args, convert.args, renaming)
  template
}

convert.atts <- function(attributes, data = NULL) {
  if (is.call.to(attributes, "c"))
    unlist(lapply(as.list(attributes)[-1], convert.atts))
  else if (is.call.to(attributes, "@"))
    if (is.null(data))
      Stop("attribute reference used incorrectly.")
    else if (!is.data(other <- eval(attributes[[2]], .GlobalEnv)))
      Stop("invalid waypoint referenced: ", deparse(attributes))
    else if (as.character(attributes[[3]]) %in% names(other$schema))
      names(data$schema)[[which(other$schema[[as.character(attributes[[3]])]] == data$schema)[[1]]]]
    else
      Stop("missing attribute referenced: ", deparse(attributes))
  else if (is.symbol(attributes))
    as.character(attributes)
  else
    Stop("attribute specified incorrectly.")
}

convert.exprs <- function(...) UseMethod("convert.exprs")

convert.exprs.default <- function(expressions, data, atts = NULL) {
  convert.exprs(as.list(expressions), data, atts)
}

convert.exprs.if <- function(expressions, data, atts = NULL)
  convert.exprs(list(expressions), data, atts)

convert.exprs.call <- function(expressions, data, atts = NULL) {
  if (is.call.to(expressions, "c"))
    convert.exprs(as.list(expressions)[-1], data, atts)
  else
    convert.exprs(list(expressions), data, atts)
}

convert.exprs.name <- function(expressions, data, atts = NULL) {
  if (is.auto(expressions))
    Stop("AUTO used illegally.")
  else if (expressions == "")
    character()
  else
    convert.exprs(list(expressions), data, atts)
}

convert.exprs.list <- function(expressions, data, atts = NULL) {
  ## Do evaluation of .() constructs now. Lazy evaluation is bad because re-binding.
  expressions <- lapply(expressions, eval., data = data)

  if (is.null(atts))
    atts <- paste0("expr", (length(grokit$expressions) + 1):(length(grokit$expressions) + length(expressions)))
  else if (length(atts) != length(expressions))
    Stop("in convert.exprs, atts must be the same length as expressions.")
  grokit$expressions[atts] <- expressions
  atts
}

convert.outputs <- function(names, update = TRUE) {
  as.character(lapply(names, function(name) {
    while (name %in% grokit$outputs)
      name <- paste0("_", name)
    if (update)
      grokit$outputs <- c(grokit$outputs, name)
    name
  }))
}

convert.schema <- function(schema) {
  as.call(c(as.symbol("c"), lapply(schema, as.symbol)))
}

convert.typename <- function(type) {
  characters <- strsplit(type, "")[[1]]
  if (all(characters != ":"))
    type <- paste0("base::", type)
  tolower(type)
}
