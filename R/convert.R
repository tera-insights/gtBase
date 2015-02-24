## convert.args is used to rename template that make use of inputs names to include the long names.
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
    unlist(lapply(as.list(attributes)[-1], convert.atts, data = data))
  else if (is.call.to(attributes, "@"))
    if (is.null(data))
      stop("attribute reference used incorrectly: ", deparse(attributes))
    else
      as.character(long.name(attributes, data))
  else if (is.symbol(attributes))
    as.character(attributes)
  else
    stop("attribute specified incorrectly:", deparse(attributes))
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

convert.exprs.list <- function(expressions, data, atts = NULL) {
  if (length(expressions) == 0)
    return(character())

  ## Do evaluation of .() constructs now. Lazy evaluation is bad because re-binding.
  expressions <- lapply(expressions, eval., data = data)

  if (is.null(atts))
    atts <- paste0("expr", (length(grokit$expressions) + 1):(length(grokit$expressions) + length(expressions)))
  else if (length(atts) != length(expressions))
    stop("in convert.exprs, atts must be the same length as expressions.")
  grokit$expressions[atts] <- expressions
  atts
}

convert.exprs.name <- function(expressions, data, atts = NULL) {
  if (is.auto(expressions))
    stop("AUTO used illegally.")
  else if (expressions == "")
    character()
  else
    convert.exprs(list(expressions), data, atts)
}

convert.outputs <- function(names, update = TRUE) {
  if (length(repeats <- which(duplicated(names))) != 0)
    stop("repeated column names: ", paste(names[repeats], collapse = ", "))
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
