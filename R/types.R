#' Types in R Grokit.
#'
#' Details on the specification of types in R Grokit.
#'
#' To convert user-specified or detected types, two S3 method pipelines are used
#' consecutively. First, convert.types is called by functions that process types.
#' This set of methods ensures that the output is a list of objects of class
#' template. The list is necessary as the types will be translated to piggy by a
#' call to lapply. If a single template exists by itself and not within a length
#' one list, the lapply would instead go over the list representing the type.
#' The structure of convert.types closely mimics that of convert.exprs, as they
#' are both used to produce lists to be processed. Secondly, convert.type does
#' the actual conversion. This function should only ever be called by function
#' convert.types.
#'
#' Types can be specified by the user as a call or a symbol. In the former case,
#' each argument that is a call is itself processed and then the whole call is
#' processed using the function called as the name and the arguments as the
#' template arguments.
#'
#' Additionally, types can be a value rather than a call. Although a user could
#' technically cause this directly, this usually arises from the automatic
#' detection of types within a file that is being read. In such a case, types in
#' R are directly mapped onto Grokit types. It should be noted that factors are
#' treated as strings rather than as factors or categories, as not information
#' can be discerned from a single value to template the type.
#'
#' The types that can be converted is subject to change as more types are added
#' to Grokit. If a type can be converted, then the S3 function convert.type must
#' have a method defined for that class. Otherwise, an error is thrown.

convert.type <- function(type) UseMethod("convert.type")

convert.type.default <- function(type)
  stop("Unrecognized type: ", type, " has type ", typeof(type))

convert.type.call <- function(type) {
  if (is.call.to(type, "::") && all(is.symbols(as.list(type)))) {
    eval(as.call(c(substitute(TYPE), type)))
  } else {
    name <- type[[1]]
    assert(is.identifier(name), "invalid type: ", name)
    args <- as.list(type)[-1]
    names <- names(args)
    args <- ifelse(lapply(args, is.language), lapply(args, convert.types), args)
    names(args) <- names
    eval(as.call(c(substitute(TYPE), type[[1]], args)))
  }
}

convert.type.factor <- function(type)
  TYPE(string)

convert.type.integer <- function(type)
  TYPE(int)

convert.type.logical <- function(type)
  TYPE(bool)

convert.type.name <- function(type)
  eval(call("TYPE", type))

convert.type.numeric <- function(type)
  TYPE(double)

convert.type.character <- function(type)
  TYPE(string)

convert.types <- function(types) UseMethod("convert.types")

convert.types.default <- function(types)
  convert.types(list(types))[[1]]

convert.types.call <- function(types) {
  args <- as.list(types)[-1]
  if (is.call.to(types, "."))
    if (length(types) != 2)
      stop("the .() construct is only allowed a single argument.")
    else
      eval(types[[2]])
  else
    convert.types(list(types))[[1]]
}

convert.types.list <- function(types) {
  lapply(types, convert.type)
}
