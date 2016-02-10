TemplateFunction <- function(.type, .name, ..., .infer = T) {
  .type <- substitute(.type)
  args <- list(...)
  .name <- substitute(.name)
  if (is.symbol(.name))
    .name <- call("::", as.symbol("base"), .name)

  assert(is.call.to(.name, "::") && all(is.symbols(as.list(.name))),
         "invalid template name: ", deparse(.name))

  lib <- as.character(.name[[2]])
  if (!lib %in% grokit$libraries)
    grokit$libraries <- c(grokit$libraries, lib)

  types <- c("GLA", "UDF", "GF", "GIST", "GT", "GI", "TYPE")
  assert(is.symbol(.type) && as.character(.type) %in% types,
         "invalid template type: ", deparse(.type))

  ## Inferring argument names. If an argument name is not specified but the
  ## argument was given as a symbol in the call, that symbol is used instead.
  ## TODO: Possibly adapt the data.frame function's functionality for this.
  if (.infer) {
    keys <- names(args)
    exprs <- as.list(substitute(list(...)))[-1]
    names(args) <-
      ifelse(if (is.null(keys)) rep(TRUE, length(args)) else keys == "",
             ifelse(is.symbols(exprs), as.character(exprs), ""),
             keys)
  }

  ## A value of NA for a template argument is used to specify that that argument
  ## should be omitted so that the default behavior in the PHP function is used.
  ## NULL values are supported and not modified here.

  ## TODO:
  ## Currently, no differentiation is made between values of NA and NaN because
  ## NaNs are not supported by Piggy. If support is added in the future, this
  ## function must have functionality added for it.
  args <- args[!is.na(args)]

  template <- list(type = .type, library = .name[[2]], name = .name[[3]], args = args)
  class(template) <- c("Template")
  template
}

GF   <- function(...) TemplateFunction(GF,   ...)
GT   <- function(...) TemplateFunction(GT,   ...)
GI   <- function(...) TemplateFunction(GI,   ...)
GLA  <- function(...) TemplateFunction(GLA,  ...)
UDF  <- function(...) TemplateFunction(UDF,  ...)
GIST <- function(...) TemplateFunction(GIST, ...)
TYPE <- function(...) TemplateFunction(TYPE, ...)

MakeTemplate <- function(type, name, args) {
  type <- substitute(type)
  eval(as.call(c(substitute(TemplateFunction), .type = type, .name = name, args)))
}
