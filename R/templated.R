TemplateFunction <- function(.type, .name, ..., .infer = T) {
  .type <- substitute(.type)
  args <- list(...)
  .name <- substitute(.name)
  if (is.symbol(.name))
    .name <- call("::", as.symbol("base"), .name)

  assert(is.call.to(.name, "::") && all(is.symbols(as.list(.name))),
         "invalid template name: ", deparse(.name))

  assert(tolower(as.character(.name[[2]])) %in% tolower(grokit$libraries),
         "unloaded library called: ", deparse(.name))

  types <- c("GLA", "UDF", "GF", "GIST", "GT", "GI", "TYPE")
  assert(is.symbol(.type) && as.character(.type) %in% types,
         "invalid template type: ", deparse(.type))

  ## inferring argument names
  if (.infer) {
    keys <- names(args)
    exprs <- as.list(substitute(list(...)))[-1]
    names(args) <-
      ifelse(if (is.null(keys)) rep(TRUE, length(args)) else keys == "",
             ifelse(is.symbols(exprs), as.character(exprs), ""),
             keys)
  }

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
