TemplateFunction <- function(.type, .name, ...) {
  .type <- substitute(.type)
  args <- list(...)
  .name <- substitute(.name)
  if (is.symbol(.name))
    .name <- call("::", as.symbol("base"), .name)
  if (!(is.call.to(.name, "::") && all(is.symbols(as.list(.name)))))
    Stop("invalid template name: ", deparse(.name))
  if (!(as.character(.name[[2]]) %in% grokit$libraries))
    Stop("unloaded library called: ", deparse(.name))
  types <- c("GLA", "UDF", "GF", "GIST", "GT", "GI", "TYPE")
  if (!(is.symbol(.type) && as.character(.type) %in% types))
    Stop("invalid template type: ", deparse(.type))
  template <- list(type = .type, library = .name[[2]], name = .name[[3]], args = args)
  class(template) <- c("Template")
  template
}

GF <- function(...) TemplateFunction(GF, ...)
GT <- function(...) TemplateFunction(GT, ...)
GI <- function(...) TemplateFunction(GI, ...)
GLA <- function(...) TemplateFunction(GLA, ...)
UDF <- function(...) TemplateFunction(UDF, ...)
GIST <- function(...) TemplateFunction(GIST, ...)
TYPE <- function(...) TemplateFunction(TYPE, ...)

MakeTemplate <- function(type, name, args) {
  type <- substitute(type)
  eval(as.call(c(substitute(TemplateFunction), .type = type, .name = name, args)))
}
