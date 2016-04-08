extract.symbols <- function(expr) {
  if(is.symbol(expr))
    expr
  else if (is.method(expr))
    extract.symbols(expr[[2]])
  else if (is.call(expr))
    if (is.method(expr[[1]]))
      unlist(lapply(expr, extract.symbols))
    else
      unlist(lapply(expr[-1], extract.symbols))
  else
    NULL
}

. <- function(x) x

long.name <- function(expr, data, environment) {
  if (all(is.symbols(as.list(expr))))
    if (!is.data(other <- eval(expr[[2]], environment)))
      stop("invalid waypoint referenced: ", deparse(expr))
    else if (!(att <- as.character(expr[[3]])) %in% names(other$schema))
      stop("missing attribute referenced: ", deparse(expr))
    else if (!other$schema[[att]] %in% data$schema)
      stop("long name ", deparse(expr), " missing from current waypoint")
    else
      as.symbol(names(data$schema)[[which(other$schema[[as.character(expr[[3]])]] == data$schema)[[1]]]])
  else
    stop("attribute reference operator used incorrectly: ", deparse(expr))
}

eval. <- function(expr, data, envir = attr(expr, ".Environment")) {
  if (is.call(expr))
    if (is.call.to(expr, "."))
      if (length(expr) != 2)
        stop("the .() construct is only allowed a single argument.")
      else
        eval(expr[[2]], envir)
    else if (is.call.to(expr, "@")) ## Evaluating attribute reference
      long.name(expr, data, envir)
    else
      as.call(lapply(expr, eval., data, envir))
  else
    expr
}

process <- function(expression, data) {
  if (is.symbol(expression))
    as.symbol(data$schema[[as.character(expression)]])
  else
    as.call(c(expression[[1]], lapply(as.list(expression)[-1], process, data)))
}

## empty: whether to leave empty strings alone.
quotate <- function(x, wrapper = '"', empty = TRUE) {
  brackets <- c("{" = "}", "(" = ")", "[" = "]", "<" = ">")
  if (is.bracket(as.symbol(wrapper)))
    end <- brackets[[wrapper]]
  else
    end <- wrapper
  unlist(lapply(x, function(x) {
    x <- as.character(x)
    if (empty && x == "")
      x
    else
      paste0(wrapper, x, end)
  }))
}

.deparse <- function(expr) {
  x <- deparse(expr, width.cutoff = 500)
  if (length(x) > 1) {
    warning("Deparse exceeded maximum length. Possible error.")
    x[-1] <- substring(x[-1], 5)
    paste(x, collapse = "")
  } else {
    x
  }
}

switch <- function(EXPR, ...) {
  if (is.symbol(EXPR))
    base::switch(as.character(EXPR), ...)
  else
    base::switch(EXPR, ...)
}

backtick <- function(x) quotate(x, '`')

mod <- function(x) sqrt(sum(x ** 2))

as.unit <- function(x) x / mod(x)

name.exprs <- function(expressions, data) {
  if (length(expressions) == 0) {
    names <- expressions <- names(data$schema)[match(unique(data$schema), data$schema)]
    expressions <- lapply(expressions, as.symbol)
  } else {
    names <- names(expressions)
  }
  lapply(expressions, check.exprs)
  atts <- unlist(lapply(expressions, convert.exprs, data = data))
  check.inputs(data, atts)
  exprs <- grokit$expressions[atts]
  indices <- as.logical(lapply(exprs, is.symbol))
  if (is.null(names)) {
    names[ indices] <- unlist(lapply(exprs[indices], as.character))
    names[!indices] <- paste0("V", which(!indices))
  } else {
    names[ indices & names == ""] <- unlist(lapply(exprs[indices & names == ""], as.character))
    names[!indices & names == ""] <- paste0("V", which(!indices & names == ""))
  }
  names(atts) <- names
  atts
}

get.schema <- function() {
  offlineMode <- Sys.getenv("mode") == "offline"
  if (offlineMode) {
    schema <- fromJSON(file = "~/schema.json")
    schema
  } else {
    temp <- tempfile("schema", fileext = ".txt")
    system2("grokit-cli", args = c("schema", temp, mget("grokit.jobid", envir = .GlobalEnv, ifnotfound = "")))
    schema <- fromJSON(file = temp)
    file.remove(temp)
    schema
  }
}

get.exprs <- function(...) grokit$expressions[c(...)]

add.class <- function(x, class) {
  class(x) <- c(class, oldClass(x))
  x
}

set.class <- function(x, class) {
  class(x) <- class
  x
}

as.symbols <- function(x) lapply(x, as.symbol)

## create.name is used to generate unique names, separated by type.
## Given a name and a type, it returns a modification of the name that is
## different from all previously generated names for that type by appending
## an underscore and a number.
create.name <- function(name, type) {
  if (!(type %in% names(grokit$names)))
    grokit$names[[type]] <- integer()
  if (name %in% names(grokit$names[[type]]))
    grokit$names[[type]][[name]] <- grokit$names[[type]][[name]] + 1
  else
    grokit$names[[type]][[name]] <- 1
  alias <- paste0(name, "_", grokit$names[[type]][[name]])
  alias
}

create.alias <- function(name = "alias") {
  alias <- create.name(name, "waypoint")
  grokit$waypoints <- c(grokit$waypoints, alias)
  alias
}

## Grabs the base name from a generated name by stripping off the suffix, i.e.
## the underscore and number.
base.name <- function(name)
  paste(head(strsplit(name, "_")[[1]], -1), collapse = "_")

`%nin%` <- function(x, y) !(x %in% y)

make.unique <- function(names, lookup, invisible = FALSE) {
  if (invisible && length(names) > 0)
    names <- paste0("_", names)
  as.character(lapply(names, function(name) {
    while (name %in% lookup)
      name <- paste0("_", name)
    lookup <<- c(lookup, name)
    name
  }))
}

as.exprs <- function(expr) {
  if (is.call.to(expr, "c"))
    as.list(expr)[-1]
  else
    list(expr)
}

## This function takes in a character vector of library names and
## ensures they are loaded for any queries processed.
grokit.library <- function(libraries)
  grokit$libraries <- c(grokit$libraries, as.character(libraries))

get.args <- function(expr) as.list(expr)[-1]

num.args <- function(expr) {
  if (is.call(expr))
    ## as.numeric needed because sapply returns lists with 0 args
    sum(as.numeric(sapply(get.args(expr), num.args)))
  else
    1
}

assert <- function(condition, ...) if (!condition) stop(...)

warning.if <- function(condition, ...) if (condition) warning(...)

get.catalog <- function(relation) {
  catalog <- grokit$schemas$catalog
  relations <- unlist(lapply(catalog, `[[`, "name"))
  if (!(relation %in% relations))
    stop("unavailable relation: ", relation)

  index <- which(relations == relation)
  catalog[[index]]
}

get.relations <- function() sapply(grokit$schemas$catalog, `[[`, "name")

get.attributes <- function(relation) sapply(get.catalog(relation)$attributes, `[[`, "name")

is.relation <- function(relation)
  relation %in% get.relations()
}

## Given a templated object, this function returns the string library::name,
## where library and name are the relevant fields of the function. Note that a
## templated function is not the same thing as a waypoint that uses one.
get.function.name <- function(object) paste0(object$library, "::", object$name)

## This function is similar to setdiff but the names of x are kept.
subtract <- function(x, y) x[!(x %in% y)]

## This function deduced the relation name. If the input evaluates to a length
## one character vector, then that is used as the name. Otherwise, if the given
## expression is a symbol, that symbol is treated as the relation name. If not,
## an error is thrown. When calling this within another function,
