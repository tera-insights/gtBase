Stop <- function(..., traceback = rev(sys.calls())) {
  class(traceback) <- "pairlist"
  if (getOption("stacktrace", FALSE))
    cat(paste0(length(traceback):1, ": ", traceback, "\n"))
  if (exists("grokit.jobid")) {
    tr <- list(line = 1, col = 1, line.end = 1, col.end = 1)
    message <- gsub("\t", " ", gsub("\n", " ", paste0(...)))
    error <- list(
        `__type__` = "error",
        message = message,
        kind = "R",
        line = tr$line,
        column = tr$col,
        line_end = tr$line.end,
        column_end = tr$col.end
        )
    cat(paste0(...))
    throw.error(error)
  } else {
    if (length(list(...)) != 0)
      stop(..., call. = FALSE)
    else
      stop("error occurred in standard R.")
  }
}

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

eval. <- function(expr, data, envir = parent.frame()) {
  if (is.call(expr))
    if (is.call.to(expr, "."))
      if (length(expr) != 2)
        Stop("the .() construct is only allowed a single argument.")
      else
        eval(expr[[2]], envir)
    else if (is.call.to(expr, "@")) ## Evaluating attribute reference
      if (all(is.symbols(as.list(expr))))
        if (!is.data(other <- eval(expr[[2]], .GlobalEnv)))
          Stop("invalid waypoint referenced: ", deparse(expr))
        else if (as.character(expr[[3]]) %in% names(other$schema))
          names(data$schema)[[which(other$schema[[as.character(expr[[3]])]] == data$schema)[[1]]]]
        else
          Stop("missing attribute referenced: ", deparse(expr))
      else
        Stop("attribute reference used incorrectly: ", deparse(expr))
    else
      as.call(lapply(expr, eval., envir, data))
  else
    expr
}

process <- function(expression, data) {
  if (is.symbol(expression))
    as.symbol(data$schema[[as.character(expression)]])
  else
    as.call(c(expression[[1]], lapply(as.list(expression)[-1], process, data)))
}

quotate <- function(x, wrapper = '"') {
  brackets <- c("{" = "}", "(" = ")", "[" = "]")
  if (is.bracket(as.symbol(wrapper)))
    end <- brackets[[wrapper]]
  else
    end <- wrapper
  unlist(lapply(x, function(x) {
    x <- as.character(x)
    if (x == "")
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
    names <- expressions <- names(data$schema)
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
    names[indices] <- unlist(lapply(exprs[indices], as.character))
    names[!indices] <- paste0("V", which(!indices))
  } else {
    names[indices & names == ""] <- unlist(lapply(exprs[indices & names == ""], as.character))
    names[!indices & names == ""] <- paste0("V", which(!indices & names == ""))
  }
  names(atts) <- names
  atts
}

get.schema <- function() {
  temp <- tempfile("schema", ".", ".txt")
  system2("grokit-cli", args = c("schema", temp, mget("grokit.jobid", envir = .GlobalEnv, ifnotfound = "")))
  schema <- fromJSON(file = temp)
  file.remove(temp)
  schema
}

get.exprs <- function(...) grokit$expressions[c(...)]

add.class <- function(x, class) {
  class(x) <- c(class(x), class)
  x
}

set.class <- function(x, class) {
  class(x) <- class
  x
}

set.names <- function(x, names) {
  if (!is.vector(x))
    x <- as.list(x)
  names(x) <- names
  x
}

as.symbols <- function(x) lapply(x, as.symbol)

get.alias <- function(type = "alias") {
  if (type %in% names(grokit$alias))
    grokit$alias[[type]] <- grokit$alias[[type]] + 1
  else
    grokit$alias[[type]] <- 1
  grokit$waypoints <- paste0(type, "_", grokit$alias[[type]])
}

subtract <- function(x, y) x[!(x %in% y)]

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
