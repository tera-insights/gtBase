## Only single intervals constructed. Future support could be added for OR using
## multiple intervals in the final piggy code.
update.clustering <- function(expr, data) {
  index <- which(data$schema %in% names(grokit$cluster))
  if (length(index) == 0)
    ## Clustering attribute not visible
    return()
  cluster <- names(data$schema)[[index]]

  lower <- upper <- NULL

  if (is.call(expr) && is.symbol(expr[[1]]) && length(expr) == 3) {
    operator <- as.character(expr[[1]])

    if (operator == "&&") {
      update.clustering(expr[[2]], data)
      update.clustering(expr[[3]], data)
      return()
    }

    args <- as.list(expr)[-1]
    index <- which(is.symbols(args) & args == cluster)
    if (length(index) != 1)
      ## Clustering attribute uninvolved in condition
      return()
    literal <- args[[3 - index]]

    if (operator == "==") {
      do.bounding(literal, data$schema[[cluster]], "upper")
      do.bounding(literal, data$schema[[cluster]], "lower")
    } else if (operator %in% c("<", "<=")) {
      do.bounding(literal, data$schema[[cluster]], if (index == 1) "upper" else "lower")
    } else if (operator %in% c(">", ">=")) {
      do.bounding(literal, data$schema[[cluster]], if (index == 1) "lower" else "upper")
    }
  }
}

## bound shold be "upper" or "lower"
do.bounding <- function(literal, attribute, bound) UseMethod("do.bounding")

do.bounding.typeof <- function(literal, attribute, bound)
  stop("clusting attribute cannot be compared with type: ", typeof(literal))

do.bounding.numeric <- function(literal, attribute, bound) {
  literal <- if (bound == "lower") ceiling(literal) else floor(literal)
  if (bound == "lower")
    grokit$cluster[[attribute]]$lower <- max(literal, grokit$cluster[[attribute]]$lower)
  else
    grokit$cluster[[attribute]]$upper <- min(literal, grokit$cluster[[attribute]]$upper)
}

do.bounding.Date <- function(literal, attribute, bound) {
  type <- grokit$cluster[[attribute]]$type
  if (type == "base::date")
    literal <- as.integer(literal) + 2440588
  else if (type == "base::datetime")
    literal <- as.integer(as.POSIXct(literal))
  else
    stop("Date compared to ", type)
  do.bounding(literal, attribute, bound)
}

do.bounding.POSIXct <- function(literal, attribute, bound) {
  type <- grokit$cluster[[attribute]]$type
  if (type == "base::datetime")
    literal <- as.integer(literal)
  else
    stop("POSIXct compared to ", type)
  do.bounding(literal, attribute, bound)
}

do.bounding.POSIXlt <- function(literal, attribute, bound) {
  type <- grokit$cluster[[attribute]]$type
  if (type == "base::datetime")
    literal <- as.integer(as.POSIXct(literal))
  else
    stop("POSIXct compared to ", type)
  do.bounding(literal, attribute, bound)
}
