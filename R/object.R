as.object.default <- function(data, ...) {
  expressions <- as.list(substitute(list(...)))[-1]
  if (length(expressions) == 0)
    expressions <- lapply(data$schema, as.symbol)
  names <- names(expressions)
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

  result <- GetResult(data, "json", atts)
  fromJSON(file = result)
}

as.object.glm <- function(data, ...) {
  result <- as.object(data)$content[[1]][[1]]
  result$coefficients <- result$coefficients$data
  names(result$coefficients) <- c(if (intercept) "(Intercept)" else NULL, terms)
  result
}
