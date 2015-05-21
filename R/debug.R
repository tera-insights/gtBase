Stop <- function() {
  if (exists("grokit.jobid")) {
    tb <- traceback(2)
    refs <- lapply(tb, attr, "srcref")
    locs <- ifelse(lapply(refs, is.null), list(), lapply(refs, head, 4))
    first <- max(which(!unlist(lapply(refs, is.null))))
    tr <-
      if (first == -Inf)
        list(line = -1, col = -1, line.end = -1, col.end = -1)
      else
        setNames(as.list(locs[[first]]), c("line", "col", "line.end", "col.end"))
    error <- list(
        `__type__` = "error",
        message = geterrmessage(),
        kind = "R",
        traceback = as.list(tb),
        locations = locs,
        line = tr$line,
        column = tr$col,
        line_end = tr$line.end,
        column_end = tr$col.end
        )
    throw.error(error)
  }
}

Traceback <- function() {
  tr <- .Internal(traceback(0))
  err <- tr[[tail(which(!as.logical(lapply(lapply(tr, attr, "srcref"), is.null))), 1)]]
  src <- attr(err, "srcref")
  line <- src[[1]]
  col <- src[[2]]
  line.end <- src[[3]]
  col.end <- src[[4]]
  list(line = line, col = col, line.end = line.end, col.end = col.end)
}

throw.error <- function(error) {
  result <- toJSON(error)
  ## print(result)
  temp <- tempfile("error", fileext = ".json")
  sink(temp)
  cat(result)
  sink()
  system2("grokit-cli", args = c("error", grokit.jobid, temp))
  quit(save = "no")
}
