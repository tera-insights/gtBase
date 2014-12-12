Stop <- function() {
  if (exists("grokit.jobid")) {
    tb <- lapply(head(tail(sys.calls(), -4), -1), deparse)
    tr <- list(line = 1, col = 1, line.end = 1, col.end = 1)
    error <- list(
        `__type__` = "error",
        message = geterrmessage(),
        kind = "R",
        traceback = tb,
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
  temp <- tempfile("", getwd(), ".json")
  sink(temp)
  cat(result)
  sink()
  system2("grokit-cli", args = c("error", grokit.jobid, temp))
  quit(save = "no")
}
