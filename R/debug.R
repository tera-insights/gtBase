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
