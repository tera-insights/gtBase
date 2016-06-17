## If inner.GLA is given, then it should be a call specifying another waypoint
## but without a data argument. Otherwise, the data given is taken to a GLA
## waypoint and the Segmenter is placed on top of it.
Segmenter <- function(data, segment, passes = 1, segments = 64, fragment = TRUE, inner.GLA) {
  if (!missing(inner.GLA)) {
    inner.GLA <- substitute(inner.GLA)
    inner.GLA[["data"]] <- data
    data <- eval(inner.GLA)
  } else if (!is(data, "GLA")) {
    stop("Segment must be placed on top of an aggregate waypoint.")
  }
  if (missing(segment))
    if (length(data$inputs) == 0)
      stop("Segment cannot be AUTO as the input GLA has no inputs.")
    else
      segment <- grokit$expressions[[data$inputs[[1]]]]
  else
    segment <- substitute(segment)
  segment <- convert.exprs(segment)
  data$inputs <- c(segment, data$inputs)

  data$gla <- GLA(Segmenter, passes, segments, inner.prefer.fragment = fragment, GLA = data$gla)
  data
}
