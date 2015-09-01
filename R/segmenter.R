Segmenter <- function(data, segment, passes = 1, num.segments = 64) {
  if (!("GLA" %in% class(data)))
    stop("Segment must be placed on top of an aggregate waypoint.")

  if (missing(segment))
    if (length(data$inputs) == 0)
      stop("Segment cannot be AUTO as the input GLA has no inputs.")
    else
      segment <- grokit$expressions[[data$inputs[[1]]]]
  else
    segment <- substitute(segment)
  segment <- convert.exprs(segment)
  data$inputs <- c(segment, data$inputs)

  data$gla <- GLA(Segmenter, passes = passes, segments = num.segments, GLA = data$gla)
  data
}
