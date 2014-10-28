Segmenter <- function(data, segment = AUTO, passes = 1, num.segments = 64) {
  if (!("GLA" %in% class(data)))
    Stop("Segment must be placed on top of an aggregate waypoint.")

  segment <- substitute(segment)
  check.exprs(segment)
  if (is.auto(segment))
    if (length(data$inputs) == 0)
      Stop("Segment cannot be AUTO as the input GLA has no inputs.")
    else
      segment <- grokit$expressions[[data$inputs[[1]]]]
  segment <- convert.exprs(segment)
  data$inputs <- c(segment, data$inputs)

  data$gla <- GLA(Segmenter, passes = passes, segments = num.segments, GLA = data$gla)
  data
}
