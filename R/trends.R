Trends <- function(data, ..., inputs = AUTO, outputs = result, force.frame = FALSE) {
  inputs <- substitute(inputs)
  check.exprs(inputs)
  if (is.auto(inputs))
    inputs <- convert.schema(data$schema)
  inputs <- convert.exprs(inputs)

  outputs <- substitute(outputs)
  check.atts(outputs)
  if (is.auto(outputs))
    Stop("outputs not allowed to be AUTO.")
  outputs <- convert.atts(outputs)
  if (length(outputs) != 3)
    Stop("There must be exactly three outputs specified.")

  constructor <- TrendsMake(...)
  agg <- Aggregate(data, constructor$GLA, inputs, outputs)
  agg
}

TrendsMake <- function(positive = list(exp), negative = list(function(x) exp(-x)),
                       threshold = 256, begin, end, scale = 1, intervals = 32L) {
  if (!is.integer(intervals) || intervals <= 0)
    Stop("intervals must be a positive integer.")
  transform <- function(x) {
    if (is.numeric(x)) {
      if (length(x) == intervals)
        as.unit(x)
      else
        Stop("numeric vector must be length ", intervals)
    } else if (is.function(x)) {
      x <- unlist(lapply(1:intervals, x))
      if (is.numeric(x) && length(x) == intervals)
        as.unit(x)
      else
        Stop("a function specifying a trend must return numeric of length ", intervals)
    } else
      Stop("trends each must be a numeric or a function.")
  }

  pos <- lapply(lapply(positive, transform), as.unit)
  neg <- lapply(lapply(negative, transform), as.unit)

  if (!any(c(class(begin), class(end)) %in% c("Date", "POSIXlt", "POSIXct", "integer")))
    Stop("begin and end must specify a date-time with one of the following four classes:\n",
         "Date, POSIXct, POSIXlt, integer")
  if (any(c("Date", "POSIXlt") %in% class(begin)))
      begin <- as.POSIXct(begin)
  if (any(c("Date", "POSIXlt") %in% class(end)))
      end <- as.POSIXct(end)
  begin <- as.integer(begin)
  end <- as.integer(end)
  if (begin >= end)
    Stop("begin time must be chronologically before end time.")

  GLA <- GLA(
      gs::Trends,
      pos = pos,
      neg = neg,
      threshold = threshold,
      begin = begin,
      end = end,
      scale = scale,
      intervals = intervals
      )

  list(GLA = GLA)
}
