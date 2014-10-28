median <- function(x, ...) UseMethod("median")

median.default <- stats::median.default

median.data <- function(x, ...) Median(x, ...)

Median <- function(data, number.bins = 1000, sort.threshold = 1000,
                   inputs = AUTO, outputs = result) {
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
  if (length(outputs) != 1)
    Stop("There must be exactly one output specified.")

  gla <- GLA(statistics::Median_Binning,
             number.bins = number.bins,
             sort.threshold = sort.threshold)
  agg <- Aggregate(data, gla, inputs, outputs)
  agg
  ## if (exists("grokit.jobid") && !force.frame) {
  ##   View(agg)
  ## } else {
  ##   result <- data.frame(as.object(agg)$content[[1]][[1]])
  ##   iteration <- as.integer(result[["iteration"]][[1]])
  ##   row.names(result) <- ifelse(lapply(grokit$expressions[inputs], is.symbol),
  ##                               grokit$expressions[inputs],
  ##                               paste0("V", 1:length(inputs)))
  ##   result <- data.frame(t(result))["medians", ]
  ##   attr(result, "iteration") <- iteration
  ##   result
  ## }
}
