Translate.Cache <- function(data) {
  piggy <- paste0(data$alias, " = CACHE ", data$data$alias, ";\n")
  c(Translate(data$data), setNames(piggy, data$alias))
}

Translate.Compact <- function(data) {
  piggy <- paste0(data$alias, " = COMPACT ", data$data$alias, ";\n")
  c(Translate(data$data), setNames(piggy, data$alias))
}

Translate.Filter <- function(filter) {
  update.clustering(grokit$expressions[[filter$condition]], filter$data)
  c(Translate(filter$data),
    setNames(
        paste0(filter$alias, " = FILTER ", filter$data$alias, " BY",
               "\n\t", Translate.Expr(grokit$expressions[[filter$condition]], filter$data),
               ";\n"),
        filter$alias))
}

Translate.Generated <- function(generator) {
  c(Translate(generator$data),
    setNames(
        paste0(generator$alias, " = FOREACH ", generator$data$alias, " GENERATE",
               paste0("\n\t", Translate.Inputs(generator$generated, generator$data), collapse = ","),
               ";\n"),
        generator$alias))
}

Translate.GF <- function(gf) {
  c(unlist(lapply(gf$states, Translate)), Translate(gf$data),
    setNames(
        paste0(gf$alias, " = FILTER ", gf$data$alias, " BY",
               "\n", Translate(gf$gf),
               if (length(gf$states) > 0)
               paste0("\nREQUIRES", paste0("\n\t", lapply(gf$states, `[[`, "alias"), collapse = ",")),
               if (length(gf$inputs) > 0)
               paste0("\nUSING", paste0("\n\t", Translate.Inputs(gf$inputs, gf$data), collapse = ",")),
               ";\n"),
        gf$alias))
}

Translate.GI <- function(data) {
  atts <- if (is.character(data$outputs))
    paste0("\nATTRIBUTES FROM ", data$outputs)
  else
    paste0("\nATTRIBUTES",
           paste0("\n\t", Translate.Outputs(data$schema),
                  ifelse(sapply(data$outputs, is.null), "", " : "),
                  lapply(data$outputs, Translate.Template),
                  collapse = ","))
  setNames(
      paste0(data$alias, " = READ",
             paste0("\n  FILE ", quotate(data$files), collapse = ""),
             "\nUSING",
             "\n", Translate(data$gi),
             if (length(data$chunk)) paste("\nCHUNKSIZE", data$chunk),
             atts,
             ";\n"),
      data$alias)
}

Translate.GIST <- function(gist) {
  c(unlist(lapply(gist$states, Translate)),
    setNames(
        paste0(gist$alias, " =",
               "\n", Translate(gist$gist),
               if (length(gist$states) > 0)
               paste0("\nREQUIRES", paste0("\n\t", lapply(gist$states, `[[`, "alias"), collapse = ",")),
               if (length(gist$schema) > 0)
               paste0("\nAS", paste0("\n\t", Translate.Outputs(gist$schema), collapse = ",")),
               ";\n"),
        gist$alias))
}

Translate.GLA <- function(gla) {
  c(unlist(lapply(gla$states, Translate)), Translate(gla$data),
    setNames(
        paste0(gla$alias, " =",
               "\n", Translate(gla$gla),
               "\nFROM ", gla$data$alias,
               if (length(gla$states) > 0)
               paste0("\nREQUIRES", paste0("\n\t", lapply(gla$states, `[[`, "alias"), collapse = ",")),
               if (length(gla$inputs) > 0)
               paste0("\nUSING", paste0("\n\t", Translate.Inputs(gla$inputs, gla$data), collapse = ",")),
               if (length(gla$schema) > 0)
               paste0("\nAS", paste0("\n\t", Translate.Outputs(gla$schema), collapse = ",")),
               ";\n"),
        gla$alias))
}

Translate.GT <- function(gt) {
  c(unlist(lapply(gt$states, Translate)), Translate(gt$data),
    setNames(
        paste0(gt$alias, " =",
               "\n", Translate(gt$gt),
               "\nFROM ", gt$data$alias,
               if (length(gt$states) > 0)
               paste0("\nREQUIRES", paste0("\n\t", lapply(gt$states, `[[`, "alias"), collapse = ",")),
               if (length(gt$inputs) > 0)
               paste0("\nUSING", paste0("\n\t", Translate.Inputs(gt$inputs, gt$data), collapse = ",")),
               if (length(gt$outputs) > 0)
               paste0("\nAS", paste0("\n\t", Translate.Outputs(gt$outputs), collapse = ",")),
               ";\n"),
        gt$alias))
}

Translate.Join <- function(join) {
  c(Translate(join$x), Translate(join$y),
    setNames(
        paste0(join$alias, " =", if(join$left.outer) " LEFT OUTER", " JOIN\n",
               "\t", join$x$alias, " BY (",
               paste0(lapply(join$xSchema, Translate.Expr.name, join$x), collapse = ", "),
               ")", ",\n",
               "\t", join$y$alias, " BY (",
               paste0(lapply(join$ySchema, Translate.Expr.name, join$y), collapse = ", "),
               ")", ";\n"),
        join$alias))
}

Translate.Load <- function(data) {
  loading <- paste("LOAD", data$relation, "AS", data$alias)
  if (!is.null(data$cluster)) {
    range <- as.numeric(grokit$cluster[[data$cluster]][1:2])
    clustering <- paste("\nFILTER RANGE", paste(ifelse(is.finite(range), range, "NULL"), collapse = ", "))
  }

  setNames(paste0(loading, if (!is.null(data$cluster)) clustering, ";\n"), data$alias)
}
