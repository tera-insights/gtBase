Translate <- function(x, ...) UseMethod("Translate")

Translate.file <- function(file) {
  paste0("\"", file, "\"")
}

Translate.Load <- function(data) {
  ##  data$alias <- check.alias(data$alias)
  loading <- paste("LOAD", data$relation, "AS", data$alias)
  if (!is.null(data$cluster)) {
    range <- as.numeric(grokit$cluster[[data$cluster]][1:2])
    clustering <- paste("\nFILTER RANGE", paste(ifelse(is.finite(range), range, "NULL"), collapse = ", "))
  }

  paste0(loading, if (!is.null(data$cluster)) clustering, ";")
}

Translate.ReadRelation <- function(data) {
##  data$alias <- check.alias(data$alias)
  paste0(data$alias, " = READ FILE ", Translate(data$file), "\n",
         "USING ", Translate(data$gi), "\n",
         "ATTRIBUTES FROM ", data$relation, ";")
}

Translate.ReadFile <- function(data) {
##  data$alias <- check.alias(data$alias)
  paste0(data$alias, " = READ ", paste("FILE", Translate(data$file), collapse = " "), "\n",
         "USING ", Translate(data$gi), "\n",
         if (!is.null(data$chunk)) paste("CHUNKSIZE", data$chunk),
         "ATTRIBUTES\n",
         paste0("\t", Translate.Outputs(data$schema), " : ", lapply(data$types, Translate.Template),
                collapse = ",\n"),
         "\n",
         ";")
}

Translate.Template <- function(fun, level = 0) {
  if (!is.null(names(fun$args))) {
    prefixes <- quotate(names(fun$args))
    prefixes[prefixes != ""] <- paste(prefixes[prefixes != ""], "= ")
  } else {
    prefixes <- rep("", length(fun$args))
  }
  indent <- paste(rep("\t", level), collapse = "")
  paste0(fun$type, ":", fun$library, "\\", fun$name, "<",
         {
           if (length(fun$args) > 0)
             paste0("\n",
                    paste0(mapply(Translate.Arg, fun$args, level + 1, prefixes), collapse = ",\n"),
                    "\n", indent)
           else
             ""
           },
         ">")
}

Translate.Arg <- function(arg, level = 1, prefix = "") {
  pair <- paste0(typeof(arg), ", ", class(arg)[[length(class(arg))]])
  indent <- paste(rep("\t", level), collapse = "")
  body <-
    switch(
        pair,
        "NULL, NULL" = "NULL",
        "logical, logical" =,
        "integer, numeric" =,
        "integer, integer" =,
        "double, numeric" = {
          if (length(arg) == 0)
            "[ ]"
          else if (length(arg) == 1)
            arg
          else
            paste0("[", paste(arg, collapse = ", "), "]")
        },
        "character, character" = {
          if (length(arg) == 0)
            "[ ]"
          else if (length(arg) == 1)
           quotate(gsub('"', '\\\\"', arg))
          else
            paste0("[", paste0(quotate(gsub('"', '\\\\"', arg)), collapse = ", "), "]")
        },
        "symbol, name" = backtick(arg),
        "language, call" = {
          paste0(arg[[1]], "(", paste(lapply(as.list(arg)[-1], Translate.Arg), collapse = ", "), ")")
        },
        "list, Template" = Translate.Template(arg, level),
        "list, list" = {
          if (length(arg) == 0) {
            "[ ]"
          } else if (!is.null(names(arg))) {
            prefixes <- quotate(names(arg))
            prefixes[prefixes != ""] <- paste(prefixes[prefixes != ""], "= ")
            paste0("[\n",
                   paste0(mapply(Translate.Arg, arg, level + 1, prefixes), collapse = ",\n"),
                   "\n", indent, "]")
          } else {
            paste0("[\n",
                   paste(mapply(Translate.Arg, arg, level + 1, ""), collapse = ",\n"),
                   "\n", indent, "]")
          }
        },
        "logical, matrix" =,
        "integer, matrix" =,
        "double, matrix" = {
          if (length(arg) == 0)
            "[ ]"
          else
            paste0("[",
                   paste0(indent, "\t", lapply(1:nrow(arg), function(i) Translate.Arg(arg[i, ]))),
                   "\n", indent, "]")
        },
        "character, attribute" = paste0("#", backtick(arg)),
        "character, mapping" = {
          if (is.null(names(arg)))
            paste0("{", paste(backtick(arg), collapse = ", "), "}")
          else
            paste0("{", paste(backtick(names(arg)), "=", backtick(arg), collapse = ", "), "}")
        },
        "character, inputs" = {
          paste0("[", paste0(backtick(arg), collapse = ", "), "]")
        },
        Stop("Unsupported argument type found for: ", deparse(arg), "\n",
             "Type: ", typeof(arg), "\tClass: ", class(arg))
        )
  paste0(indent, prefix, body)
}

Translate.Join <- function(join) {
  paste(Translate(join$x), "\n",
        Translate(join$y), "\n",
        paste(join$alias, "="),
        "JOIN",
        paste0("\t", join$x$alias, " BY (",
               paste0(join$xSchema, collapse = ", "),
               ")", ","),
        paste0("\t", join$y$alias, " BY (",
               paste0(join$ySchema, collapse = ", "),
               ")", ";"),
        sep = "\n")
}

Translate.GLA <- function(gla) {
  paste(paste(lapply(gla$states, Translate), collapse = "\n"),
        Translate(gla$data),
        paste(gla$alias, "="),
        Translate(gla$gla),
        "FROM",
        paste0("\t", gla$data$alias),
        if (!is.null(gla$states)) "REQUIRES",
        paste("\t", lapply(gla$states, `[[`, "alias"), collapse = ",\n"),
        if (length(gla$inputs) > 0) "USING",
        Translate.Inputs(gla$inputs, gla$data),
        if (length(gla$schema) > 0) "AS",
        paste0("\t", Translate.Outputs(gla$schema), collapse = ",\n"),
        ";",
        sep = "\n")
}

Translate.GT <- function(gt) {
  paste(paste(lapply(gt$states, Translate), collapse = "\n"),
        Translate(gt$data),
        paste(gt$alias, "="),
        Translate(gt$gt),
        "FROM",
        paste0("\t", gt$data$alias),
        if (!is.null(gt$states)) "REQUIRES",
        paste("\t", lapply(gt$states, `[[`, "alias"), collapse = ",\n"),
        "USING",
        Translate.Inputs(gt$inputs, gt$data),
        "AS",
        paste0("\t", Translate.Outputs(gt$outputs), collapse = ",\n"),
        ";",
        sep = "\n")
}

Translate.Filter <- function(filter) {
  update.clustering(grokit$expressions[[filter$condition]], filter$data)
  paste(Translate(filter$data), "\n",
        paste(filter$alias, "="),
        "FILTER",
        paste0("\t", filter$data$alias),
        "BY",
        paste0("\t", Translate.Expr(grokit$expressions[[filter$condition]], filter$data)),
        ";",
        sep = "\n")
}

Translate.Generated <- function(generator) {
  paste(Translate(generator$data), "\n",
        paste(generator$alias, "="),
        paste("FOREACH", generator$data$alias, "GENERATE"),
        Translate.Inputs(generator$generated, generator$data),
        ";",
        sep = "\n")
}

Translate.Print <- function(data, inputs, type, result) {
  paste(if (exists("grokit.jobid")) paste0("JOBID ", grokit.jobid, ";"),
        paste0("USING ", grokit$libraries, ";", collapse = "\n"),
        Translate(data), "\n",
        paste("PRINT", data$alias, "USING"),
        paste0("\t", lapply(grokit$expressions[inputs], Translate.Expr, data), collapse = ",\n"),
        paste('AS', quotate(type), 'HEADER'),
        paste0("\t", quotate(names(inputs)), collapse = ",\n"),
        paste0('INTO "', result, '" SEPARATOR "|";\n'),
        sep = "\n")
}

Translate.Store <- function(store) {
  libraries <- paste0("USING ", grokit$libraries, ";", collapse = "\n")
  paste(libraries,
        Translate(x), "\n",
        paste0("STORE ", x$alias, " INTO ", relation, overwrite, ";"),
        sep = "\n")
}

Translate.Inputs <- function(inputs, data) {
  if (length(inputs) > 0)
    paste0("\t", backtick(inputs), " = ",
           lapply(grokit$expressions[inputs], Translate.Expr, data),
           collapse = ",\n")
  else
  ""
}

Translate.Outputs <- function(outputs) {
  ## checks that names are syntactically correct in C++, i.e. only underscores and alpha-numeric
  if (any(bad <- !is.identifier(outputs)))
    Stop("illegal attribute names: ", paste(outputs[bad], collapse = ", "))

  if (any(bad <- duplicated(outputs)))
    Stop("duplicated output names: ", paste(outputs[bad], collapse = ", "))
  backtick(outputs)
}

Translate.Expr <- function(expr, data) UseMethod("Translate.Expr")

Translate.Expr.Arguments <- function(expr, data, bracket = "(")
  quotate(paste(lapply(as.list(expr)[-1], Translate.Expr, data), collapse = ", "), bracket)

`Translate.Expr.{` <- `Translate.Expr.(` <- function(expr, data)
  Translate.Expr.Arguments(expr, data, as.character(expr[[1]]))

Translate.Expr.call <- function(expr, data) {
  if (is.operator(expr[[1]]))
    Translate.Expr.Operation(expr, data)
  else if (is.symbol(expr[[1]]))
    paste0(expr[[1]], Translate.Expr.Arguments(expr, data))
  else if ((is.call.to(expr[[1]], "::"))) ## R requires the 2 arguments to :: to be symbols
    paste0(expr[[1]][[2]], "::", expr[[1]][[3]], Translate.Expr.Arguments(expr, data))
  else if (is.method(expr[[1]]))
    Translate.Expr.Method(expr, data)
  else if (is.UDF(expr[[1]]))
    Translate.Expr.UDF(expr, data)
  else if (is.call(expr[[1]]))
    Stop("cannot have nested function calls, e.e. f()(): ", deparse(expr))
  else
    Stop("illegal call in expression: ", deparse(expr))
}

Translate.Expr.character <- function(expr, data) quotate(gsub('"', '\\\\"', expr))

Translate.Expr.Date <- function(expr, data) gsub(" 0", " ", format(expr, "DATE(%Y, %m, %d)"))

Translate.Expr.POSIXlt <- Translate.Expr.POSIXct <- function(expr, data) as.integer(expr)

Translate.Expr.default <- function(expr, data) expr

Translate.Expr.if <- function(expr, data) {
  if (length(expr) != 4)
    stop("inline if (ternary operator) must be given an else branch.")
  quotate(paste(Translate.Expr(expr[[2]], data),
                "?", Translate.Expr(expr[[3]], data),
                ":", Translate.Expr(expr[[4]], data)),
          "(")
}

Translate.Expr.Method <- function(expr, data) {
  if (!is.symbol(expr[[1]][[3]]))
    Stop("improper method name: ", deparse(expr))
  paste0("[", Translate.Expr(expr[[1]][[2]], data), "->", expr[[1]][[3]], Translate.Expr.Arguments(expr, data), "]")
}

Translate.Expr.name <- function(expr, data) {
  if (as.character(expr) %nin% names(data$schema))
    Stop(name, " is not an attribute of ", data$alias)

  att <- data$schema[[as.character(expr)]]
  paste(backtick(strsplit(att, ".", fixed = TRUE)[[1]]), collapse = ".")
}

Translate.Expr.Operation <- function(expr, data) {
  if (expr[[1]] == "%in%")
    paste0(Translate(UDF(Contains, values = as.character(eval(expr[[3]])))), "(", Translate.Expr(expr[[2]], data), ")")
  else if (length(expr) == 3)
    paste(Translate.Expr(expr[[2]], data),
          Translate.Expr.Operator(expr[[1]], data),
          Translate.Expr(expr[[3]], data))
  else
    paste(Translate.Expr.Operator(expr[[1]], data),
          Translate.Expr(expr[[2]], data))
}

Translate.Expr.Operator <- function(symbol, data) {
  map <- c("$" = "->", "%%" = "%", "%/%" = "//")
  illegal <- c("[[", "[", "@", "^", ":", "<-", "<<-", "=", "?")
  name <- as.character(symbol)
  if (name %in% illegal)
    Stop("illegal operator used in an expression: ", name)
  if (name %in% names(map))
    map[[name]]
  else
    name
}

Translate.Expr.UDF <- function(expr, data) {
  ## if statement because x[] is length 3 with an empty symbol on the end
  template <- MakeTemplate(UDF, expr[[1]][[2]],
                           if (length(expr[[1]]) > 2 && expr[[1]][[3]] != "") as.list(expr[[1]])[-(1:2)])
  paste0(Translate(template, level = 1), Translate.Expr.Arguments(expr, data))
}
