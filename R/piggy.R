Translate <- function(x, ...) UseMethod("Translate")

Translate.NULL <- function(null) ""

Translate.default <- function(x) stop("cannot translate: ", x)

Translate.Template <- function(fun, level = 0) {
  if (!is.null(names(fun$args))) {
    prefixes <- quotate(names(fun$args))
    prefixes[prefixes != ""] <- paste(prefixes[prefixes != ""], "= ")
  } else {
    prefixes <- rep("", length(fun$args))
  }
  indent <- paste(rep("\t", level), collapse = "")
  paste0(fun$type, ":", backtick(fun$library), "\\", backtick(fun$name), "<",
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
           Translate.Expr.character(arg)
          else
            paste0("[", paste(lapply(arg, Translate.Expr.character), collapse = ", "), "]")
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
        "character, attribute" = paste0("#", paste(backtick(strsplit(arg, ".", fixed = TRUE)[[1]]), collapse = ".")),
        "character, mapping" = {
          if (is.null(names(arg)))
            paste0("{", paste(backtick(arg), collapse = ", "), "}")
          else
            paste0("{", paste(backtick(names(arg)), "=", backtick(arg), collapse = ", "), "}")
        },
        "character, inputs" = {
          paste0("[", paste0(backtick(arg), collapse = ", "), "]")
        },
        stop("Unsupported argument type found for: ", deparse(arg), "\n",
             "Type: ", typeof(arg), "\tClass: ", class(arg))
        )
  paste0(indent, prefix, body)
}

Translate.Libraries <- function() paste0("USING ", grokit$libraries, ";\n", collapse = "")

Translate.ID <- function() if (exists("grokit.jobid")) paste0("JOBID ", grokit.jobid, ";\n") else ""

Translate.Print <- function(data, inputs, type, result, sep = "|", limit = limit) {
  paste0("PRINT ", data$alias, " USING",
         paste0("\n\t", lapply(grokit$expressions[inputs], Translate.Expr, data), collapse = ","),
         "\nAS ", quotate(type), " HEADER",
         paste0("\n\t", quotate(names(inputs)), collapse = ","),
         "\nINTO ", quotate(result),
         "\nSEPARATOR ", quotate(sep),
         if (limit) "\nLIMIT 200000",
         ";\n")
}

Translate.Inputs <- function(inputs, data) {
  if (length(inputs) > 0)
    paste(backtick(inputs), "=", lapply(grokit$expressions[inputs], Translate.Expr, data))
  else
    ""
}

Translate.Outputs <- function(outputs) {
  if (any(bad <- !is.identifier(outputs)))
    stop("illegal attribute names: ", paste(outputs[bad], collapse = ", "))
  if (any(bad <- duplicated(outputs)))
    stop("duplicated output names: ", paste(outputs[bad], collapse = ", "))
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
  else if (is.call.to(expr[[1]], "::")) ## R requires the 2 arguments to :: to be symbols
    paste0(expr[[1]][[2]], "\\", expr[[1]][[3]], Translate.Expr.Arguments(expr, data))
  else if (is.method(expr[[1]]))
    Translate.Expr.Method(expr, data)
  else if (is.UDF(expr[[1]]))
    Translate.Expr.UDF(expr, data)
  else if (is.call(expr[[1]]))
    stop("cannot have nested function calls, e.e. f()(): ", deparse(expr))
  else
    stop("illegal call in expression: ", deparse(expr))
}

Translate.Expr.character <- function(expr, data) {
  map <- c("\"" = "\\\"", "\n" = "\\n", "\t" = "\\t", "\\" = "\\\\")
  quotate(paste(ifelse((chars <- unlist(strsplit(expr, ""))) %in% names(map), map[chars], chars), collapse = ""),
          empty = FALSE)
}

Translate.Expr.Date <- function(expr, data) gsub(" 0", " ", format(expr, "DATE(%Y, %m, %d)"))

Translate.Expr.POSIXlt <- Translate.Expr.POSIXct <- function(expr, data) as.integer(expr)

Translate.Expr.default <- function(expr, data) expr

Translate.Expr.if <- function(expr, data) {
  if (length(expr) != 4)
    stop("inline if (ternary operator) must be given an else branch.")
  quotate(paste(     Translate.Expr(expr[[2]], data),
                "?", Translate.Expr(expr[[3]], data),
                ":", Translate.Expr(expr[[4]], data)),
          "(")
}

Translate.Expr.Method <- function(expr, data) {
  if (!is.symbol(expr[[1]][[3]]))
    stop("improper method name: ", deparse(expr))
  paste0("[", Translate.Expr(expr[[1]][[2]], data), "->", expr[[1]][[3]], Translate.Expr.Arguments(expr, data), "]")
}

Translate.Expr.name <- function(expr, data) {
  if (as.character(expr) %nin% names(data$schema))
    stop(as.character(expr), " is not an attribute of ", data$alias)

  att <- data$schema[[as.character(expr)]]
  paste(backtick(strsplit(att, ".", fixed = TRUE)[[1]]), collapse = ".")
}

Translate.Expr.Operation <- function(expr, data) {
  if (expr[[1]] == "%in%")
    paste0(Translate(UDF(Contains, values = expr[[3]])),
           "(", Translate.Expr(expr[[2]], data), ")")
  else if (length(expr) == 3)
    paste(Translate.Expr(expr[[2]], data),
          Translate.Expr.Operator(expr[[1]], data),
          Translate.Expr(expr[[3]], data))
  else
    paste0(Translate.Expr.Operator(expr[[1]], data),
           Translate.Expr(expr[[2]], data))
}

Translate.Expr.Operator <- function(symbol, data) {
  map <- c("$" = "->", "%%" = "%", "%/%" = "//")
  illegal <- c("[[", "[", "@", "^", ":", "<-", "<<-", "=", "?")
  name <- as.character(symbol)
  if (name %in% illegal)
    stop("illegal operator used in an expression: ", name)
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
