## funcition to ask user for various types of values through the web interface
ask <- function(...) {
  queries = as.list(substitute(list(...))[-1])
  if (is.null(names(queries)))
    lapply(queries, function(query) {
      if(as.character(query[[1]]) != "container")
        stop("No names given for ask element: ", query)
    })
  else {
    for (i in 1:length(queries))
      if(names(queries)[[i]] == "")
        if (as.character(queries[[i]][[1]]) == "container")
          names(queries)[[i]] = as.character(i)
        else
          stop(queries[[i]], " was not given a name.")
  }
  names <- names(queries)
  queries <- lapply(queries, convertQuery)
  names(queries) <- names
  json.output <- toJSON(queries)
  print(json.output)
  file <- tempfile("ASK-", fileext = ".json")
  sink(file)
  cat(json.output)
  sink()
  system2("grokit-cli", args = c("ask", grokit.jobid, file))
  result <- fromJSON(file = file)
  if (result$success)
      return(result$msg)
  else
      stop("User did not provide required input");
}

validParameters <- list(
    "file" = c("desc"),
    "integer" = c("desc", "min", "max"),
    "double" = c("desc", "min", "max"),
    "select" = c("desc", "values"),
    "text" = c("desc"),
    "textArea" = c("desc", "rows")
    )

checkParameters <- function(type, given) {
  expected <- validParameters[[type]]
  lapply(given, function(parameter) {
    if(!(parameter %in% expected))
      warning("For type ", type, ", parameter ", parameter, " not expected.")
  })
}

is.integer2 <- function(i) {
  if (!is.numeric(i))
    FALSE
  else
    i - as.integer(i) == 0
}

typeCheck <- function(x, field, type) {
  if (is.null(x[[field]]))
    TRUE
  else
    eval(as.call(c(as.list(call(paste0("is.", as.character(substitute(type))))), x[[field]])))
}

convertQuery <- function(query) {
  if (!is.language(query))
    stop("convertQuery called incorrectly.")
  if (length(query[[1]]) != 1 || !is.symbol(query[[1]]))
    stop("Item asked for has unexpected structure: ", query)
  type <- as.character(query[[1]])
  if (type == "container") {
    if (is.null(names(query[-1])))
      lapply(query[-1], function(subquery) {
        if(as.character(subquery[[1]]) != "container")
          stop("Name not supplied for: ", subquery)
      })
    else {
      for (i in 1:length(query[-1]))
        if (names(query[-1])[[i]] == "")
          if (as.character(query[-1][[i]][[1]]) == "container")
            names(query)[[i]] = as.character(i)
          else
            stop(query[-1][[i]], " was not given a name.")
    }
    names = names(query[-1])
    result = list("__type__" = "container", "__content__" = lapply(query[-1], convertQuery))
    names(result[["__content__"]]) = names
    return(result)
  } else {
    if (!(type %in% names(validParameters)))
      stop("Type ", type, " not supported.")
    checkParameters(type, names(query[-1]))
    result <- list("__type__" = type)
    options <- lapply(validParameters[[type]], function(parameter) {
      if (!is.null(query[[parameter]])) {
        result[[parameter]] <<- eval(query[[parameter]], parent.frame(3))
      }
    })
    ## additional checks for each query type go here
    switch(type,
           "file" = {
             if (!is.null(result[["desc"]]) && !is.character(result[["desc"]]))
               stop("The field 'desc' must be a string for type 'file'.")
           },
           "integer" = {
             if (!is.null(result[["desc"]]) && !is.character(result[["desc"]]))
               stop("The field 'desc' must be a string for type 'file'.")
             if (!is.null(result[["min"]]) && !is.integer2(result[["min"]]))
               stop("The field 'min' must be an integer for type 'file'.")
             if (!is.null(result[["max"]]) && !is.integer2(result[["max"]]))
               stop("The field 'max' must be a string for type 'file'.")
           })
    return(result)
  }
}
