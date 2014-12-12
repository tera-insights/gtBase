## Used to create relation. Can be used without assignment.
Create <- function(name, ...) {
  name <- substitute(name)
  if (!is.symbol(name) && is.identifier(as.character(name)))
    stop("invalid name given: ", deparse(name))
  if (as.character(name) %in% unlist(lapply(get.schema()$catalog, `[[`, "name")))
    stop("cannot overwrite relation: ", name)
  types <- convert.types(substitute(c(...)))
  if (length(types) == 0)
    stop("schema cannot be empty")
  schema <- names(types)
  if (is.null(schema) || any(schema == ""))
    stop("missing attribute names")

  piggy <- paste(paste("CREATE RELATION", name, "("),
                 paste("\t", Translate.Outputs(schema), ":", lapply(types, Translate.Template), collapse = ",\n"),
                 ");", "FLUSH;", "QUIT;\n", sep = "\n")
  file <- tempfile("Q", getwd(), ".")
  pgy <- paste0(file, "pgy")
  err <- paste0(file, "err")
  run(piggy, pgy, err)
  ## TODO: This won't work because the schema isn't created immediately. Need to run another query first.
  grokit$schemas <- get.schema()
}

Delete <- function(name) {
  name <- substitute(name)
  if (!is.symbol(name) && is.identifier(as.character(name)))
    stop("invalid name given: ", deparse(name))
  if (!as.character(name) %in% unlist(lapply(get.schema()$catalog, `[[`, "name")))
    stop("unavailable relation: ", name)
  piggy <- paste0("DELETE RELATION ", name, ";FLUSH;\nQUIT;\n")
  file <- tempfile("Q", getwd(), ".")
  pgy <- paste0(file, "pgy")
  err <- paste0(file, "err")
  run(piggy, pgy, err)
  grokit$schemas <- get.schema()
}
