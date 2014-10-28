## Used to create relation. Can be used without assignment.
Create <- function(name, ...) {
  name <- substitute(name)
  if (!is.symbol(name) && is.identifier(as.character(name)))
    Stop("invalid name given: ", deparse(name))
  if (as.character(name) %in% unlist(lapply(get.schema()$catalog, `[[`, "name")))
    Stop("cannot overwrite relation: ", name)
  types <- convert.types(substitute(c(...)))
  if (length(types) == 0)
    Stop("schema cannot be empty")
  schema <- names(types)
  if (is.null(schema) || any(schema == ""))
    Stop("missing attribute names")
  if (any(bad <- !is.identifier(schema)))
    Stop("invalid attribute names: ", paste(schema[bad], collapse = ", "))

  piggy <- paste(paste("CREATE RELATION", name, "("),
                 paste("\t", schema, ":", lapply(types, Translate.Template), collapse = ",\n"),
                 ");", "FLUSH;", "QUIT;\n", sep = "\n")
  file <- tempfile("Q", getwd(), ".")
  pgy <- paste0(file, "pgy")
  err <- paste0(file, "err")
  run(piggy, pgy, err)
}

Delete <- function(name) {
  name <- substitute(name)
  if (!is.symbol(name) && is.identifier(as.character(name)))
    Stop("invalid name given: ", deparse(name))
  if (!as.character(name) %in% unlist(lapply(get.schema()$catalog, `[[`, "name")))
    Stop("unavailable relation: ", name)
  piggy <- paste0("DELETE RELATION ", name, ";FLUSH;\nQUIT;\n")
  file <- tempfile("Q", getwd(), ".")
  pgy <- paste0(file, "pgy")
  err <- paste0(file, "err")
  run(piggy, pgy, err)
}
