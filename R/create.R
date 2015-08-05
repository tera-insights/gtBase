#' Create and Delete Relations.
#'
#' Functions used to create and delete relations.
#'
#' These functions are used to create new relations. These functions only affect
#' the relational schemas visible to the user; they have no actual effect on the
#' hard disk storage. In order to actually write data to an existing relation,
#' \code{\link{Store}} should be used. When \code{Delete} is called, that
#' relation is no longer available to the user and the space is used on the hard
#' disc is freed, but the stored data is not immediately overwritten.
#'
#' @param relation Usually, a name or character string specifying the relation
#'   to load. A character string (enclosed in explicit single or double quotes)
#'   is always taken as the relation name.
#'
#'   If the value of \code{relation} is a length-one character vector the name
#'   of the relation is taken to be the value of the only element. Otherwise,
#'   \code{relation} must be a name or character string.
#' @param ... Named arguments used to specify the schema of the relation being
#'   created. Each argument should be in the form of \code{name = type}, where
#'   \code{name} is a valid name for an attribute and type specifies a
#'   \code{\link{type}}.
#' @return An \code{\link{invisible}} NULL value.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
Create <- function(name, ...) {
  name <- substitute(name)
  if (!is.symbol(name) && is.identifier(as.character(name)))
    stop("invalid name given: ", deparse(name))
  if (as.character(name) %in% unlist(lapply(get.schema()$catalog, `[[`, "name")))
    stop("cannot overwrite relation: ", name)
  types <- lapply(as.list(substitute(list(...)))[-1], convert.type)
  if (length(types) == 0)
    stop("schema cannot be empty")
  schema <- names(types)
  if (is.null(schema) || any(schema == ""))
    stop("missing attribute names")

  piggy <- paste(paste("CREATE RELATION", name, "("),
                 paste("\t", Translate.Outputs(schema), ":", lapply(types, Translate.Template), collapse = ",\n"),
                 ");", "FLUSH;", "QUIT;\n", sep = "\n")
  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  run(piggy, pgy, err)
  ## TODO: This won't work because the schema isn't created immediately. Need to run another query first.
  grokit$schemas <- get.schema()
  invisible(NULL)
}

#' @rdname Create
Delete <- function(name) {
  name <- substitute(name)
  if (!is.symbol(name) && is.identifier(as.character(name)))
    stop("invalid name given: ", deparse(name))
  if (!as.character(name) %in% unlist(lapply(get.schema()$catalog, `[[`, "name")))
    stop("unavailable relation: ", name)
  piggy <- paste0("DELETE RELATION ", name, ";FLUSH;\nQUIT;\n")
  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  run(piggy, pgy, err)
  ## TODO: This won't work because the schema isn't created immediately. Need to run another query first.
  grokit$schemas <- get.schema()
  invisible(NULL)
}
