## This file stands for data definition language.

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
Create <- function(relation, ...) {
  ischar <- tryCatch(is.character(relation) && length(relation) == 1,
                     error = identity)
  if (inherits(ischar, "error"))
    ischar <- FALSE
  if (!ischar)
    relation <- as.character(substitute(relation))
  assert(is.character(relation) && length(relation) == 1,
         "'relation' should be a name or a length-one character vector")
  assert(!is.relation(relation),
         "cannot overwrite relation: ", relation)

  types <- lapply(as.list(substitute(list(...)))[-1], convert.type)
  if (length(types) == 0)
    stop("schema cannot be empty")
  schema <- convert.names(types)
  if (any(schema == ""))
    stop("missing attribute names")

  piggy <- paste(Translate.Libraries(),
                 paste("CREATE RELATION", relation, "("),
                 paste("\t", Translate.Outputs(schema), ":", lapply(types, Translate.Template), collapse = ",\n"),
                 ");", "FLUSH;", "QUIT;\n", sep = "\n")
  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  RunQuery(piggy, pgy, err)
  ## TODO: This won't work because the schema isn't created immediately. Need to run another query first.
  grokit$schemas <- get.schema()
  invisible(NULL)
}

#' @rdname Create
Delete <- function(relation) {
  ischar <- tryCatch(is.character(relation) && length(relation) == 1,
                     error = identity)
  if (inherits(ischar, "error"))
    ischar <- FALSE
  if (!ischar)
    relation <- as.character(substitute(relation))
  assert(is.character(relation) && length(relation) == 1,
         "'relation' should be a name or a length-one character vector")
  assert(is.relation(relation),
         "invalid relation given: ", deparse(relation))

  piggy <- paste0("DELETE RELATION ", relation, ";\nFLUSH;\nQUIT;\n")
  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  RunQuery(piggy, pgy, err)
  ## TODO: This won't work because the schema isn't created immediately. Need to run another query first.
  grokit$schemas <- get.schema()
  invisible(NULL)
}

#' @rdname Create
Cluster <- function(relation, attribute) {
  ischar <- tryCatch(is.character(relation) && length(relation) == 1,
                     error = identity)
  if (inherits(ischar, "error"))
    ischar <- FALSE
  if (!ischar)
    relation <- as.character(substitute(relation))
  assert(is.character(relation) && length(relation) == 1,
         "'relation' should be a name or a length-one character vector")
  assert(is.relation(relation),
         "invalid relation given: ", deparse(relation))

  attribute <- substitute(attribute)
  assert(is.symbol(attribute) && is.identifier(as.character(attribute)),
         "invalid attribute given: ", deparse(attribute))
  assert(as.character(attribute) %in% get.attributes(relation),
         "unavailable attribute: ", attribute)

  piggy <- paste0("CLUSTER ", relation, " BY ", as.character(attribute), ";\n")
  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  RunQuery(piggy, pgy, err)
  grokit$schemas <- get.schema()
  invisible(NULL)
}

Clear <- function(relation) {
  ischar <- tryCatch(is.character(relation) && length(relation) == 1,
                     error = identity)
  if (inherits(ischar, "error"))
    ischar <- FALSE
  if (!ischar)
    relation <- as.character(substitute(relation))
  assert(is.character(relation) && length(relation) == 1,
         "'relation' should be a name or a length-one character vector")
  assert(is.relation(relation),
         "invalid relation given: ", deparse(relation))

  piggy <- paste0("DELETE CONTENT ", relation, ";\nFLUSH;\nQUIT;\n")
  file <- tempfile("Q")
  pgy <- paste0(file, ".pgy")
  err <- paste0(file, ".err")
  RunQuery(piggy, pgy, err)
  ## TODO: This won't work because the schema isn't created immediately. Need to run another query first.
  grokit$schemas <- get.schema()
  invisible(NULL)
}
