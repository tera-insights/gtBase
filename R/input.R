#' Read a text file.
#'
#' \code{Read} reads a dataset from a plain text file.
#'
#' This section deals with the specification of attribute names and types, which
#' is considerably more complicated that of \code{\link{read.table}}. The
#' description of \code{attributes} should be read before continuing.
#'
#' When given as a call, the specification is quoted and broken apart before
#' being processed on a per-element basis. Unlike \code{\link{read.table}},
#' column names and types can be specified for some columns and left blank for
#' others, in which case automatic processing takes over as it does in
#' \code{\link{read.table}}. In order to skip either a column name or type,
#' simply omit the corresponding label in the call, using a completely empty
#' argument when skipping both for a column.
#'
#' For example, if you want to omit the name of the second column, the type of
#' the third column, and both for the fourth column, \code{c(a = b, c, d=, )}
#' would be appropriate. In this example, the first column has name \dQuote{a}
#' and type \code{b}. The second column is given a generated name, such as
#' \dQuote{V2}. Third column has its type deduced based on the file. Both of
#' these occur for the fourth column.
#'
#' In the case that you want a single column without specifying either the name
#' or the type, simply use \code{c()}. Normally, converting this to a list
#' structure based on its AST results in no arguments. However, it is understood
#' that reading in a CSV with zero columns is nonsensical and so this special
#' functionality is used, as there is no other way to specify such a call.
#'
#' In truth, the function being called does not have to be \code{c}. A warning
#' is thrown but the exact function being called is otherwise ignored. This
#' allows for accidentally using \code{list} or similar mistakes.
#'
#' The default names for columns are the same as they are in \code{read.table},
#' If the file header is read, then the corresponding name is used. Otherwise,
#' the name is \dQuote{V} followed by the column index, starting at zero.
#'
#' Types are determined by an inner call to \code{\link{read.table}}. However,
#' strings are never assumed to be factors. See the \code{MoreArgs} argument
#' for more details.
#'
#' @param files A character vector of the files to be read. Paths can either be
#'   absolute or relative to the current working directory for the R session.
#'   Each file should be a csv with the same format. Only the first file is used
#'   to determine column names, types, etc.
#' @param attributes The specification of the columns being read from the files.
#'   This should either be the name of a relation or a call to \code{\link{c}}.
#'
#'   In the former case, the name is specified in the same format as it is for
#'   \code{\link{Load}}, meaning that it can be a name or character string.
#'   The inputs are taken to be the same as those of that relation, both names
#'   and types.
#'
#'   Otherwise, the attributes are specified as the arguments of the call to
#'   \code{\link{c}}. Each argument should be of the form \code{name = type},
#'   where \code{name} is the desired name for the corresponding column and
#'   \code{type} is a \code{\link[=types]{literal type object}}. The columns are
#'   specified in order and skipping is disallowed. See \sQuote{details} for
#'   more information.
#'
#'   However, in the case that the value of \code{attributes} is a length-one
#'   character vector, then both of these formats are superseded and the single
#'   element is taken to be the name of relation to use if and only if the name
#'   is a valid relation.
#'
#'   In the case that the file has more columns than are specified here, extra
#'   columns at the end are simply omitted.
#'
#'   This argument can be omitted, which is equivalent to reading in every
#'   column and inferring the name and type information.
#' @param header Whether the files contain the name of the columns on the first
#'   line. If so, only the header in the first file is used.
#' @param skip The number of lines to skip at the beginning of each document.
#' @param nrows The maximum number of lines to read. Negative and other invalid
#'   values are ignored.
#' @param sep The field delimiter, given as a length-one character vector. This
#'   single element should either be the word "TAB" or a single ASCII character,
#'   escape characters included. For example, "\\t", " ", and "\\" work. The
#'   only exception to this is "\\n" for obvious reasons.
#' @param simple Whether quotes are allowed. If not, then the fields are split
#'   whenever the delimiter is seen, regardless of whether it is inside a quoted
#'   string. Using a simple algorithm is significantly faster than not and is
#'   highly recommended whenever possible.
#' @param quote The character used to quote strings, given as a length-one
#'   character vector whose single element should be a single ASCII character.
#'   This is the character that is used to quote strings. Having different
#'   characters to begin and end a string, such as "(" and ")" is not supported.
#' @param escape The character used to escape the quote character, given as a
#'   length-one character vector whose single element should be a single ASCII
#'   character.
#' @param trim.cr Whether to check for and remove the carriage return (CR)
#'   characters. On Window machines, lines typically end with a carriage return
#'   before the line feed (LF) character, a.k.a. the newline character. Setting
#'   this as true ensures that CRs are not included in the last field. Only LF
#'   and CR+LF behaviour is currently supported. See
#'   \href{https://en.wikipedia.org/wiki/Newline}{here} for more information.
#' @param nullable An object used to specify the strings for each column that
#'   are to be interpreted as \sQuote{NULL} values, somewhat analogous to the
#'   \code{na.strings} arguments. However, it differs in that each attribute
#'   can have at most one null string and attributes can have different null
#'   strings, where a null string is a string that is interpreted as a
#'   \code{NULL} value.
#'
#'   The null string for each attribute can either be a length-one character,
#'   whose only element is taken to be the null string,
#'
#'   OR
#'
#'   a length-one logical. \code{TRUE} is the same as using the string
#'   \code{"NULL"} and \code{FALSE} denotes that the attribute is not nullable.
#'
#'   \code{nullable} can either be given as one of the two above formats, in
#'   which case the same value is used for every attribute, or as a list in
#'   order to use different null strings for different attributes.
#'
#'   If given as a list, the elements are interpreted in the following order:
#'
#'   1) If named, the name is taken to be the attribute. The value is
#'   interpreted as described above.
#'   2) If a list, the element labelled \sQuote{attr} should be a length-one
#'   character giving the attribute name. If an element labelled \sQuote{null}
#'   exists, it is interpreted as the null string; if not, then the null string
#'   is taken to be \code{"NULL"}.
#'   3) If a length-one character, then the single element is treated as the
#'   attribute name and the null string is taken to be \code{"NULL"}.
#'
#'   Any other format results in an error.
#'
#'   Currently only the (1) format of the list is supported.
#' @param MoreArgs A list of additional arguments to pass to an inner call to
#'   \code{\link{read.table}}. This call is used to read in a partial table,
#'   which is used for various checks as well as determining column names and
#'   types. Only the first file given in \code{files} is read. Each argument
#'   must be named, as to avoid confusion.
#'
#'   Any argument taken by both \code{ReadCSV} and \code{\link{read.table}} is
#'   disallowed from being passed in this manner. Instead, the value passed to
#'   this function is used instead. The exception to this is \code{nrows}. Only
#'   a single row is ever read by code{\link{read.table}}.
#'
#'   Furtheremore, arguments may be changed to fully mimic the behavior of the
#'   Grokit CSV Reader, such as to accomodate \code{simple = TRUE}.
#' @param chunk The chunk size, to be passed to \code{\link{Input}}.
#' @param check Should R read the file. If this is false, then no type or name
#'   inference is allowed. This is required for reading named pipes which can
#'   only be read once.
#'
#'   If the file is not read, then the column information cannot be inferred and
#'   must be given for each column.
#' @return A \code{\link{waypoint}} with the designated columns and rows.
#' @author Jon Claus, <jonterainsights@@gmail.com>, Tera Insights, LLC.
ReadCSV <- function(files, attributes, header = FALSE, skip = 0, nrows = -1,
                    sep = ",", simple = FALSE, quote = '"', escape = "\\",
                    trim.cr = FALSE, nullable = FALSE, MoreArgs = list(),
                    chunk = NULL, check = TRUE) {
  ## Various checks, warning, and processing for each argument.
  assert(is.character(files) && length(files) > 0,
         "files should be a character vector specifying the file path(s).")
  assert(all(present <- file_test("-f", files)),
         "missing files:\n", paste(files[!present], collapse = "\n"))
  files <- normalizePath(files)

  assert(is.logical(header) && length(header) == 1,
         "header should be a single boolean value.")

  assert(is.numeric(skip) && length(skip) == 1 && skip >= 0 && skip == floor(skip),
         "skip should be a single non-negative integer.")

  assert(is.numeric(nrows) && length(nrows) == 1 && nrows == floor(nrows),
         "nrows should be a single integer.")
  nrows <- as.integer(nrows)

  assert(is.character(sep) && length(sep) == 1 && nchar(sep) <= 1,
         "sep should be a single one-character string.")

  assert(is.logical(simple) && length(simple) == 1,
         "simple should be a single boolean value.")

  assert(is.character(quote) && length(quote) == 1 && nchar(quote) == 1,
         "quote should be a single one-character string.")

  assert(is.character(escape) && length(escape) == 1 && nchar(escape) == 1,
         "escape should be a single one-character string.")

  assert(is.logical(trim.cr) && length(trim.cr) == 1,
         "trim.cr should be a single boolean value.")

  ## Tuning parameters for simple algorithm.
  if (simple) {
    if (!missing(escape))
      warning("escape unnecessarily given for simple algorithm.")
    if (!missing(quote))
      warning("quote unnecessarily given for simple algorithm.")
    quote <- escape <- NULL
  }

  if (check) {
    ## The text is manually read in. Pre-processing is required to handle special
    ## escape characters, as scan only supports back-slash as the escape character.
    text <- readLines(files[[1]], header + skip + 1)
    line <- tail(text, 1)

    if (!simple && escape != "\\") {
      line <- gsub("\\", "\\\\", line)
      line <- gsub(escape, "\\", line)
    }

    ## Processing of arguments to read.table
    assert(is.list(MoreArgs) && is.named(MoreArgs),
           "MoreArgs must be a list with every element named.")

    inner.args <- names(formals(read.table))
    these.args <- names(formals(sys.function()))
    given.args <- names(MoreArgs)

    allowed <- subtract(inner.args, c(these.args, "file", "text"))
    illegal <- subtract(given.args, allowed)
    present <- intersect(allowed, given.args)

    if (length(illegal) > 0)
      warning("illegal arguments to read.table: ", paste(illegal, collapse = ", "))

    ## Args is the shared arguments between this function and read.table.
    Args <- mget(intersect(inner.args, these.args), sys.frame(sys.nframe()))
    MoreArgs <- c(MoreArgs[present], Args)
    MoreArgs$text <- paste0(c(text[as.integer(header)], line), "\n", collapse = "")
    MoreArgs$skip <- 0
    MoreArgs$nrows <- 1

    ## The names and types are gathered from the sample.
    sample <- as.list(eval(as.call(c(quote(read.table), MoreArgs))))
  } else {
    sample <- NULL
  }

  ## Processing of attributes. The isTRUE implicitly ensures that length = 1.
  ## First the value is checked for validity.
  is.relation <- tryCatch(is.character(attributes) && isTRUE(is.relation(attributes)),
                          error = identity)
  if (inherits(is.relation, "error"))
      is.relation <- FALSE

  ## If the value is not a relation name:
  if (!is.relation) {
    if (missing(attributes))
      if (check)
        attributes <- as.call(c(substitute(c), rep(list(substitute()), length(sample))))
      else
        stop("attributes must be given if the file is not checked.")
    else
      attributes <- substitute(attributes)

    ## A quoted name is simply converted to a character.
    if (is.symbol(attributes)) {
      attributes <- as.character(attributes)
    } else if (is.character(attributes)) {
      ## No processing needed here. Just avoids default case.
    } else {
      ## Default case is that the attributes are manually given as a call.
      assert(is.call(attributes), "illegal format for attributes.")
      warning.if(attributes[[1]] != quote(c), "attributes not a call to `c`.")

      ## The call is converted to a list structure with a special case for an empty call.
      if (length(attributes) == 1)
        attributes <- list(quote(expr = ))
      else
        attributes <- as.list(attributes)[-1]

      ## Checking if there are enough columns in the csv.
      if (check) {
        assert(length(attributes) <= length(sample),
               "more attributes specified than are present in given csv file.")

        ## Any given names are inserted into the previously constructed col.names
        if (!is.null(names(attributes)))
          col.names <- ifelse(names(attributes) == "", names(sample), names(attributes))
        else
          col.names <- head(names(sample), length(attributes))

        ## Column types are inferred from sample as necessary and then converted.
        ## substitute() is the empty symbol, meaning a missing attribute type.
        col.types <- ifelse(attributes == substitute(), sample, attributes)
      } else {
        ## No inference can be done as a sample was not read.
        assert(all(attributes != substitute() & convert.names(attributes) != ""),
               "attributes must be completely specified if 'check' is FALSE")
        col.types <- attributes
        col.names <- names(attributes)
      }
      col.types <- lapply(col.types, convert.type)
      attributes <- setNames(col.types, col.names)
    }
  }

  if (is.character(attributes)) {
    assert(isTRUE(is.relation(attributes)), "attributes does not name a relation.")
    col.names <- get.attributes(attributes)
    if (check)
      assert(length(col.names) <= length(sample),
             "more columns in ", attributes, " than are available in csv file given.")
  }

  ## Checking and processing of nullable.
  is.valid <- function(value)
    (is.character(value) || is.logical(value)) && length(value) == 1

  ## The function for checking the list format of nullable.
  ## TODO: Currently, named nullable arguments aren't supported for long names,
  ## as convert.args won't change list names.
  validate <- function(name, value)
    if (name != "") {
      assert(name %in% col.names && is.valid(value),
             "illegal nullable name-value pair: ", name, " ", value)
      value
    } else if (is.list(value)) {
      assert(!is.null(value$attr) && value$attr %in% col.names,
             "illegal nullable element: ", value)
      assert(is.null(value$null) || (is.character(value$null) && length(value$null) == 1),
             "illegal null value: ", value$null)
      list(attr = set.class(value$attr, "attribute"),
           null = if (is.null(value$null)) "NULL" else value$null)
    } else if (is.character(value)) {
      assert(length(value) == 1 && value %in% col.names,
             "illegal nullable element: ", value)
      set.class(value, "attribute")
    } else {
      stop("illegal format for nullable element: ", value)
    }

  assert(is.valid(nullable) || is.list(nullable))
  if (is.list(nullable))
      nullable <- mapply(validate, convert.names(nullable), nullable, SIMPLIFY = FALSE)
  ## convert.names in conjunction with mapply might cause all names to be empty but not null.
  if (all(names(nullable) == ""))
    names(nullable) <- NULL

  alias <- create.alias("read")

  ## gi <- GI(base::CSVReader,
  ##          skip = skip + header, n = nrows, sep = sep, simple = simple,
  ##          quote = quote, escape = escape, trim.cr = trim.cr, nullable = nullable)

  ## Conditional arguments are only done because the piggy parse can't handle "\\".
  ## TODO: Change this back to the above code once escape characters work.
  gi <- GI(base::CSVReader,
           skip = skip + header, n = nrows, sep = sep, simple = simple,
           trim.cr = trim.cr, nullable = nullable)
  if (!missing(quote))
    gi$args$quote <- quote
  if (!missing(escape))
    gi$args$escape <- escape

  Input(files, gi, attributes)
}

as.data <- function(data, types) {
  if (!is.data.frame(data))
    data <- as.data.frame(data)

  file <- tempfile("DF-", fileext = ".csv")
  write.table(data, file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
  class(file) <- c("file")

  if (missing(types))
    types <- as.list(data)
  else
    types <- as.list(substitute(types))[-1]
  types <- lapply(types, convert.type)

  attributes <- setNames(types, names(data))

  gi <- GI(base::CSVReader, skip = 0, simple = TRUE, sep = "tab")

  Input(file, gi, attributes)
}
