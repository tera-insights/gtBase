\name{Input}
\alias{Read}
\alias{ReadCSV}
\alias{ReadFile}
\alias{ReadRelation}
\title{
Data Input
}
\description{
  Reads a file or relation from the disk and creates an object of class
  \code{"\link{data}"}.
}
\usage{
Read(relation)

ReadCSV(file, attributes, skip = 0, sep = ",", simple = FALSE, nrows = 10000, ...)

ReadFile(file, gi, attributes, ...)

ReadRelation(file, relation, sep = ",", simple = FALSE)
}
\arguments{
  \item{relation}{
    For \code{Read}: the relation to read.

    For \code{ReadRelation}: the relation whose schema is used.
  }
  \item{file}{
    the path of the file to read expressed as a character.
  }
  \item{skip}{
    as the argument of the same name in \code{\link{read.csv}}. Should
    be adjusted accordingly to account for a header line.
  }
  \item{attributes}{
    the desired column names for the data. See \sQuote{Details} for more
    information.
  }
  \item{sep}{
    as the argument of the same name in \code{\link{read.csv}}
  }
  \item{simple}{
    should the simple algorithm be used.
  }
  \item{nrows}{
    the number of rows used by \code{read.csv} to get a sample.
  }
  \item{\dots}{
    For \code{ReadCSV}: additional arguments to \code{\link{read.csv}}.

    For \code{ReadFile}: additional arguments to \code{\link{read.table}}.
  }
  \item{gi}{
    a templated GI function.
  }
}
\details{
  All four functions construct an object of class \code{"\link{data}"}
  in different fashions.

  \itemize{
    \item \code{Read} uses both the data and the names of the specified
    relation, whereas the rest read from a file.

    \item \code{ReadCSV} behaves similarly to
    \code{\link{read.csv}}. The data in the specified file is read in
    using character separated values, with EOL characters separating
    rows. The names of the columns are detailed in the section
    \sQuote{AUTO}.

    \item \code{ReadFile} is a generalized version of \code{ReadCSV} and
    reads a file using an arbitrary generalized input
    function. Currently deprecated as there are none other than
    \code{ReadCSV}.

    \item \code{ReadRelation} is a specific case of \code{ReadCSV}. The
    file provides the data but the names of the relation are used. The
    data is read in as \code{ReadCSV} with \code{skip = 0} and \code{sep
    = ", "}.
  }

  \code{attributes} should be specified as a call to \code{c} in which
  each argument is either a symbol specifying the name of an attribute
  or a \code{name = type} pair. In the latter case, the name of the
  argument specifies the name of the attribute and \code{type} specifies
  the type of that attribute. \code{type} can either be specified as a
  symbol or a call, in which the name of the call is the type to be used
  and the arguments will be used to template the type.
}
\value{
  An object of class \code{"\link{data}"} that represents the relevant
  data.
}
\author{
  Jon Claus at Tera Insights LLC
}
