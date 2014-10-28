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

ReadCSV(file, skip = 0, attributes = AUTO, types = AUTO, sep = ",",
simple = FALSE, nrows = 100, ...)

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
    the desired column names for the data. See \link{attributes} for details.
  }
  \item{types}{
    the types of the columns to be read.
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

  If not \code{AUTO}, \code{types} should be a list of values with the
  desired types. For example, when reading in a file with 3 columns of
  doubles, \code{types = list(1.0, 1.0, 1.0)} is suitable.
}
\section{AUTO}{
  For \code{ReadCSV}, if \code{attributes = AUTO}, then names are
  constructed as in \code{read.csv}. Arguments passed via ellipsis are
  used by \code{read.csv} to read in a sample of the data. This
  sample provides the names used. More specifically, if \code{header =
  FALSE} is passed into \code{read.csv}, then the default names
  \code{VO}, \code{V1}, and so forth. Otherwise, the first line of the
  data will supply them.

  Additionally, if \code{types = AUTO}, the types of the sample will be
  used as \code{types}.
}
\value{
  An object of class \code{"\link{data}"} that represents the relevant
  data.
}
\author{
  Jon Claus at Tera Insights LLC
}
