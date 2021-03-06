\name{Data}
\alias{Data}
\title{
  Abstract Data Objects
}
\description{
  Large scale data is represented abstractly in the GrokIt system. Rather
  than manipulating the data directly, the R interface sets up a
  description of the query using a waypoint system.
}
\details{
  Due to the scale of the data that the GrokIt system is intended to
  use, it is impossible to fully import the data into R because of
  memory limitations. Instead, the data is represented abstractly as an
  S3 object of class \code{"data"} built upon a list, with only the most
  relevant information kept.

  The only elements that every \code{"data"} object is guaranteed to
  contain are labeled \code{"schema"} and \code{"origin"}. The latter is
  used only for book-keeping and optimization and is not of much
  interest. The former is of much more import; it is used to ensure that
  the user only performs operations on attributes the data actually
  contains. As such, to find which attributes an object \code{x}
  contains, simply call \code{x$schema} to produce a character vector
  giving the names of the attributes.
}
\note{
  Although the above are the only two elements that every \code{"data"}
  object is guaranteed to have, there will always be additional
  elements. These are used to structure the overall query and relate it
  to the system; as such, the list structure grows rapidly and should be
  ignored by the average user.
}
\author{
  Jon Claus, <jonterainsights@gmail.com>, Tera Insights LLC
}
\examples{
## Proper specification of inputs and outputs
data <- Read(lineitem100g)
data$schema ## produces a character vector
str(data) ## fully details the structure for illustration's sake
}
