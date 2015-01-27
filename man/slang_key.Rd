\name{slang_key}
\alias{slang_key}
\docType{data}
\title{
Table of Common or Shortened University Names
}
\description{
parseSchools uses this table to try matching a nickname to the formal university
or department name. This table corresponds to the second stage of processing.
}
\usage{data("slang_key")}
\format{
  The format is:
 chr [1:384, 1:2] "a&m" "alberta" "asu" "au" "baylor" "bc" ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:2] "slang" "name"
}
\examples{
data(slang_key)

}
\keyword{datasets}
