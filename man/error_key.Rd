\name{error_key}
\alias{error_key}
\docType{data}
\title{
Common School Name Error Key
}
\description{
The error_key dataset includes several regular expressions used to parse 
school names. This dataset is used to parse commonly mis-spelled terms
(e.g. university). It also maps several specific department names into a 
standardized university name assigned to targets matching the search patterns. 
For example, any entry name that includes the term 'Booth' is mapped to the 
standardized name 'univ chicago'. This dataset supports brewdata's parseSchools 
helper function. 
}
\usage{data("error_key")}
\format{
  The format is:
 chr [1:37, 1:2] "boston|bu " "carnegie|mellon|cmu" "fort collins" ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:2] "regex" "corrected"
}
\examples{
data(error_key)
}
\keyword{datasets}
