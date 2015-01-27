\name{saw_score_table}
\alias{saw_score_table}
\docType{data}
\title{
Analytical Writing Score Section Table
}
\description{
findScorePercent uses the saw_score_table (Score/Analytical Writing Table) to 
look up the percent of test takers scorig lower than a specified scores using 
tables 1A and 1B on page 22 of the GRE score guide.
}
\usage{data("saw_score_table")}
\format{
  A data frame with 13 observations on the following 2 variables.
  \describe{
    \item{\code{score}}{ Current GRE score scale, i.e. 0-6 }
    \item{\code{aw}}{ Percent of scores below the associated input }
  }
}
\source{
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\examples{
data(saw_score_table)

}
\keyword{datasets}
