\name{svq_score_table}
\alias{svq_score_table}
\docType{data}
\title{
Verbal-Quantitative Reasoning Section Score Table
}
\description{
findScorePercent uses the svq_score_table (Verbal-Quantitative Section Score 
Table) to look up the percent of test takers scorig lower than specified scores 
using tables 1A and 1B on page 22 of the GRE score guide.
}
\usage{data("svq_score_table")}
\format{
  A data frame with 41 observations on the following 3 variables.
  \describe{
    \item{\code{score}}{ Post-2011 GRE scaled score, i.e. 130-170 }
    \item{\code{v}}{ Verbal score percentile ranking  }
    \item{\code{q}}{ Quantitative score percentile ranking  }
  }
}
\source{
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\examples{
data(svq_score_table)

}
\keyword{datasets}
