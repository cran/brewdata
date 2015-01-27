\name{verbal_conc_table}
\alias{verbal_conc_table}
\docType{data}
\title{
Table Used to Convert Pre-2011 Verbal Scores to Current Scaled Score
}
\description{
translateScore uses this dataset to convert pre-2011 GRE scores to the current 
scale using the concordance tables 1D and 1E on pages 23-24 of the GRE score 
guide.
}
\usage{data("verbal_conc_table")}
\format{
  A data frame with 61 observations on the following 3 variables.
  \describe{
    \item{\code{old}}{ Pre-2011 GRE scaled score, i.e. 200-800 }
    \item{\code{new}}{ Current GRE score scale, i.e. 130-170 }
    \item{\code{pct}}{ Percent of scores below the associated input }
  }
}
\source{
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\examples{
data(verbal_conc_table)

}
\keyword{datasets}
