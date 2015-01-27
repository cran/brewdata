\name{translateScore}
\alias{translateScore}
\title{
Function to Convert Pre-2011 GRE Scores to the Current Scale
}
\description{
translateScore converts pre-2011 GRE scores to the current scale using the 
concordance tables 1D and 1E on pages 23-24 of the GRE score guide.
}
\usage{
translateScore(old_score, section)
}
\arguments{
  \item{old_score}{
	\code{old_score} pre-2011 quantitative or verbal section score (200-800)
}
  \item{section}{
	\code{section} "verbal" or "quant" variable indicating which table to use
}
}
\value{
translateScore returns a GRE score corresponding to the current scale 
(130-170) for the specified section, either quantitative or verbal.
}
\references{
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\seealso{
\code{\link{findScorePercentile}}, \code{\link{parseResults}}, 
\code{\link{parseSchools}}, \code{\link{brewdata}}
}
\examples{
translateScore( 710, "quant" )
translateScore( 710, "verbal" )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
