\name{findScorePercentile}
\alias{findScorePercentile}
\title{
Function Finding Percentile Rank from Scaled Scores
}
\description{
findScorePercent looks up the percent of test takers scorig lower than 
specified scores using tables 1A and 1B on page 22 of the GRE score guide. This
is a brewdata helper function that is available to end users, but not necessary
for them to run individually. 
}
\usage{
findScorePercentile( score, section)
}
\arguments{
  \item{score}{ \code{score} denotes the post-2011 scaled score (i.e. 130-170).
}
  \item{section}{  \code{section} denotes the GRE section to look up. Acceptable 
  inputs are 'verbal', 'quant', and 'writing'. Any other values will not return 
  a result.
}
}
\value{
findScorePercent returns the percentile score corresponding to one of 
the three GRE section (i.e. analytical writing, verbal, or quantitative) scaled 
scores (i.e. 0-6 and 130-170). 
}
\references{
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\seealso{
\code{\link{brewdata}}, \code{\link{parseResults}}, 
\code{\link{parseSchools}}, \code{\link{translateScore}}, 
\code{\link{getGradCafeData}}, \code{\link{getMaxPages}}
}
\examples{
#Quantitative percentile score
findScorePercentile( 160, "quant")

#Analytical Writing percentile score
findScorePercentile( 4.5, "writing" )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
