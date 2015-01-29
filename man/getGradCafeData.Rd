\name{getGradCafeData}
\alias{getGradCafeData}
\title{
Function that Downloads Grad Cafe Results Data
}
\description{
getGradCafeData retrieves data from the Grad Cafe Results Search pages. This is
an internal helper function, so end-users should not need to interact with it
directly. 
}
\usage{
getGradCafeData( years, term, degree, focus )
}
\arguments{
  \item{term}{
	\code{term} indicates which term an applicant would begin graduate study. There
are only two acceptable values for this parameter: 'F' and 'S'. 'F' narrows the
search to fall matriculations only. 'S' narrows the search to include only the
spring term. Users may choose only one value. The default is 'F'. 
}
  \item{years}{
	\code{years} specifies which years of data to include in the dataset. The year
specifies the time at which an applicant would start school not when he or she
applied. So, if someone applied for fall 2014 and was accepted in December 2013
and that person was also thoughtful enough to post his or her metrics on the
Grad Cafe's Results Search page, then that record would appear if 2014 is
part of the years searched. The two digit year (e.g. "10", "12", "06", etc.) 
is the only acceptable date format. Inputs may be a single value or a list such
as c("08","13","15"). 
}
  \item{degree}{
	\code{degree} determines whether results should be for masters or phd programs.
Users must specify exactly one and enclose the value in quotes. 'masters' or 
'phd' are the only acceptable values for this field. The default is 'phd'. 
}
  \item{focus}{
	\code{focus} specifies the program. Any term that returns results on the 
Grad Cafe is acceptable, but brewdata was tuned using 'statistics' or 
'biostatistics'. University name mappings could be quite poor for any value 
other than these two. The default value is 'statistics'. 
}
}
\value{
getGradCafeData returns a data frame of loosely parsed results that satisfy the
search parameters. 
}
\references{
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\seealso{
\code{\link{findScorePercentile}}, \code{\link{parseResults}}, 
\code{\link{parseSchools}}, \code{\link{brewdata}}, \code{\link{getMaxPages}}
}
\examples{
#getGradCafeData( years=15, term="F", degree="phd", focus="Statistics" )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line