\name{getMaxPages}
\alias{getMaxPages}
\title{
Helper Function to Find the Maximum Number of Grad Cafe Focus Pages
}
\description{
getMaxPages retrieves the max number of data webpages available for processing.
Results Search data is served up to the getGradCafeData function one page at a
time. This function ensures that the search for results does not run forever. 
}
\usage{
getMaxPages( url )
}
\arguments{
  \item{url}{
	\code{url} is the web address for the Grad Cafe Results page. For example,
	if the desired search focus is 'statistics', then the url is 
	'http://www.thegradcafe.com/survey/index.php?q=statistics&t=a&pp=250&o=&p=2'.
}
}
\value{
getMaxPages returns the total number of pages of Grad Cafe Results. 
}
\references{
tbd...
}
\seealso{
\code{\link{findScorePercentile}}, \code{\link{parseResults}}, 
\code{\link{parseSchools}}, \code{\link{brewdata}}, 
\code{\link{getGradCafeData}}
}
\examples{
test = "http://www.thegradcafe.com/survey/index.php?q=statistics&t=a&pp=250&o=&p=8"
getMaxPages( test )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line