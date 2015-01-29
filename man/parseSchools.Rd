\name{parseSchools}
\alias{parseSchools}
\title{
Function to Match Error Prone Free-text to Standard School Names
}
\description{
parseSchools finds best matching school name among several possible spellings & 
abbreviations. Matches are based on a three stages of parsing:
stage (1) standardizes the text by removing common typos and spelling errors,
stage (2) manually searches for common name variations for the same school,
stage (3) uses an automated text processing algorithm to match the closest
school name from a standardized list. 
}
\usage{
parseSchools( original_name, resolution = 10, map=FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{original_name}{ \code{original_name} denotes an Nx1 vector of 
  university names read from the Grad Cafe. 
}
  \item{resolution}{
\code{resolution} controls the precision required before an original name is 
replaced  with the best standardized equivalent. Therefore, very low values 
(between 0-5) are cautious selections leading to fewer mis-matches, but more 
sparse results. Medium range values (8-12) lead to surprisingly accurate 
replacements when the mother processing stages fail. One might expect a few 
mis-matched name replacements, but the number of errors should be fairly low. 
Finally, large values (more than 20) practically guarantee that a school name 
which is not in our standard dictionary will be replaced with something. Be 
weary of such large selections; the potential for many mis-matched replacements 
is high. For the test set, the bulk of the nearest matchs were within 10 units 
of the original value. Almost none were larger than 30. The default value is 10. 
}
  \item{map}{
\code{map} is a variable controlling whether or not the original school names
are included in the data frame returned by brewdata(). If map=TRUE, then the 
returned data includes the parsed names as well as the original. The default 
value is map=FALSE.
}
}
\value{
\item{ school_name }{
is the name of the university corresponding to the row of data. parseSchools 
normalizes the names reported on the website. 
}
}
\seealso{
\code{\link{findScorePercentile}}, \code{\link{parseResults}}, 
\code{\link{parseSchools}}, \code{\link{translateScore}},
\code{\link{getGradCafeData}}, \code{\link{getMaxPages}}
}
\examples{
x = c( "university of california--berkeley","university of california--berkly", 
	"uc berkeley", "berkeley" )
parseSchools( x )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
