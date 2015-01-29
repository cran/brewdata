\name{ brewdata-package }
\alias{ brewdata-package }
\docType{package}
\title{ Package Brewdata }
\description{
Brewdata is a package built to lookup, parse, and analyze statistics and 
biostatistics graduate admissions data reported in the Grad Cafe Results Search. 
}
\details{
\tabular{ll}{
Package: \tab brewdata\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2015-01-29\cr
License: \tab GPL(>=2) \cr
}
brewdata() is the primary method of this package. It returns a data frame of 
normalized school names, admissions decisions, parsed undergraduate GPA, GRE 
scores, and the date of the admissions decision. 
}
\author{
Nathan Welch <nathan.welch@me.com>

}
\references{
Grad Cafe: http://forum.thegradcafe.com/, 
GRE score guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\keyword{ package }
\examples{
#Get data for fall 2015 PhD statistics admission decisions
one_yr_data = brewdata()
head( one_yr_data )

}