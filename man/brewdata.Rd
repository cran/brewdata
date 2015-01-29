\name{brewdata}
\alias{brewdata}
\alias{brewdata()}
\title{ Function that Converts Grad Cafe Results Data into Usable Information }
\description{
The brewdata method queries the GradCafe Results Search page for application 
decision data. It then calls the parseResults function to breakdown the text 
from "Decision & Date" into values useful for exploring admissions decisions.
}
\usage{
brewdata(years = 2015, term = "F", degree = "phd", focus = "statistics", 
	resolution = 10, map=FALSE)
}
\arguments{
  \item{years}{
\code{years} specifies which years of data to include in the dataset. The year
specifies the time at which an applicant would start school not when he or she
applied. So, if someone applied for fall 2014 and was accepted in December 2013
and that person was also thoughtful enough to post his or her metrics on the
Grad Cafe's Results Search page, then that record would appear if 2014 is
part of the years searched. The four digit year (e.g. 2010, 2012, 1999, etc.) 
is the only acceptable date format. Inputs may be a single value or a list such
as 2010:2015 or c(2011,2013,2015). The default is 2015.
}
  \item{term}{
\code{term} indicates which term an applicant would begin graduate study. There
are only two acceptable values for this parameter: 'F' and 'S'. 'F' narrows the
search to fall matriculations only. 'S' narrows the search to include only the
spring term. Users may choose only one value. The default is 'F'. 
}
  \item{degree}{
\code{degree} determines whether results should be for masters or phd programs.
Users must specify exactly one and enclose the value in quotes. 'masters' or 
'phd' are the only acceptable values for this field. The default is 'phd'. 
}
  \item{focus}{
\code{focus} specifies the program. Any term that returns results on the 
Grad Cafe is acceptable, but brewdata was tuned using 'statistics' or 
'biostatistics'. School name mappings could be quite poor for any value 
other than these two.  If you choose a value besides 'statistics' or 'biostatistics',
it is strongly suggested to check the school name mapping by setting map=TRUE.
The default value is 'statistics'.
}
  \item{resolution}{
\code{resolution} is related to the school name parsing algorithm. This
variable controls the precision required before an original name is replaced 
with the best standardized equivalent. Therefore, very low values (between 0-5)
are cautious selections leading to fewer mis-matches, but more sparse results. 
Medium range values (8-12) lead to surprisingly accurate replacements when the
mother processing stages fail. One might expect a few mis-matched name 
replacements, but the number of errors should be fairly low. Finally, large 
values (more than 20) practically guarantee that a school name which is not in 
our standard dictionary will be replaced with something. Be weary of such large
selections; the potential for many mis-matched replacements is high. For the
test set, the bulk of the nearest matchs were within 10 units of the original 
value. Almost none were larger than 30. The default value is 10. 
}
  \item{map}{
\code{map} is a variable controlling whether or not the original school names
are included in the data frame returned by brewdata(). If map=TRUE, then the 
returned data includes the parsed names as well as the original. The default 
value is map=FALSE.
}
}
\value{

brewdata returns a data frame of the parsed Grade Cafe Results data. The data 
frame includes the following attributes: 

\item{ school_name }{ 
is the closest standardized name matching the name entered at the Grad Cafe. 
brewdata normalizes the names reported on the website to enable aggregate
analysis. See the parse_names parameter description above for more details on 
the parsing methods.
}
\item{ original_name }{ 
is the original name of the university reported to the Grad Cafe. If map=TRUE, 
then brewdata includes a column showing the names reported on the website 
alongside the normalized names assigned by brewdata. This column is excluded by
default. 
}
\item{ decision }{ denotes a university's decision on an application. Possible
decisions are accepted ('A'), wait listed ('W'), rejected ('R'), interview ('I')
or other ('O'). 
}
\item{ status }{ denotes an applicant's immigration status. This field is
reported directly from the Grad Cafe. Per the website definitions, possible
status values are American ('A'), International with a US degree ('U'), 
International without US degree ('I'), other ('O'), or unknown ('?').
}
\item{ gpa }{ is the self-reported grade point average. }
\item{ gre_v }{ is the self-reported GRE verbal section score }
\item{ gre_q }{ is the self-reported GRE quantitative section score } 
\item{ gre_aw }{ is the self-reported GRE analytical writing score }
\item{ v_pct }{ is the percent of verbal section scores below an applicant's 
self-reported score. Weighted numeric scores are converted to percentile scores 
using tables 1A and 1B on page 22 of the GRE score guide. 
Source: https://www.ets.org/s/gre/pdf/gre_guide.pdf. 
}
\item{ q_pct }{ is the percent of quantitative section scores below an 
applicant's self-reported score. Weighted numeric scores are converted to
percentile scores using tables 1A and 1B on page 22 of the GRE score guide. 
Source: https://www.ets.org/s/gre/pdf/gre_guide.pdf. 
}
\item{ aw_pct }{ is the percent of analytical writing section scores below an 
applicant's self-reported score. Weighted numeric scores are converted to
percentile scores using tables 1A and 1B on page 22 of the GRE score guide. 
Source: https://www.ets.org/s/gre/pdf/gre_guide.pdf. 
}
\item{ month }{ is the month of the date that an admission decision was 
made--not the date an applicant uploaded the result to the Grad Cafe. 
}
\item{ day }{ is the day of the date that an admission decision was made--not 
the date an applicant uploaded the result to the Grad Cafe. 
}
\item{ year }{ is the day of the date that an admission decision was made--not 
the date an applicant uploaded the result to the Grad Cafe.
}
	
}
\references{
Grad Cafe: http://www.thegradcafe.com
GRE Score Guide: https://www.ets.org/s/gre/pdf/gre_guide.pdf
}
\author{
Nathan Welch <nathan.welch@me.com>

}
\note{
Several specialty university departments are mapped to their parent 
institutions. For example, Booth, Wharton, and Teachers College are mapped to 
the University of Chicago, University of Pennsylvania, and Columbia University, 
respectively. If you are interested in results for such schools, set map=TRUE 
and use grep() on the original_name column to locate rows of data with the 
desired department. See below for an example.
}

\seealso{
\code{\link{findScorePercentile}}, \code{\link{parseResults}}, 
\code{\link{parseSchools}}, \code{\link{translateScore}},
\code{\link{getGradCafeData}}, \code{\link{getMaxPages}}
}
\examples{
#Get data for fall 2015 PhD statistics admission decisions
one_yr_data = brewdata( years=2014 )
head( one_yr_data )

### Remaining examples commented out to satisfy CRAN policies ###
#Get several years of data
#yrs=2014:2015
#multi_yr_data = brewdata( years=yrs ); head( multi_yr_data )
#results_by_school = split(multi_yr_data[,-1],multi_yr_data$school_name)

#Find 2014 results for Chicago Booth
#f14 = brewdata( years=2014, map=TRUE )
#booth = f14[ grepl( "booth", tolower( f14$original_name ) ), ]
#booth

#Continuing with the f15 & school data, let's analyze results from a particular
#school, e.g. University of Washington 
#uw = f15_by_school$'univ washington'; uw	#show all UW decisions
#uw_stats = uw[ uw$gre_v!=0 & uw$gre_q!=0, ]	#UW decisions with GRE stats
#plot( uw_stats$gpa, uw_stats$gre_q, xlab="Undergrad GPA", ylab="GRE Quant Score",
#	main="University of Washington GPA vs GRE Quant", pch=NA  )
#col_key = c('darkgreen','gold','red','black','darkgrey')
#lab = factor( uw_stats$decision, levels=c('A','W','R','I','N') )
#text( uw_stats$gpa, uw_stats$gre_q, label=lab, col=col_key[lab], cex=0.85 )


#Plot the last two years of Berkeley's GPA/GRE Quant decision trends
#yrs=2013:2014
#data = brewdata( years=yrs ); head( data )
#berk = split(data[,-1],data$school_name)$'univ california berkeley'
#berk_stats = berk[ berk$gre_v!=0 & berk$gre_q!=0, ]
#plot( berk_stats$gpa, berk_stats$gre_q, xlab="Undergrad GPA", ylab="GRE Quant Score",
#	main="Berkeley GPA vs GRE Quant Fall 2010-2015", pch=NA  )
#col_key = c('darkgreen','gold','red','black','darkgrey')
#lab = factor( berk_stats$decision, levels=c('A','W','R','I','N') )
#points( jitter( berk_stats$gpa ), jitter( berk_stats$gre_q ), 
#	col=col_key[lab], pch=20)
#lgd=c("Accepted", "Wait listed", "Rejected", "Interview", "Not Reported" )
#legend( "bottomleft", legend=lgd, col=col_key, pch=20, bty="n", cex=0.75 )


#Plot several years of results of Duke results using the same data from the 
#Berkeley download.
#library( scatterplot3d )
#library( rgl )
#duke = split(data[,-1],data$school_name)$'duke univ'
#duke_stats = duke[ duke$gre_v!=0 & duke$gre_q!=0, ]
#col_key = c('darkgreen','gold','red','black','darkgrey')
#lab = factor( duke_stats$decision, levels=c('A','W','R','I','N') )
#scatterplot3d( duke_stats$gpa, duke_stats$gre_q, duke_stats$gre_v,
#	xlab="Undergrad GPA", ylab="GRE Quant Score", zlab="GRE Verbal Score",
#	main="Duke GPA vs GRE Quant vs GRE Verbal Fall 2010-2015", pch=20,
#	color=col_key[lab]  )
#plot3d( duke_stats$gpa, duke_stats$gre_q, duke_stats$gre_v,
#	xlab="Undergrad GPA", ylab="GRE Quant Score", zlab="GRE Verbal Score",
#	main="Duke GPA vs GRE Quant vs GRE Verbal Fall 2010-2015", pch=20,
#	col=col_key[lab]  )
	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
