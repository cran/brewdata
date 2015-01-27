\name{parseResults}
\alias{parseResults}
\title{
Function to Convert "Decision and Date" String to Data
}
\description{
parseResults attempts to extract self-reported statistics listed from the 
Grad Cafe "Decision and Date" text field. The parsing rules below were generated 
based on test runs using the following parameters in the brewdata method:
	
	years=c(2015,2014), term="F", degree="phd", focus="statistics"

End-users should not need to interact directly with this function, but improving
the rules might improve the fidelity of the brewdata method results.
}
\usage{
parseResults( result )
}
\arguments{
  \item{result}{ \code{result} is a "Decision and Date" string from the Grad 
  Cafe. 
	}
}
\value{
parseResults returns a data frame with the following fields. 

\item{ gpa }{ is the self-reported grade point average. }
\item{ gre_v }{ is the self-reported GRE verbal section score }
\item{ gre_q }{ is the self-reported GRE quantitative section score } 
\item{ gre_aw }{ is the self-reported GRE analytical writing score }
\item{ month }{ is the month of the date that an admission decision was 
made--not the date an applicant uploaded the result to the Grad Cafe. 
}
\item{ day }{ is the day of the date that an admission decision was made--not 
the date an applicant uploaded the result to the Grad Cafe. 
}
\item{ year }{ is the day of the date that an admission decision was made--not 
the date an applicant uploaded the result to the Grad Cafe.
}
\item{ decision }{ denotes a university's decision on an application. Possible
decisions are accepted ('A'), wait listed ('W'), rejected ('R'), interview ('I')
or other ('O'). 
}

}
\seealso{
\code{\link{findScorePercentile}}, \code{\link{brewdata}}, 
\code{\link{parseSchools}}, \code{\link{translateScore}}
}
\examples{
#"Decision and Date" string parsing
x = "Accepted via E-mail on 16 Jan 2015 UG GPA: 4.00GRE General (V/Q/W): 167/170/4.00GRE "
parseResults(x)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
