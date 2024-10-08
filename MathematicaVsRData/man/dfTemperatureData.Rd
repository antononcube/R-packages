% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dfTemperatureData}
\alias{dfTemperatureData}
\title{Orlando temperature data}
\format{
A data frame with 1461 rows and 2 columns.
\describe{
    \item{Time}{Date as the number of seconds from 1900-01-01 00:00.}
    \item{Temperature}{Temperature in Celsius.}
}
}
\usage{
dfTemperatureData
}
\description{
The data is daily temperature in Orlando, Florida, USA
from 2015-01-01 to 2019-01-01.
}
\details{
The data was obtained with the Mathematica code:
\code{WeatherData[{"Orlando", "USA"}, "Temperature", {{2015, 1, 1}, {2019, 1, 1}, "Day"}]}.
}
\seealso{
Other MathematicaVsR data: 
\code{\link{dfAppleCOVID19MobilityData}},
\code{\link{dfDistributionData}},
\code{\link{dfFinancialData}},
\code{\link{dfMushroom}},
\code{\link{dfSP500}},
\code{\link{dfTitanic}},
\code{\link{dfUSAPresidentsSpeeches}},
\code{\link{textHamlet}}
}
\concept{MathematicaVsR data}
\keyword{datasets}
