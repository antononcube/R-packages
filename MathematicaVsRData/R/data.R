#' Titanic survival
#'
#' This data set contains the survival status of 1309 passengers aboard
#' the maiden voyage of the RMS Titanic in 1912 (the ships crew are not included),
#' along with the passengers age, sex and class (which serves as a proxy for economic status).
#'
#' Each row represents one person.
#'
#' The columns describe different attributes about the person.
#' The first column "id" has simple ID's derived from ordering the data (in some way.)
#' The names of the rest of the columns are self-explanatory.
#' The column "passengerAge" has values "rounded" into age-groups, \code{seq(0,80,10)}.
#' If the passenger age is unknown/missing the value is -1.
#'
#' @format A data frame with 1309 rows and 5 columns.
#' \describe{
#'   \item{id}{Simple, order derived ID.}
#'   \item{passengerClass}{Passenger class.}
#'   \item{passengerAge}{Passenger age rounded into age groups.}
#'   \item{passengerSex}{Passenger sex.}
#'   \item{passengerSurvival}{Passenger survival.}
#' }
#' @source \url{https://datarepository.wolframcloud.com/resources/Sample-Data-Titanic-Survival}
#' @family MathematicaVsR data
"dfTitanic"


#' Mushroom edibility
#'
#' This data set consists of 8124 records of the physical
#' characteristics of gilled mushrooms in the Agaricus and Lepiota family,
#' along with their edibility.
#' Each row represents one mushroom.
#'
#' @format A data frame with 8124 rows and 24 columns.
#' @source \url{https://datahub.io/machine-learning/mushroom}
#' @family MathematicaVsR data
"dfMushroom"


#' Heteroscedastic distribution data
#'
#' The data is generated with the code:
#' \code{xs <- seq(-3, 3, 0.01)}
#' \code{dfDistributionData <- purrr::map_df( xs, function(x) { data.frame( X = x, Y = exp(-x^2) + rnorm( n = 1, mean = 0, sd =  0.15 * sqrt( abs( 1.5 - x) / 1.5 ) ) ) })}.
#'
#' @format A data frame with 601 rows and 2 columns.
#' \describe{
#'     \item{X}{x-coordinate.}
#'     \item{Y}{y-coordinate.}
#' }
#' @family MathematicaVsR data
"dfDistributionData"


#' Orlando temperature data
#'
#' The data is daily temperature in Orlando, Florida, USA
#' from 2015-01-01 to 2019-01-01.
#'
#' The data was obtained with the Mathematica code:
#' \code{WeatherData[{"Orlando", "USA"}, "Temperature", {{2015, 1, 1}, {2019, 1, 1}, "Day"}]}.
#'
#' @format A data frame with 1461 rows and 2 columns.
#' \describe{
#'     \item{Time}{Date as the number of seconds from 1900-01-01 00:00.}
#'     \item{Temperature}{Temperature in Celsius.}
#' }
#' @family MathematicaVsR data
"dfTemperatureData"


#' GE stock data
#'
#' GE stock data from 2014-01-01 to 2019-01-01.
#'
#' @format A data frame with 1258 rows and 2 columns.
#' \describe{
#'     \item{Time}{Date.}
#'     \item{Value}{Adjusted stock price in US dollars.}
#' }
#' @family MathematicaVsR data
"dfFinancialData"


#' THE TRAGEDY OF HAMLET, PRINCE OF DENMARK by William Shakespeare
#'
#' The text of Shakespeare's play Hamlet is split into parts that
#' correspond to the dialogue in the play.
#'
#' @format A character vector
#'
#' @source \url{http://erdani.com/tdpl/hamlet.txt}
#' @family MathematicaVsR data
"textHamlet"


#' USA presidents speeches
#'
#' USA presidents speeches together with related metadata.
#'
#' @format A data frame
#' \describe{
#'     \item{Date}{Date of the speech.}
#'     \item{Title}{Title of the speech.}
#'     \item{Name}{Name of the president that gave the speech.}
#'     \item{Text}{Text of the speech.}
#' }
#'
#' @source \url{http://www.gutenberg.org/ebooks/925}
#' @family MathematicaVsR data
"dfUSAPresidentsSpeeches"


#' S&P 500 stock daily closing values
#'
#' S&P 500 stock daily closing values retrieved with Mathematica's function \code{FinancialData}.
#'
#' @format A data frame
#' \describe{
#'     \item{StockID}{Stock identifier.}
#'     \item{AbsoluteTime}{Absolute time of the date in seconds from 1900-01-01.}
#'     \item{Value}{The value of the stock.}
#'     \item{Date}{The POSIXct date corresponding to \code{dfSP500$AbsoluteTime}.}
#' }
#'
#' @source \url{https://reference.wolfram.com/language/ref/FinancialData.html}
#'
#' @details For additional details see \url{https://en.wikipedia.org/wiki/S&P_500_Index} and
#' \url{https://en.wikipedia.org/wiki/List_of_S&P_500_companies}.
#'
#' @family MathematicaVsR data
"dfSP500"


#' Apple COVID-19 Mobility data
#'
#' Apple COVID-19 Mobility data sample with data for Nice, France only.
#'
#' @format A data frame
#' \describe{
#'     \item{GeoType}{Geo type}
#'     \item{Region}{Region name}
#'     \item{TransportationType}{One of "driving", "transit", "walking".}
#'     \item{AlternativeName}{Alternative name}
#'     \item{SubRegion}{Sub region name}
#'     \item{Country}{Country}
#'     \item{AbsoluteTime}{Absolute time of the date in seconds from 1900-01-01.}
#'     \item{Value}{The value of the stock.}
#'     \item{Date}{The POSIXct date corresponding to \code{dfAppleCOVID19MobilityData$AbsoluteTime}.}
#' }
#'
#' @source \url{https://covid19.apple.com/mobility}
#'
#' @details The data from 2020-01-13 to 2021-01-15.
#'
#' @family MathematicaVsR data
"dfAppleCOVID19MobilityData"

