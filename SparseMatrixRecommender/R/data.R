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
#'   \item{id}{simple, order derived ID}
#'   \item{passengerClass}{passenger class}
#'   \item{passengerAge}{passenger age rounded into age groups}
#'   \item{passengerSex}{passenger sex}
#'   \item{passengerSurvival}{passenger survival}
#' }
#' @source \url{https://datarepository.wolframcloud.com/resources/Sample-Data-Titanic-Survival}
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
"dfMushroom"

