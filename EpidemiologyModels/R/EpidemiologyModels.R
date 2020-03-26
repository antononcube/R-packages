##===========================================================
## Epidemiology Models in R
## Copyright (C) 2020  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
## Written by Anton Antonov,
## antononcube @@@ gmail ... com,
## Windermere, Florida, USA.
##===========================================================



##===========================================================
## SIRModel
##===========================================================

#' SIR model
#' @description SIRModel
#' @param initialConditionsQ Should initial conditions be given?
#' @param rateRulesQ Should rate rules be given?
#' @param A list
#' @export
SIRModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "IPt" = "Infected Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity"
        ),

      Rates =
        c(
          "deathRateTP" = "Population death rate",
          "deathRateIP" = "Infected Population death rate",
          "contactRateIP" = "Contact rate for the infected population",
          "aip" = "Average infectious period",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ) ,
               {

                 dSPt <- - contactRateIP * SPt * IPt / TP0 - deathRateTP * SPt

                 dIPt <-   contactRateIP * SPt * IPt / TP0 - IPt / aip - deathRateIP * IPt

                 dRPt <-   IPt / aip - deathRateTP * RPt

                 return( list( c( dSPt, dIPt, dRPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c( SPt = 10^5 - 100, IPt = 100, RPt = 0 )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c( TP0 = 10^5, deathRateTP = 0.0000219178, deathRateIP = 0.035 / 28, aip = 28, contactRateIP = 0.56)))
  }

  res
}


##===========================================================
## SI2RModel
##===========================================================

#' SI2R model
#' @description SI2RModel
#' @param initialConditionsQ Should initial conditions be given?
#' @param rateRulesQ Should rate rules be given?
#' @param A list
#' @export
SI2RModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "INSPt" = "Infected Normally Symptomatic Population",
           "ISSPt" = "Infected Severely Symptomatic Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity"
        ),

      Rates =
        c(
          "deathRateTP" = "Population death rate",
          "deathRateINSP" = "Infected Normally Symptomatic Population death rate",
          "deathRateISSP" = "Infected Severely Symptomatic Population death rate",
          "contactRateIP" = "Contact rate for the infected population",
          "sspf" = "Severely symptomatic population fraction",
          "aip" = "Average infectious period",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ) ,
               {

                 newlyInfectedTerm <- contactRateINSP * SPt * INSPt / TP0 + contactRateISSP * SPt * ISSPt / TP0

                 dSPt <- - newlyInfectedTerm - deathRateTP * SPt

                 dINSPt <- (1 - sspf) * newlyInfectedTerm - INSPt / aip - deathRateINSP * INSPt

                 dISSPt <- sspf * newlyInfectedTerm - ISSPt / aip - deathRateISSP * ISSPt

                 dRPt <- (INSPt + ISSPt) / aip - deathRateTP * RPt

                 return( list( c( dSPt, dINSPt, dISSPt, dRPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c(
               SPt = 10^5 - 1,
               INSPt = 0,
               ISSPt = 1,
               RPt = 0
             )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c(
               TP0 = 10^5,
               deathRateTP = 0.0000219178,
               deathRateINSP = 0.015 / 28,
               deathRateISSP = 0.035 / 28,
               aip = 28,
               contactRateINSP = 0.56,
               contactRateISSP = 0.56,
               sspf = 0.2
             )))
  }

  res
}


##===========================================================
## SEI2RModel
##===========================================================

#' SEI2RModel model
#' @description SEI2RModel
#' @param initialConditionsQ Should initial conditions be given?
#' @param rateRulesQ Should rate rules be given?
#' @param birthsTermQ Should the births term be included or not?
#' @param A list
#' @export
SEI2RModel <- function( initialConditionsQ = TRUE, rateRulesQ = TRUE, birthsTermQ = FALSE ) {

  res <-
    list(

      Stocks =
        c( "TPt" = "Total Population" ,
           "SPt" = "Susceptible Population",
           "EPt" = "Exposed Population",
           "INSPt" = "Infected Normally Symptomatic Population",
           "ISSPt" = "Infected Severely Symptomatic Population",
           "RPt" = "Recovered Population",
           "MLPt" = "Money of Lost Productivity"
        ),

      Rates =
        c(
          "deathRateTP" = "Population death rate",
          "deathRateINSP" = "Infected Normally Symptomatic Population death rate",
          "deathRateISSP" = "Infected Severely Symptomatic Population death rate",
          "contactRateIP" = "Contact rate for the infected population",
          "sspf" = "Severely symptomatic population fraction",
          "aip" = "Average infectious period",
          "lpcr" = "Lost productivity cost rate (per person per day)"
        ),

      RHSFunction =
        function( time, state, parameters ) {
          with(as.list( c( state, parameters ) ) ,
               {

                 newlyInfectedTerm = contactRateISSP / TP0 * SPt * ISSPt + contactRateINSP / TP0 * SPt * INSPt;

                 if( birthsTermQ ) {
                   aSPt = deathRateTP * TP0 - newlyInfectedTerm - deathRateTP * SPt
                 } else {
                   dSPt = - newlyInfectedTerm - deathRateTP * SPt
                 }

                 dEPt = newlyInfectedTerm - (deathRateTP + (1 / aincp) ) * EPt

                 dINSPt = (1 - sspf) * (1 / aincp) * EPt - (1 / aip) * INSPt - deathRateINSP * INSPt

                 dISSPt = sspf * (1 / aincp) * EPt - (1 / aip) * ISSPt - deathRateISSP * ISSPt

                 dRPt = (1 / aip) * (ISSPt + INSPt) - deathRateTP * RPt

                 dMLPt = lpcr * (TP0 - RPt - SPt)


                 return( list( c( dSPt, dEPt, dINSPt, dISSPt, dRPt, dMLPt ) ) )
               }
          )
        }
    )

  if( initialConditionsQ ) {
    res <-
      c( res,
         list(
           InitialConditions =
             c(
               SPt = 10^5 - 1,
               EPt = 0,
               INSPt = 0,
               ISSPt = 1,
               RPt = 0,
               MLPt = 0
             )))
  }

  if( rateRulesQ ) {
    res <-
      c( res,
         list(
           RateRules =
             c(
               TP0 = 10^5,
               deathRateTP = 0.0000219178,
               deathRateINSP = 0.015 / 28,
               deathRateISSP = 0.035 / 28,
               aip = 28,
               aincp = 14,
               contactRateINSP = 0.56,
               contactRateISSP = 0.56,
               sspf = 0.2,
               lpcr = 1
             )))
  }

  res
}

