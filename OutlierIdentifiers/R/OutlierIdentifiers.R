##=======================================================================================
## Implementation of one dimensional outlier identifying algorithms in R
##
## BSD 3-Clause License
##
## Copyright (c) 2013, Anton Antonov
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
##
## * Redistributions of source code must retain the above copyright notice, this
## list of conditions and the following disclaimer.
##
## * Redistributions in binary form must reproduce the above copyright notice,
## this list of conditions and the following disclaimer in the documentation
## and/or other materials provided with the distribution.
##
## * Neither the name of the copyright holder nor the names of its
## contributors may be used to endorse or promote products derived from
## this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
## SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
## CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
## OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
## Written by Anton Antonov,
## antononcube@gmail.com,
## Windermere, Florida, USA.
##
##=======================================================================================
##
## This script of R functions re-implements this Mathematica package:
##
## [1] Anton Antonov, Implementation of one dimensional outlier identifying algorithms in Mathematica,
##     Mathematica package OutlierIdentifiers.m, (2013), MathematicaForPrediction project at GitHub,
##     https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m .
##
## It was easier for me to implement in R the one-dimensional outlier detection functions
## in [1] than to comprehend the signatures of the different R libraries.
##=======================================================================================

#' Hampel identifier parameters.
#' @description Find an Hampel outlier threshold for a data vector.
#' @param dataVec A data vector.
#' @export
HampelIdentifierParameters <- function( dataVec ) {
  x0 <- median(dataVec, na.rm = TRUE )
  md <- 1.4826 * median(abs(dataVec - x0), na.rm = TRUE );
  c(x0 - md, x0 + md)
}

#' Quartile identifier parameters
#' @description Find an Quartile outlier for a data vector.
#' @param dataVec A data vector.
#' @export
QuartileIdentifierParameters <- function( dataVec ) {
  res <- quantile( dataVec, c( 1/4, 1/2, 3/4 ), na.rm = TRUE )
  xL <- res[[1]]
  x0 <- res[[2]]
  xU <- res[[3]]
  c( x0 - (xU - xL), x0 + (xU - xL) )
}

#' SPLUS quartile identifier parameters
#' @description Find an SPLUS Quartile outlier for a data vector.
#' @param dataVec A data vector.
#' @export
SPLUSQuartileIdentifierParameters <- function( dataVec ) {
  if ( length(dataVec) <=4 ) {
    xL <- min(dataVec, na.rm = TRUE )
    xU <- max(dataVec, na.rm = TRUE )
  } else {
    res <- quantile( dataVec, c( 1/4, 3/4 ), na.rm = TRUE )
    xL <- res[[1]]
    xU <- res[[2]]
  }
  c( xL - 1.5*(xU-xL), xU + 1.5*(xU-xL) )
}


#' Outlier identifier.
#' @description Find an outlier threshold for a data vector.
#' @param dataVec A data vector.
#' @param lowerAndUpperThresholds outlier identifier parameters
#' @export
OutlierIdentifier <- function( dataVec, lowerAndUpperThresholds ) {
  dataVec[ dataVec <= lowerAndUpperThresholds[[1]] | dataVec >= lowerAndUpperThresholds[[2]] ]
}

#' Top outlier identifier.
#' @description Find the top outliers for a data vector
#' @param dataVec A data vector.
#' @param lowerAndUpperThresholds outlier identifier parameters
#' @export
TopOutlierIdentifier <- function( dataVec, lowerAndUpperThresholds ) {
  dataVec[dataVec >= lowerAndUpperThresholds[[2]] ]
}

#' Bottom outlier identifier.
#' @description Find the bottom outliers for a data vector.
#' @param dataVec data vector
#' @param lowerAndUpperThresholds outlier identifier parameters
#' @export
BottomOutlierIdentifier <- function( dataVec, lowerAndUpperThresholds ) {
  dataVec[dataVec <= lowerAndUpperThresholds[[1]] ]
}

#' Outlier positions finder.
#' @description Find the outlier positions in a data vector.
#' @param dataVec A data vector.
#' @param outlierIdentifier An outlier identifier function.
#' @export
OutlierPosition <- function( dataVec, outlierIdentifier = HampelIdentifierParameters ) {
  cls <- outlierIdentifier(dataVec)
  which( dataVec <= cls[[1]] | dataVec >= cls[[2]] )
}

#' Top outlier positions finder.
#' @description Find the top outlier positions in a data vector.
#' @param dataVec A data vector.
#' @param outlierIdentifier An outlier identifier function.
#' @export
TopOutlierPosition <- function( dataVec, outlierIdentifier = HampelIdentifierParameters ) {
  cls <- outlierIdentifier(dataVec)
  which( dataVec >= cls[[2]] )
}

#' Bottom outlier positions finder.
#' @description Find the bottom outlier positions in a data vector.
#' @param dataVec A data vector.
#' @param outlierIdentifier An outlier identifier function.
#' @export
BottomOutlierPosition <- function( dataVec, outlierIdentifier = HampelIdentifierParameters ) {
  cls <- outlierIdentifier(dataVec)
  which( dataVec <= cls[[1]] )
}



