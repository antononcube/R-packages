##=======================================================================================
## Geometric Nearest Neighbors Monad in R
##
## BSD 3-Clause License
##
## Copyright (c) 2019, Anton Antonov
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
## ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
## Windermere, Florida, USA.
##
##=======================================================================================

#' @import magrittr
#' @import Matrix
#' @import dplyr
#' @import purrr
#' @import OutlierIdentifiers
NULL



##===========================================================
## GNNMon failure symbol
##===========================================================

#' Failure symbol for GNNMon.
#' @description Failure symbol for the monad GNNMon.
#' @export
GNNMonFailureSymbol <- NA

#' Failure test for an GNNMon object.
#' @description Test is an GNNMon object a failure symbol.
#' @export
GNNMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## GNNMon Unit
##===========================================================

#' Make a GNNMon Unit
#' @description Creates a monad object.
#' @return An S3 class "GNNMon". In other words, a list with the attribute "class" set to "GNNMon".
#' @export
GNNMonUnit <- function( data ) {

  res <- list( Value = NULL, Data = NULL, NumberOfNNs = NULL, DistanceMethod = NULL, NearestNeighborDistances = NULL, RadiusFunction = NULL, Radius = NULL, LowerThreshold = NULL, UpperThreshold = NULL )
  attr(res, "class") <- "GNNMon"

  res <- res %>% GNNMonSetData( data )

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a GNNMon object.
#' @description Sets the value in a GNNMon monad object.
#' @param gnnObj An GNNMon object.
#' @param value The new value.
#' @return A GNNMon object.
#' @details Assigns \code{value} to \code{gnnObj$Value}.
#' @family Set/Take functions
#' @export
GNNMonSetValue <- function( gnnObj, value ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  gnnObj$Value <- value
  gnnObj
}

#' Take the value in a GNNMon object.
#' @description Takes the value from GNNMon monad object.
#' @param gnnObj An GNNMon object.
#' @return Just \code{gnnObj$Value}.
#' @family Set/Take functions
#' @export
GNNMonTakeValue <- function( gnnObj ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  gnnObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an GNNMon object.
#' @param gnnObj An GNNMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @param warningQ Should a warning be issued or not?
#' @return A logical value or an GNNMon object.
#' @export
GNNMonMemberPresenceCheck <- function( gnnObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE, warningQ = TRUE ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(gnnObj[[memberName]]) ) {
    if( warningQ ) {
      warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    }
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { GNNMonFailureSymbol }
  else { gnnObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param gnnObj An GNNMon object.
#' @return A GNNMon object.
#' @details Prints \code{f(gnnObj$Value)}.
#' @export
GNNMonEchoValue <- function( gnnObj ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  print( gnnObj$Value )

  gnnObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
#' @param gnnObj An GNNMon object.
#' @param f A function to be applied to \code{gnnObj$Value}.
#' @return A GNNMon object.
#' @details Prints \code{f(gnnObj$Value)}.
#' @export
GNNMonEchoFunctionValue <- function( gnnObj, f ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  print( f(gnnObj$Value) )

  gnnObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{gnnObj %>% f}
#' then returns the original \code{gnnObj};
#' else returns the result of \code{gnnObj %>% f}.
#' @param gnnObj An GNNMon object.
#' @param f A function to be applied to the monad object.
#' @return A GNNMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
GNNMonOption <- function( gnnObj, f ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  res <- gnnObj %>% f

  if( GNNMonFailureQ(res) ) { return(gnnObj) }

  res
}

##===========================================================
## Data setter
##===========================================================

SparseMatrixQ <- function(x) {
  sum(c("dgCMatrix", "dgRMatrix", "dgTMatrix") %in% class(x)) > 0
}

#' Set Data.
#' @description Sets Data into the monad object.
#' @param gnnObj An GNNMon object.
#' @param Data An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetData <- function( gnnObj, Data ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(Data) || is.matrix(Data) || SparseMatrixQ(Data) ) ) {
    warning("The argument Data is expected to be NULL or a matrix.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  if( is.null(rownames(Data)) ) {
    rownames(Data) <- paste0("p", formatC(1:nrow(Data), digits = ceiling(log10(nrow(Data))), flag = "0"))
  }

  gnnObj$Data <- Data

  gnnObj
}

##===========================================================
## NumberOfNNs setter
##===========================================================

#' Set NumberOfNNs.
#' @description Sets NumberOfNNs into the monad object.
#' @param gnnObj An GNNMon object.
#' @param NumberOfNNs An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetNumberOfNNs <- function( gnnObj, NumberOfNNs ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(NumberOfNNs) || is.numeric(NumberOfNNs)) ) {
    warning("The argument NumberOfNNs is expected to be NULL or a numeric.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$NumberOfNNs <- NumberOfNNs

  gnnObj
}

##===========================================================
## DistanceMethod setter
##===========================================================

#' Set DistanceMethod.
#' @description Sets DistanceMethod into the monad object.
#' @param gnnObj An GNNMon object.
#' @param DistanceMethod An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetDistanceMethod <- function( gnnObj, DistanceMethod ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(DistanceMethod) || is.character(DistanceMethod)) ) {
    warning("The argument DistanceMethod is expected to be NULL or a character.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$DistanceMethod <- DistanceMethod

  gnnObj
}

##===========================================================
## NearestNeighborDistances setter
##===========================================================

#' Set NearestNeighborDistances.
#' @description Sets NearestNeighborDistances into the monad object.
#' @param gnnObj An GNNMon object.
#' @param NearestNeighborDistances An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetNearestNeighborDistances <- function( gnnObj, NearestNeighborDistances ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(NearestNeighborDistances) || is.data.frame(NearestNeighborDistances)) ) {
    warning("The argument NearestNeighborDistances is expected to be NULL or a data.frame.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$NearestNeighborDistances <- NearestNeighborDistances

  gnnObj
}

##===========================================================
## RadiusFunction setter
##===========================================================

#' Set RadiusFunction.
#' @description Sets RadiusFunction into the monad object.
#' @param gnnObj An GNNMon object.
#' @param RadiusFunction An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetRadiusFunction <- function( gnnObj, RadiusFunction ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(RadiusFunction) || is.function(RadiusFunction)) ) {
    warning("The argument RadiusFunction is expected to be NULL or a function.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$RadiusFunction <- RadiusFunction

  gnnObj
}

##===========================================================
## Radius setter
##===========================================================

#' Set Radius.
#' @description Sets Radius into the monad object.
#' @param gnnObj An GNNMon object.
#' @param Radius An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetRadius <- function( gnnObj, Radius ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(Radius) || is.numeric(Radius)) ) {
    warning("The argument Radius is expected to be NULL or a numeric.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$Radius <- Radius

  gnnObj
}

##===========================================================
## LowerThreshold setter
##===========================================================

#' Set LowerThreshold.
#' @description Sets LowerThreshold into the monad object.
#' @param gnnObj An GNNMon object.
#' @param LowerThreshold An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetLowerThreshold <- function( gnnObj, LowerThreshold ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(LowerThreshold) || is.numeric(LowerThreshold)) ) {
    warning("The argument LowerThreshold is expected to be NULL or a numeric.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$LowerThreshold <- LowerThreshold

  gnnObj
}

##===========================================================
## UpperThreshold setter
##===========================================================

#' Set UpperThreshold.
#' @description Sets UpperThreshold into the monad object.
#' @param gnnObj An GNNMon object.
#' @param UpperThreshold An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetUpperThreshold <- function( gnnObj, UpperThreshold ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(UpperThreshold) || is.numeric(UpperThreshold)) ) {
    warning("The argument UpperThreshold is expected to be NULL or a numeric.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  gnnObj$UpperThreshold <- UpperThreshold

  gnnObj
}

##===========================================================
## Data Taker
##===========================================================

#' Take Data.
#' @description Takes Data from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeData <- function( gnnObj, functionName = "GNNMonTakeData" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "Data", memberPrettyName = "Data", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$Data
}

##===========================================================
## NumberOfNNs Taker
##===========================================================

#' Take NumberOfNNs.
#' @description Takes NumberOfNNs from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeNumberOfNNs <- function( gnnObj, functionName = "GNNMonTakeNumberOfNNs" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "NumberOfNNs", memberPrettyName = "NumberOfNNs", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$NumberOfNNs
}

##===========================================================
## DistanceMethod Taker
##===========================================================

#' Take DistanceMethod.
#' @description Takes DistanceMethod from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeDistanceMethod <- function( gnnObj, functionName = "GNNMonTakeDistanceMethod" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "DistanceMethod", memberPrettyName = "DistanceMethod", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$DistanceMethod
}

##===========================================================
## NearestNeighborDistances Taker
##===========================================================

#' Take NearestNeighborDistances.
#' @description Takes NearestNeighborDistances from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeNearestNeighborDistances <- function( gnnObj, functionName = "GNNMonTakeNearestNeighborDistances" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "NearestNeighborDistances", memberPrettyName = "NearestNeighborDistances", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$NearestNeighborDistances
}

##===========================================================
## RadiusFunction Taker
##===========================================================

#' Take RadiusFunction.
#' @description Takes RadiusFunction from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeRadiusFunction <- function( gnnObj, functionName = "GNNMonTakeRadiusFunction" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "RadiusFunction", memberPrettyName = "RadiusFunction", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$RadiusFunction
}

##===========================================================
## Radius Taker
##===========================================================

#' Take Radius.
#' @description Takes Radius from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeRadius <- function( gnnObj, functionName = "GNNMonTakeRadius" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "Radius", memberPrettyName = "Radius", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$Radius
}

##===========================================================
## LowerThreshold Taker
##===========================================================

#' Take LowerThreshold.
#' @description Takes LowerThreshold from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeLowerThreshold <- function( gnnObj, functionName = "GNNMonTakeLowerThreshold" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "LowerThreshold", memberPrettyName = "LowerThreshold", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$LowerThreshold
}

##===========================================================
## UpperThreshold Taker
##===========================================================

#' Take UpperThreshold.
#' @description Takes UpperThreshold from the monad object.
#' @param gnnObj An GNNMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{GNNMonFailureSymbol}.
#' @family Set/Take functions
#' @export
GNNMonTakeUpperThreshold <- function( gnnObj, functionName = "GNNMonTakeUpperThreshold" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !GNNMonMemberPresenceCheck( gnnObj, memberName = "UpperThreshold", memberPrettyName = "UpperThreshold", functionName = functionName,  logicalResult = TRUE) ) {
    return(GNNMonFailureSymbol)
  }

  gnnObj$UpperThreshold
}



##===========================================================
## Matrix distances
##===========================================================

## Almost an exact copy of the function SMRMatrixDistances from
##    https://github.com/antononcube/R-packages/blob/master/SparseMatrixRecommender/R/SparseMatrixRecommender.R .

#' Compute matrix-vector distances.
#' @description Computes the distances of a point to the rows of monad's
#' data matrix using a specified method.
#' @param gnnObj A GNNMon object
#' @param point A numeric vector.
#' @param method A string for the distance method.
#' One of "euclidean", "cosine".
#' @details The result is assigned to \code{gnnObj$Value}.
#' @return A GNNMon object
#' @export
GNNMonComputeMatrixDistances <- function( gnnObj, point, method = "euclidean" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  smat <- gnnObj %>% GNNMonTakeData
  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  smat <- as( smat, "dgCMatrix" )

  if( !( is.numeric(point) && length(point) == ncol(smat) ) ) {
    warning( paste0( "The argument vec is expected to a numeric vector with length that equals ncol(gnnObj$Data), ", ncol(smat), "." ),
             call. = TRUE )
    return(GNNMonFailureSymbol)
  }

  if( tolower(method) %in% c("euclidean", "euclideandistance") ) {

    ## Make the pattern matrix
    smat01 <- smat; smat01@x[ smat01@x != 0 ] <- 1

    ## From the pattern matrix and the "mean" vector
    ## Compute the subtraction sparse matrix.
    smatVec <- Matrix::t( Matrix::t(smat01) * point )
    #print( length(smatVec@x) / length(smatVec) )

    ## Find the Euclidean-distance residuals.
    vecRes <- sum( point * point ) - Matrix::rowSums( smatVec * smatVec )

    ## Compute the overall differences.
    m <- ( smat - smatVec )
    m <- Matrix::rowSums( m * m ) + vecRes

    ## Result.
    res <- sqrt(m)

  } else if ( tolower(method) %in% c( "cosine", "cosinedistance" ) ) {

    smat <- Matrix::Diagonal( x = 1 / sqrt( Matrix::rowSums( smat * smat ) ) ) %*% smat

    ## This can be optimized with by making point a sparse matrix first.
    point <- point / sqrt( sum( point * point ) )

    res <- 1 - smat %*% point
    res <- res[,1]

  } else {

    warning( paste0( "The method ", method, " is uknown." ), call. = TRUE )
    return(GNNMonFailureSymbol)

  }

  gnnObj$Value <- res
  gnnObj
}


##===========================================================
## Compute NN's distances
##===========================================================

#' Compute nearest neighbors distances.
#' @description Computes the nearest neighbors and corresponding distances
#' using a specified method for each row of monad's data matrix.
#' @param gnnObj A GNNMon object
#' @param nTopNNs Number of top nearest neighbors.
#' @param radius Radius within which the nearest neighbors are taken.
#' @param method A string for the distance method.
#' One of "euclidean", "cosine".
#' @return A GNNMon object
#' @export
GNNMonComputeNearestNeighborDistances <- function( gnnObj, nTopNNs = 6, radius = Inf, method = "euclidean" ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  data <- gnnObj %>% GNNMonTakeData

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( ! ( is.numeric(nTopNNs) && length(nTopNNs) == 1 && 0 < nTopNNs && nTopNNs <= nrow(gnnObj$Data) ) ) {
    warning( "The argument nTopNNs a number greater than 0 and smaller than nrow(gnnObj$Data) - 1.", call. = TRUE )
    return(GNNMonFailureSymbol)
  }

  dfDists <-
    purrr::map_df( 1:nrow(data), function(i) {
      pvec <- data[i,,drop=F]
      recs <- gnnObj %>% GNNMonComputeMatrixDistances( point = as.numeric(pvec) ) %>% GNNMonTakeValue
      recs <- sort(recs)[ 2 : min(length(recs), nTopNNs+1 ) ]
      if( is.numeric(radius) ) {
        recs <- recs[recs <= radius]
      }
      recNames <- names(recs)
      data.frame( SearchID = rownames(data)[[i]], SearchIndex = i, ID = recNames, Distance = setNames(recs, NULL) )
    } )

  gnnObj$NearestNeighborDistances <- dfDists
  gnnObj$NumberOfNNs <- nTopNNs
  gnnObj$DistanceMethod <- method

  gnnObj
}

##===========================================================
## Computed proximity radius thresholds
##===========================================================

#' Compute proximity radius thresholds.
#' @description Estimates the upper threshold of proximity radius.
#' @param gnnObj A GNNMon object
#' @param nnsRadiusFunction The function used to compute global
#' nearest neighbors radius.
#' @param thresholdsIdentifier An outliers identification parameters function
#' that computes to hyper-sphere thresholds from the NN hyper-spheres radiuses.
#' @details The thresholds is assigned to \code{gnnObj$Value}.
#' For \code{thresholdsIdentifier} the following functions from the package
#' \code{\link{OutlierIdentifiers}} can be used:
#' \code{\link{OutlierIdentifiers::HampelIdentifierParameters}},
#' \code{\link{OutlierIdentifiers::SPLUSQuartileIdentifierParameters}},
#' \code{\link{OutlierIdentifiers::QuartileIdentifierParameters}}.
#' @return A GNNMon object
#' @export
GNNMonComputeThresholds <- function( gnnObj,
                                     nnsRadiusFunction = mean,
                                     thresholdsIdentifier = OutlierIdentifiers::HampelIdentifierParameters )  {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  ## Get NN's and distances.
  if( !GNNMonMemberPresenceCheck(gnnObj = gnnObj, memberName = "NearestNeighborDistances", memberPrettyName = "NearestNeighborDistances", functionName = "GNNMonComputeThresholds", logicalResultQ = TRUE, warningQ = FALSE ) ) {

    warning( "Compute the nearest neighbors distances first. (Using GNNMonComputeNearestNeighborDistances).", call. = TRUE )
    return(GNNMonFailureSymbol)

  } else {

    dfDists <- gnnObj %>% GNNMonTakeNearestNeighborDistances

  }

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  ## Aggregate radiuses.
  dfDistsStats <-
    dfDists %>%
    dplyr::group_by( SearchIndex ) %>%
    dplyr::summarise_at( .vars = "Distance", .funs = c( Radius = nnsRadiusFunction, Mean = mean, SD = sd ) )

  # dfDistsStats %>%
  #   dplyr::select(Mean, SD ) %>%
  #   dplyr::summarise_all( .funs =  c( Mean = mean ) ) %>%
  #   dplyr::mutate( LowerThreshold = Mean_Mean - 1.5* SD_Mean, UpperThreshold = Mean_Mean + 1.5* SD_Mean )

  ## Identify thresholds.
  res <- thresholdsIdentifier(dfDistsStats$Radius)

  if( !( is.numeric(res) && length(res) == 2 ) ) {
    warning( "The specified thresholdsIdetifier function returned un-expected result.", call. = TRUE )
    return(GNNMonFailureSymbol)
  }

  ## Results
  gnnObj$Value <- dfDistsStats

  gnnObj$RadiusFunction <- nnsRadiusFunction
  gnnObj$Radius <- nnsRadiusFunction( dfDistsStats$Radius )
  gnnObj$LowerThreshold <- res[[1]]
  gnnObj$UpperThreshold <- res[[2]]

  gnnObj
}


##===========================================================
## Nearest points
##===========================================================

#' Find nearest points.
#' @description Finds the nearest points from the monad to a given point.
#' @param gnnObj A GNNMon object
#' @param point A numeric vector.
#' @param n Number of nearest neighbors.
#' @param radius Radius within which the nearest neighbors are taken.
#' @param method A string for the distance method.
#' One of "euclidean", "cosine".
#' @details The result is assigned to \code{gnnObj$Value}.
#' This is a fairly brute force algorithm because in order to find
#' the top nearest neighbors the distances to all monad points are computed.
#' @return A GNNMon object
#' @export
GNNMonFindNearest <- function( gnnObj, point, n = 12, radius = Inf, method = "euclidean" )  {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  gnnObj <- gnnObj %>% GNNMonComputeMatrixDistances( point = point, method = method )

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  dfNNs <- gnnObj %>% GNNMonTakeValue
  dfNNs <- data.frame( Index = 1:length(dfNNs), Distance = dfNNs )

  dfNNs <- dfNNs[ order(dfNNs$Distance)[1:n], ]

  if( is.numeric(radius) ) {
    dfNNs <- dfNNs[ dfNNs$Distance <= radius, ]
  }

  ## Result
  gnnObj$Value <- dfNNs

  gnnObj
}


##===========================================================
## Classification
##===========================================================

#' Classify for "membership".
#' @description Classifies a given point as close enough to
#' the monad points (TRUE) or too distant (FALSE).
#' @param gnnObj A GNNMon object
#' @param points A numeric vector (a point) or a matrix or a data frame
#' of points. If NULL monad's data is used.
#' @details
#' If points is a matrix or a data frame then its number of columns
#' should equal the dimension of monad's points.
#' The result is assigned to \code{gnnObj$Value}.
#' @return A GNNMon object
#' @export
GNNMonClassify <- function( gnnObj, points = NULL ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  data <- gnnObj %>% GNNMonTakeData
  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  nTopNNs <- gnnObj %>% GNNMonTakeNumberOfNNs
  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  distanceMethod <- gnnObj %>% GNNMonTakeDistanceMethod
  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  upperThreshold <- gnnObj %>% GNNMonTakeUpperThreshold
  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( is.null(points) ) {
    points <- data
  }

  if( !is.matrix(points) && is.numeric(points) ) {
    points <- matrix( points, nrow = 1 )
  }

  if( is.data.frame(points) ) {
    points <- as.matrix( points )
  }

  if( !( ( is.matrix(points) || SparseMatrixQ(points) ) && ncol(points) == ncol(data) ) ) {
    warning( "The argument points is expected to be a matrix or a sparse matrix with number of columns that equals ncol(gnnObj$Data).", call. = TRUE )
    return(GNNMonFailureSymbol)
  }

  ## Find NN's distances and "membership".
  dfDists <-
    purrr::map_df( 1:nrow(points), function(i) {

      pvec <- points[i,]

      recs <-
        gnnObj %>%
        GNNMonFindNearest( point = as.numeric(pvec), n = nTopNNs, method = distanceMethod ) %>%
        GNNMonTakeValue

      mrad <- gnnObj$RadiusFunction( recs$Distance )

      data.frame( Index = i, Radius = mrad, Label = mrad <= upperThreshold )
    } )

  ## Result
  gnnObj$Value <- dfDists

  gnnObj
}


##===========================================================
## GNNMonComputeProximityMatrix
##===========================================================

#' Compute proximity matrix.
#' @description  Computes the proximity (similarity) matrix using the matrix
#' \code{gnnObj$NearestNeighborDistances}.
#' @param gnnObj A GNNMon object
#' @param n Number of nearest neighbors.
#' @details
#' The result is assigned to \code{gnnObj$Value}.
#' @return A GNNMon object
#' @export
GNNMonComputeProximityMatrix <- function( gnnObj, n = NULL ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  dfNNDists <- gnnObj %>% GNNMonTakeNearestNeighborDistances
  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if ( is.null(n) ) {
    n <-
      gnnObj %>%
      GNNMonTakeNearestNeighborDistances %>%
      dplyr::count(SearchIndex) %>%
      dplyr::pull(n) %>%
      min

    n <- round(n)

    if ( n < 1 ) {
      warning("Too few automatically derived nearest neighbors (less than 1.)", call. = TRUE)
      return(GNNMonFailureSymbol)
    }
  }

  n <- round(n)

  if ( n < 1 ) {
    warning("The argument n is expected to be an integer greater than 0.", call. = TRUE)
    return(GNNMonFailureSymbol)
  }

  # Add diagonal
  lsIDs <- unique(dfNNDists$SearchID)
  dfNNDists <- rbind(dfNNDists[, c("SearchID", "ID", "Distance")],
                     data.frame( SearchID = lsIDs, ID = lsIDs, Distance = 0))

  # Cross tabulate
  matProx <- xtabs( formula = Distance ~ SearchID + ID, data = dfNNDists, sparse = TRUE)

  # Breaks
  lsBreaks <- setNames(quantile(matProx@x[matProx@x > 0], seq(0, 1, 1/n)), NULL)

  # Convert distance into proximity scores
  matProx@x <- findInterval(x = matProx@x, vec = lsBreaks, all.inside = F)
  m <- max(matProx@x)
  m2 <- m*m
  matProx@x <- (m2 + matProx@x - m * matProx@x) / m2

  # Proper diagonal
  matProx <- Reduce( f = function(m, i) { m[i, i] <- 1; m }, init = matProx, x = 1:nrow(matProx))

  ## Result
  gnnObj$Value <- matProx

  gnnObj
}



