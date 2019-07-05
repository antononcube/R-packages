
#' @import magrittr
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
GNNMonUnit <- function( ) {

  res <- list( Value = NULL, Data = NULL, NumberOfNNs = NULL, DistanceMethod = NULL, NearestNeighborDistances = NULL, RadiusFunction = NULL, Radius = NULL, LowerThreshold = NULL, UpperThreshold = NULL )
  attr(res, "class") <- "GNNMon"

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
#' @return A logical value or an GNNMon object.
#' @export
GNNMonMemberPresenceCheck <- function( gnnObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(gnnObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
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

#' Set Data.
#' @description Sets Data into the monad object.
#' @param gnnObj An GNNMon object.
#' @param Data An object member to be set.
#' @return An GNNMon object.
#' @family Set/Take functions
#' @export
GNNMonSetData <- function( gnnObj, Data ) {

  if( GNNMonFailureQ(gnnObj) ) { return(GNNMonFailureSymbol) }

  if( !( is.null(Data) || is.matrix(Data)) ) {
    warning("The argument Data is expected to be NULL or a matrix.", call. = TRUE)
    return(GNNMonFailureSymbol)
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
