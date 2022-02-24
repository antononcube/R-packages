
#' @import magrittr
#' @import SparseMatrixRecommender
#' @import SMRMon
#' @import arules
#' @import purrr
NULL


##===========================================================
## RSTMon failure symbol
##===========================================================

#' Failure symbol for RSTMon.
#' @description Failure symbol for the monad RSTMon.
#' @export
RSTMonFailureSymbol <- NA

#' Failure test for an RSTMon object.
#' @description Test is an RSTMon object a failure symbol.
#' @export
RSTMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## Failure function
##===========================================================

#' Failure producing function
#' @description Puts the monad object into a monadic failure.
#' @param rstObj An RSTMon object.
#' @param message A message to echoed. If NULL no message is echoed.
#' @return A RSTMon object.
#' @export
RSTMonFailure <- function( rstObj, message = NULL ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( is.character(message) ) {
    warning( message, call. = FALSE )
  }

  RSTMonFailureSymbol
}


##===========================================================
## RSTMon Unit
##===========================================================

#' Make a RSTMon Unit
#' @description Creates a monad object.
#' @return An S3 class "RSTMon". In other words, a list with the attribute "class" set to "RSTMon".
#' @export
RSTMonUnit <- function( ) {

  res <- list( Value = NULL, SMR = NULL, ProfileTagTypes = NULL, LabelTagType = NULL, NTopLabels = NULL, NTopRecs = NULL, FreqSetMinSupport = NULL, FreqSetMinCount = NULL, FreqSetsMinLen = NULL, FreqSetsMaxLen = NULL, Itemsets = NULL )
  attr(res, "class") <- "RSTMon"

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a RSTMon object.
#' @description Sets the value in a RSTMon monad object.
#' @param rstObj An RSTMon object.
#' @param value The new value.
#' @return A RSTMon object.
#' @details Assigns \code{value} to \code{rstObj$Value}.
#' @family Set/Take functions
#' @export
RSTMonSetValue <- function( rstObj, value ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  rstObj$Value <- value
  rstObj
}

#' Take the value in a RSTMon object.
#' @description Takes the value from RSTMon monad object.
#' @param rstObj An RSTMon object.
#' @return Just \code{rstObj$Value}.
#' @family Set/Take functions
#' @export
RSTMonTakeValue <- function( rstObj ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  rstObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an RSTMon object.
#' @param rstObj An RSTMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResultQ Should the result be a logical value?
#' @return A logical value or an RSTMon object.
#' @export
RSTMonMemberPresenceCheck <- function( rstObj, memberName, memberPrettyName = memberName, functionName = "", logicalResultQ = FALSE ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(rstObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResultQ ) { res }
  else if ( !logicalResultQ && !res) { RSTMonFailureSymbol }
  else { rstObj }
}


##===========================================================
## Echo monad's value
##===========================================================

#' Echo monad's value.
#' @description Prints the "Value" element/member of the monad object.
#' @param rstObj An RSTMon object.
#' @return A RSTMon object.
#' @details Prints \code{rstObj$Value}.
#' @export
RSTMonEchoValue <- function( rstObj ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  print( rstObj$Value )

  rstObj
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Echo function application to monad's value.
#' @description Applies a function to the "Value" element/member of the monad object
#' and prints the result.
#' @param rstObj An RSTMon object.
#' @param f A function to be applied to \code{rstObj$Value}.
#' @return A RSTMon object.
#' @details Prints \code{f(rstObj$Value)}.
#' @export
RSTMonEchoFunctionValue <- function( rstObj, f ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  print( f(rstObj$Value) )

  rstObj
}


##===========================================================
## Echo
##===========================================================

#' Echo.
#' @description Echoes given argument.
#' @param rstObj An RSTMon object.
#' @param x Object to be echoed.
#' @return An RSTMon object
#' @family Echo functions
#' @export
RSTMonEcho <- function( rstObj, x ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  print(x)

  rstObj
}


##===========================================================
## Optional function application over monad's object
##===========================================================

#' Optional function application to monad's object.
#' @description If monadic failure is obtained from \code{f(rstObj)}
#' then returns the original \code{rstObj};
#' else returns the result of \code{f(rstObj)}.
#' @param rstObj An RSTMon object.
#' @param f A function to be applied to the monad object.
#' @return A RSTMon object.
#' @details In general \code{f} should return a monad object,
#' but that is not enforced.
#' @export
RSTMonOption <- function( rstObj, f ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  res <- rstObj %>% f

  if( RSTMonFailureQ(res) ) { return(rstObj) }

  res
}

##===========================================================
## SMR setter
##===========================================================

#' Set SMR.
#' @description Sets SMR into the monad object.
#' @param rstObj An RSTMon object.
#' @param SMR An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetSMR <- function( rstObj, SMR ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(SMR) || is.list(SMR)) ) {
    warning("The argument SMR is expected to be NULL or a list.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$SMR <- SMR

  rstObj
}

##===========================================================
## ProfileTagTypes setter
##===========================================================

#' Set ProfileTagTypes.
#' @description Sets ProfileTagTypes into the monad object.
#' @param rstObj An RSTMon object.
#' @param ProfileTagTypes An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetProfileTagTypes <- function( rstObj, ProfileTagTypes ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(ProfileTagTypes) || is.character(ProfileTagTypes)) ) {
    warning("The argument ProfileTagTypes is expected to be NULL or a character.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$ProfileTagTypes <- ProfileTagTypes

  rstObj
}

##===========================================================
## LabelTagType setter
##===========================================================

#' Set LabelTagType.
#' @description Sets LabelTagType into the monad object.
#' @param rstObj An RSTMon object.
#' @param LabelTagType An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetLabelTagType <- function( rstObj, LabelTagType ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(LabelTagType) || is.character(LabelTagType)) ) {
    warning("The argument LabelTagType is expected to be NULL or a character.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$LabelTagType <- LabelTagType

  rstObj
}

##===========================================================
## NTopLabels setter
##===========================================================

#' Set NTopLabels.
#' @description Sets NTopLabels into the monad object.
#' @param rstObj An RSTMon object.
#' @param NTopLabels An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetNTopLabels <- function( rstObj, NTopLabels ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(NTopLabels) || is.integer(NTopLabels)) ) {
    warning("The argument NTopLabels is expected to be NULL or a integer.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$NTopLabels <- NTopLabels

  rstObj
}

##===========================================================
## NTopRecs setter
##===========================================================

#' Set NTopRecs.
#' @description Sets NTopRecs into the monad object.
#' @param rstObj An RSTMon object.
#' @param NTopRecs An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetNTopRecs <- function( rstObj, NTopRecs ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(NTopRecs) || is.integer(NTopRecs)) ) {
    warning("The argument NTopRecs is expected to be NULL or a integer.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$NTopRecs <- NTopRecs

  rstObj
}

##===========================================================
## FreqSetMinSupport setter
##===========================================================

#' Set FreqSetMinSupport.
#' @description Sets FreqSetMinSupport into the monad object.
#' @param rstObj An RSTMon object.
#' @param FreqSetMinSupport An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetFreqSetMinSupport <- function( rstObj, FreqSetMinSupport ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(FreqSetMinSupport) || is.numeric(FreqSetMinSupport)) ) {
    warning("The argument FreqSetMinSupport is expected to be NULL or a numeric.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetMinSupport <- FreqSetMinSupport

  rstObj
}

##===========================================================
## FreqSetMinCount setter
##===========================================================

#' Set FreqSetMinCount.
#' @description Sets FreqSetMinCount into the monad object.
#' @param rstObj An RSTMon object.
#' @param FreqSetMinCount An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetFreqSetMinCount <- function( rstObj, FreqSetMinCount ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(FreqSetMinCount) || is.integer(FreqSetMinCount)) ) {
    warning("The argument FreqSetMinCount is expected to be NULL or a integer.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetMinCount <- FreqSetMinCount

  rstObj
}

##===========================================================
## FreqSetsMinLen setter
##===========================================================

#' Set FreqSetsMinLen.
#' @description Sets FreqSetsMinLen into the monad object.
#' @param rstObj An RSTMon object.
#' @param FreqSetsMinLen An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetFreqSetsMinLen <- function( rstObj, FreqSetsMinLen ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(FreqSetsMinLen) || is.integer(FreqSetsMinLen)) ) {
    warning("The argument FreqSetsMinLen is expected to be NULL or a integer.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetsMinLen <- FreqSetsMinLen

  rstObj
}

##===========================================================
## FreqSetsMaxLen setter
##===========================================================

#' Set FreqSetsMaxLen.
#' @description Sets FreqSetsMaxLen into the monad object.
#' @param rstObj An RSTMon object.
#' @param FreqSetsMaxLen An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetFreqSetsMaxLen <- function( rstObj, FreqSetsMaxLen ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(FreqSetsMaxLen) || is.integer(FreqSetsMaxLen)) ) {
    warning("The argument FreqSetsMaxLen is expected to be NULL or a integer.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetsMaxLen <- FreqSetsMaxLen

  rstObj
}

##===========================================================
## Itemsets setter
##===========================================================

#' Set Itemsets.
#' @description Sets Itemsets into the monad object.
#' @param rstObj An RSTMon object.
#' @param Itemsets An object member to be set.
#' @return An RSTMon object.
#' @family Set/Take functions
#' @export
RSTMonSetItemsets <- function( rstObj, Itemsets ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !( is.null(Itemsets) || is.data.frame(Itemsets)) ) {
    warning("The argument Itemsets is expected to be NULL or a data.frame.", call. = TRUE)
    return(RSTMonFailureSymbol)
  }

  rstObj$Itemsets <- Itemsets

  rstObj
}

##===========================================================
## SMR Taker
##===========================================================

#' Take SMR.
#' @description Takes SMR from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeSMR <- function( rstObj, functionName = "RSTMonTakeSMR" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "SMR", memberPrettyName = "SMR", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$SMR
}

##===========================================================
## ProfileTagTypes Taker
##===========================================================

#' Take ProfileTagTypes.
#' @description Takes ProfileTagTypes from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeProfileTagTypes <- function( rstObj, functionName = "RSTMonTakeProfileTagTypes" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "ProfileTagTypes", memberPrettyName = "ProfileTagTypes", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$ProfileTagTypes
}

##===========================================================
## LabelTagType Taker
##===========================================================

#' Take LabelTagType.
#' @description Takes LabelTagType from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeLabelTagType <- function( rstObj, functionName = "RSTMonTakeLabelTagType" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "LabelTagType", memberPrettyName = "LabelTagType", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$LabelTagType
}

##===========================================================
## NTopLabels Taker
##===========================================================

#' Take NTopLabels.
#' @description Takes NTopLabels from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeNTopLabels <- function( rstObj, functionName = "RSTMonTakeNTopLabels" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "NTopLabels", memberPrettyName = "NTopLabels", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$NTopLabels
}

##===========================================================
## NTopRecs Taker
##===========================================================

#' Take NTopRecs.
#' @description Takes NTopRecs from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeNTopRecs <- function( rstObj, functionName = "RSTMonTakeNTopRecs" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "NTopRecs", memberPrettyName = "NTopRecs", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$NTopRecs
}

##===========================================================
## FreqSetMinSupport Taker
##===========================================================

#' Take FreqSetMinSupport.
#' @description Takes FreqSetMinSupport from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeFreqSetMinSupport <- function( rstObj, functionName = "RSTMonTakeFreqSetMinSupport" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "FreqSetMinSupport", memberPrettyName = "FreqSetMinSupport", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetMinSupport
}

##===========================================================
## FreqSetMinCount Taker
##===========================================================

#' Take FreqSetMinCount.
#' @description Takes FreqSetMinCount from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeFreqSetMinCount <- function( rstObj, functionName = "RSTMonTakeFreqSetMinCount" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "FreqSetMinCount", memberPrettyName = "FreqSetMinCount", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetMinCount
}

##===========================================================
## FreqSetsMinLen Taker
##===========================================================

#' Take FreqSetsMinLen.
#' @description Takes FreqSetsMinLen from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeFreqSetsMinLen <- function( rstObj, functionName = "RSTMonTakeFreqSetsMinLen" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "FreqSetsMinLen", memberPrettyName = "FreqSetsMinLen", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetsMinLen
}

##===========================================================
## FreqSetsMaxLen Taker
##===========================================================

#' Take FreqSetsMaxLen.
#' @description Takes FreqSetsMaxLen from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeFreqSetsMaxLen <- function( rstObj, functionName = "RSTMonTakeFreqSetsMaxLen" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "FreqSetsMaxLen", memberPrettyName = "FreqSetsMaxLen", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$FreqSetsMaxLen
}

##===========================================================
## Itemsets Taker
##===========================================================

#' Take Itemsets.
#' @description Takes Itemsets from the monad object.
#' @param rstObj An RSTMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{RSTMonFailureSymbol}.
#' @family Set/Take functions
#' @export
RSTMonTakeItemsets <- function( rstObj, functionName = "RSTMonTakeItemsets" ) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if( !RSTMonMemberPresenceCheck( rstObj, memberName = "Itemsets", memberPrettyName = "Itemsets", functionName = functionName,  logicalResult = TRUE) ) {
    return(RSTMonFailureSymbol)
  }

  rstObj$Itemsets
}



##===========================================================
## IngestSMRMatrixFromCSVFile
##===========================================================

#' IngestSMRMatrixFromCSVFile.
#' @description Ingests the SMR matrix data in a given CSV file.
#' @param rstObj An RSTMon object.
#' @param fileName A string that is file name.
#' @param ... Additional parameters for \code{\link{SparseMatrixRecommender::SMRCreateFromLongForm}}.
#' @return An RSTMon object or \code{RSTMonFailureSymbol}.
#' @details The obtained recommender object is assigned to \code{rstObj$SMR}.
#' @family Ingest functions
#' @export
RSTMonIngestSMRMatrixFromCSVFile <- function(rstObj, fileName, ...) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  ## Should check does the file exist
  if ( !file.exists(fileName) ) {
    return(RSTMonFailureSymbol)
  }

  ## Ingest
  dfSMRMatrix <- read.csv(file = fileName)

  ## Make SMR object
  rstObj$SMR <- SparseMatrixRecommender::SMRCreateFromLongForm(data = dfSMRMatrix, ...)

  ## Result
  rstObj
}


##===========================================================
## IngestSMRMatrixFromFeatherFile.
##===========================================================

#' IngestSMRMatrixFromFeatherFile.
#' @description Ingests the SMR matrix data in a given feather file.
#' @param rstObj An RSTMon object.
#' @param fileName A string that is file name.
#' @param ... Additional parameters for \code{\link{SMRCreateFromLongForm}}.
#' @return An RSTMon object or \code{RSTMonFailureSymbol}.
#' @details The obtained recommender object is assigned to \code{rstObj$SMR}.
#' @family Ingest functions
#' @export
RSTMonIngestSMRMatrixFromFeatherFile <- function(rstObj, fileName, ...) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  ## Should check does the file exist
  if ( !file.exists(fileName) ) {
    return(RSTMonFailureSymbol)
  }

  ## Ingest
  dfSMRMatrix <- feather::read_feather(path = fileName)

  ## Make SMR object
  rstObj$SMR <- SparseMatrixRecommender::SMRCreateFromLongForm(data = dfSMRMatrix, ...)

  ## Result
  rstObj
}


##===========================================================
## FindFrequentSets
##===========================================================

#' FindFrequentSets
#' @description Find frequent sets of tags for given profile
#' tag types and algorithm parameters.
#' @param rstObj An RSTMon object.
#' @param fileName A string that is file name.
#' @param profileTagTypes Tag type use for making the profiles.
#' If NULL then \code{rstObj$ProfileTagTypes}.
#' @param supp Min support for the found frequent sets.
#' If NULL then \code{rstObj$FreqSetMinSupport}.
#' @param minlen Min length for the found frequent sets.
#' If NULL then \code{rstObj$FreqSetsMinLen}.
#' @param maxlen Max length for the found frequent sets.
#' If NULL then \code{rstObj$FreqSetsMaxLen}.
#' @param target Type of frequent set found.
#' @return An RSTMon object or \code{RSTMonFailureSymbol}.
#' @details The obtained data frame of frequent sets is assigned
#' to \code{rstObj$Value} and \code{rstObj$Itemsets}.
#' @family Computation functions
#' @export
RSTMonFindFrequentSets <- function(rstObj, profileTagTypes = NULL, supp = NULL, minlen = 2, maxlen = 5, target = "maximally frequent itemsets") {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if ( is.null(profileTagTypes) ) { profileTagTypes <- rstObj$ProfileTagTypes }
  if ( is.null(supp) )            { supp            <- rstObj$FreqSetMinSupport }
  if ( is.null(minlen) )          { minlen          <- rstObj$FreqSetsMinLen }
  if ( is.null(maxlen) )          { maxlen          <- rstObj$FreqSetsMaxLen }

  ## Make the profiles matrix
  lsMats <-
    purrr::map( strsplit( x = profileTagTypes, split = " ")[[1]], function(tt) {
      m <- SMRSubMatrix( rstObj$SMR, tt )
      #colnames(m) <- gsub(paste0(tt,":"), "", colnames(m))
      m
    })
  smat <- do.call( cbind, lsMats)
  smat <- smat[rowSums(smat) > 0, ]
  smat@x[smat@x > 0] <- 1
  smat <- Matrix::drop0(smat)

  # Make sure it is ngCMatrix
  smat <- as(smat, "ngCMatrix")

  ## Find frequent sets
  itemsets <- arules::eclat( t(smat), parameter = list(supp = supp, minlen = minlen, maxlen = maxlen, target = target) )

  ## Convert result itemsets into a data frame
  rstObj$Itemsets <- as(itemsets, "data.frame")

  colnames(rstObj$Itemsets) <- gsub("items",   "Items",   colnames(rstObj$Itemsets) )
  colnames(rstObj$Itemsets) <- gsub("support", "Support", colnames(rstObj$Itemsets) )
  colnames(rstObj$Itemsets) <- gsub("count",   "Count",   colnames(rstObj$Itemsets) )

  ## Make sure the Items columns is of strings
  rstObj$Itemsets$Items <- as.character(rstObj$Itemsets$Items)

  ## Result
  rstObj$Value <- rstObj$Itemsets
  rstObj
}


##===========================================================
## MakeRecommendationTests
##===========================================================

#' MakeRecommendationTests
#' @description Ingests the SMR matrix data in a given feather file.
#' @param rstObj An RSTMon object.
#' @param tagType The tag type to assign to the test.
#' If NULL then \code{rstObj$SMR$ItemColumnName} is used.
#' @return An RSTMon object or \code{RSTMonFailureSymbol}.
#' @details The obtained data frame of tests is assigned to
#' \code{rstObj$Value}.
#' @family Computation functions
#' @export
RSTMonMakeRecommendationTests <- function(rstObj, tagType = NULL, nrecs = 10) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if ( is.null(tagType) ) { tagType <- rstObj$SMR$ItemColumnName }

  ## Computation
  dfTests <-
    purrr::map_df( rstObj$Itemsets$Items, function(x) {

      lsProfile <- strsplit( x, split = "\\{|\\}|,")[[1]]
      lsProfile <- lsProfile[ nchar(lsProfile) > 0 ]

      MakeTestByRecommendation( smr = rstObj$SMR,
                                tagType = tagType,
                                profile = lsProfile,
                                nrecs = nrecs,
                                normalizeQ = TRUE )
    })

  rownames(dfTests) <- NULL

  ## Result
  rstObj$Value <- dfTests
  rstObj
}


##===========================================================
## MakeClassificationTests
##===========================================================

#' MakeClassificationTests
#' @description Ingests the SMR matrix data in a given feather file.
#' @param rstObj An RSTMon object.
#' @param tagType The tag type to assign to the test.
#' @return An RSTMon object or \code{RSTMonFailureSymbol}.
#' @details The obtained data frame of tests is assigned to
#' \code{rstObj$Value}.
#' @family Computation functions
#' @export
RSTMonMakeClassificationTests <- function(rstObj, tagType = NULL, nTestLabels = 10) {

  if( RSTMonFailureQ(rstObj) ) { return(RSTMonFailureSymbol) }

  if ( is.null(tagType) ) { tagType <- rstObj$SMR$ItemColumnName }

  if ( !(tagType %in% SMRMonTakeTagTypes(rstObj$SMR) ) ) {
    warning("The value of the arugment tagType is not a known tag type in rstObj$SMR.", call. = T)
    return(RSTMonFailureSymbol)
  }

  ## Computation
  dfTests <-
    purrr::map_df( rstObj$Itemsets$Items, function(x) {

      lsProfile <- trimws(strsplit( x, split = "\\{|\\}|,")[[1]])
      lsProfile <- lsProfile[ nchar(lsProfile) > 0 ]

      MakeTestByClassification( smr = rstObj$SMR,
                                tagType = tagType,
                                profile = lsProfile,
                                nTestLabels = nTestLabels )
    })

  rownames(dfTests) <- NULL

  ## Result
  rstObj$Value <- dfTests
  rstObj
}
