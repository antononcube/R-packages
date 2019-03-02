
#' Data summary.
#' @description Summarize data and print the summary.
#' @param msObj An MState object.
#' @return A MState object.
#' @details Prints data dimensions and summary.
#' @export
MStateEchoDataSummary <- function( msObj ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  data <- MStateTakeData( msObj = msObj, functionName = "MStateEchoDataSummary" )
  if( MStateFailureQ(data) ) { return(MStateFailureSymbol) }

  print( list( Dimensions = dim(data), Summary = summary(data) ) )

  msObj
}
