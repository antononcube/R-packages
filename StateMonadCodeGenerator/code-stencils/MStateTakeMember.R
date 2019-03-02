
##===========================================================
## MEMBER Taker
##===========================================================

#' Take MEMBER.
#' @description Takes MEMBER from the monad object.
#' @param msObj An MState object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{MStateFailureSymbol}.
#' @family Set/Take functions
#' @export
MStateTakeMEMBER <- function( msObj, functionName = "MStateTakeMEMBER" ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  if( !MStateMemberPresenceCheck( msObj, memberName = "MEMBER", memberPrettyName = "MEMBER", functionName = functionName,  logicalResult = TRUE) ) {
    return(MStateFailureSymbol)
  }

  msObj$MEMBER
}
