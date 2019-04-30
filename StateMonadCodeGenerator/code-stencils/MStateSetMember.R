
##===========================================================
## MEMBER setter
##===========================================================

#' Set MEMBER.
#' @description Sets MEMBER into the monad object.
#' @param msObj An MState object.
#' @param MEMBER An object member to be set.
#' @return An MState object.
#' @family Set/Take functions
#' @export
MStateSetMEMBER <- function( msObj, MEMBER ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  if( !( is.null(MEMBER) || is.CLASSNAME(MEMBER)) ) {
    warning("The argument MEMBER is expected to be NULL or a CLASSNAME.", call. = TRUE)
    return(MStateFailureSymbol)
  }

  msObj$MEMBER <- MEMBER

  msObj
}
