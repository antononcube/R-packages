
#' Set data.
#' @description Sets an event records data frame into the monad object.
#' @param msObj An MState object.
#' @param data A data frame with event records.
#' @return An MState object.
#' @family Set/Take functions
#' @export
MStateSetData <- function( msObj, data ) {

  if( MStateFailureQ(msObj) ) { return(MStateFailureSymbol) }

  if ( is.vector(data) ) {

    MStateSetData( msObj, data.frame( Var1 = 1:length(data), Var2 = data ) )

  } else if ( ( is.matrix(data) || is.data.frame(data) ) && ncol(data) == 1 ) {

    MStateSetData( msObj, data.frame( Var1 = 1:nrow(data), Var2 = data[,1] ) )

  } else if ( is.matrix(data) || is.data.frame(data) ) {

    expectedColNames <- c("Var1", "Var2" )

    if( ! ( is.data.frame(data) && length(intersect( colnames(data), expectedColNames)) == length(expectedColNames) ) ) {
      warning( paste( "The argument data is expected to be a data frame with columns: {", paste(expectedColNames, collapse =", "), "}."), call. = TRUE)
      warning( paste0( "Proceeding by renaming the first columm \"", colnames(data)[[1]], "\" as \"Var1\" ",
                       "and renaming the second columm \"", colnames(data)[[2]], "\" as \"Var2\"." ), call. = TRUE )
      data <- data[,1:2]
      colnames(data) <- expectedColNames
    }

    ## Conditionsƒ

    if( !is.numeric(data$Var1) || !is.numeric(data$Var2) ) {
      warning( "The columns 'Var1' and 'Var1' of the argument data are expected to be numeric.", call. = TRUE)
      return(MStateFailureSymbol)
    }

    msObj$Data <- data[, expectedColNames]

    msObj$Data <- msObj$Data[ complete.cases(msObj$Data), ]

    msObj
  }
}
