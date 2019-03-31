##===========================================================
## Latent Semantic Analysis workflows monad in R
## Copyright (C) 2019  Anton Antonov
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


#' @import purrr
#' @import Matrix
#' @import magrittr
#' @import irlba
#' @import NonNegativeMatrixFactorization
#' @import SparseMatrixRecommender
#' @import lattice
NULL


##===========================================================
## LSAMon failure symbol
##===========================================================

#' Failure symbol for LSAMon.
#' @description Failure symbol for the monad LSAMon.
#' @export
LSAMonFailureSymbol <- NA

#' Failure test for an LSAMon object.
#' @description Test is an LSAMon object a failure symbol.
#' @export
LSAMonFailureQ <- function(x) { mean(is.na(x)) }


##===========================================================
## LSAMon Unit
##===========================================================

#' Make a LSAMon Unit
#' @description Creates a monad object.
#' @param Documents A character vector, a list of strings, or NULL.
#' @return An S3 class "LSAMon". In other words, a list with the attribute "class" set to "LSAMon".
#' @export
LSAMonUnit <- function( Documents = NULL ) {

  res <- list( Value = NULL, Documents = NULL, DocumentTermMatrix = NULL, Terms = NULL, WeightedDocumentTermMatrix = NULL, W = NULL, H = NULL, TopicColumnPositions = NULL, AutomaticTopicNames = NULL )
  attr(res, "class") <- "LSAMon"

  if( !is.null(Documents) ) {
    res <- res %>% LSAMonSetDocuments( Documents = Documents )
  }

  res
}


##===========================================================
## Value setter and getter
##===========================================================

#' Set the value in a LSAMon object.
#' @description Sets the value in a LSAMon monad object.
#' @param lsaObj An LSAMon object.
#' @param value The new value.
#' @return A LSAMon object.
#' @details Assigns \code{value} to \code{lsaObj$Value}.
#' @family Set/Take functions
#' @export
LSAMonSetValue <- function( lsaObj, value ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  lsaObj$Value <- value
  lsaObj
}

#' Take the value in a LSAMon object.
#' @description Takes the value from LSAMon monad object.
#' @param lsaObj An LSAMon object.
#' @return Just \code{lsaObj$Value}.
#' @family Set/Take functions
#' @export
LSAMonTakeValue <- function( lsaObj ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  lsaObj$Value
}


##===========================================================
## Member presence check
##===========================================================

#' General member presence check.
#' @description A general function for checking the presence of a data member in an LSAMon object.
#' @param lsaObj An LSAMon object.
#' @param memberName The name of the member to be checked.
#' @param memberPrettyName A pretty member name (for messages).
#' @param functionName The name of the delegating function.
#' @param logicalResult Should the result be logical value?
#' @return A logical value or an LSAMon object.
#' @export
LSAMonMemberPresenceCheck <- function( lsaObj, memberName, memberPrettyName = memberName, functionName = "", logicalResult = FALSE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  res <- TRUE

  if( nchar(functionName) > 0 ) { functionName <- paste0( functionName, ":: ") }

  if( is.null(lsaObj[[memberName]]) ) {
    warning( paste0( functionName, paste0("Cannot find ", memberPrettyName, ".") ), call. = TRUE )
    res <- FALSE
  }

  if( logicalResult ) { res }
  else if ( !logicalResult && !res) { LSAMonFailureSymbol }
  else { lsaObj }
}


##===========================================================
## Echo function application of over monad's value
##===========================================================

#' Function application to monad's value.
#' @description Apply a function to the "Value" element/member of the monad object.
#' @param lsaObj An LSAMon object.
#' @param f A function to be applied to \code{lsaObj$Value}.
#' @return A LSAMon object.
#' @details Prints \code{f(lsaObj$Value)}.
#' @export
LSAMonEchoFunctionValue <- function( lsaObj, f ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  print( f(lsaObj$Value) )

  lsaObj
}



##===========================================================
## Documents setter
##===========================================================

#' Set Documents.
#' @description Sets Documents into the monad object.
#' @param lsaObj An LSAMon object.
#' @param Documents A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetDocuments <- function( lsaObj, Documents ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(Documents) || is.character(Documents)) ) {
    warning("The argument Documents is expected to be NULL or a character.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$Documents <- Documents

  lsaObj
}

##===========================================================
## DocumentTermMatrix setter
##===========================================================

#' Set DocumentTermMatrix.
#' @description Sets DocumentTermMatrix into the monad object.
#' @param lsaObj An LSAMon object.
#' @param DocumentTermMatrix A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetDocumentTermMatrix <- function( lsaObj, DocumentTermMatrix ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(DocumentTermMatrix) || ( "dgCMatrix" %in% class(DocumentTermMatrix) ) ) ) {
    warning("The argument DocumentTermMatrix is expected to be NULL or a sparse matrix of type 'dgCMatrix'.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$DocumentTermMatrix <- DocumentTermMatrix

  lsaObj
}

##===========================================================
## Terms setter
##===========================================================

#' Set Terms.
#' @description Sets Terms into the monad object.
#' @param lsaObj An LSAMon object.
#' @param Terms A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetTerms <- function( lsaObj, Terms ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(Terms) || is.character(Terms)) ) {
    warning("The argument Terms is expected to be NULL or a character.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$Terms <- Terms

  lsaObj
}

##===========================================================
## WeightedDocumentTermMatrix setter
##===========================================================

#' Set WeightedDocumentTermMatrix.
#' @description Sets WeightedDocumentTermMatrix into the monad object.
#' @param lsaObj An LSAMon object.
#' @param WeightedDocumentTermMatrix A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetWeightedDocumentTermMatrix <- function( lsaObj, WeightedDocumentTermMatrix ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(WeightedDocumentTermMatrix) || is.matrix(WeightedDocumentTermMatrix)) ) {
    warning("The argument WeightedDocumentTermMatrix is expected to be NULL or a matrix.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$WeightedDocumentTermMatrix <- WeightedDocumentTermMatrix

  lsaObj
}

##===========================================================
## W setter
##===========================================================

#' Set W.
#' @description Sets W into the monad object.
#' @param lsaObj An LSAMon object.
#' @param W A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetW <- function( lsaObj, W ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(W) || is.matrix(W)) ) {
    warning("The argument W is expected to be NULL or a matrix.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$W <- W

  lsaObj
}

##===========================================================
## H setter
##===========================================================

#' Set H.
#' @description Sets H into the monad object.
#' @param lsaObj An LSAMon object.
#' @param H A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetH <- function( lsaObj, H ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(H) || is.matrix(H)) ) {
    warning("The argument H is expected to be NULL or a matrix.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$H <- H

  lsaObj
}

##===========================================================
## TopicColumnPositions setter
##===========================================================

#' Set TopicColumnPositions.
#' @description Sets TopicColumnPositions into the monad object.
#' @param lsaObj An LSAMon object.
#' @param TopicColumnPositions A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetTopicColumnPositions <- function( lsaObj, TopicColumnPositions ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(TopicColumnPositions) || is.integer(TopicColumnPositions)) ) {
    warning("The argument TopicColumnPositions is expected to be NULL or a integer.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$TopicColumnPositions <- TopicColumnPositions

  lsaObj
}

##===========================================================
## AutomaticTopicNames setter
##===========================================================

#' Set AutomaticTopicNames.
#' @description Sets AutomaticTopicNames into the monad object.
#' @param lsaObj An LSAMon object.
#' @param AutomaticTopicNames A list of regression objects.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetAutomaticTopicNames <- function( lsaObj, AutomaticTopicNames ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(AutomaticTopicNames) || is.character(AutomaticTopicNames)) ) {
    warning("The argument AutomaticTopicNames is expected to be NULL or a character.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$AutomaticTopicNames <- AutomaticTopicNames

  lsaObj
}

##===========================================================
## Documents Taker
##===========================================================

#' Take Documents.
#' @description Takes Documents from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeDocuments <- function( lsaObj, functionName = "LSAMonTakeDocuments" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "Documents", memberPrettyName = "Documents", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$Documents
}

##===========================================================
## DocumentTermMatrix Taker
##===========================================================

#' Take DocumentTermMatrix.
#' @description Takes DocumentTermMatrix from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeDocumentTermMatrix <- function( lsaObj, functionName = "LSAMonTakeDocumentTermMatrix" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "DocumentTermMatrix", memberPrettyName = "DocumentTermMatrix", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$DocumentTermMatrix
}

##===========================================================
## Terms Taker
##===========================================================

#' Take Terms.
#' @description Takes Terms from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeTerms <- function( lsaObj, functionName = "LSAMonTakeTerms" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "Terms", memberPrettyName = "Terms", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$Terms
}

##===========================================================
## WeightedDocumentTermMatrix Taker
##===========================================================

#' Take WeightedDocumentTermMatrix.
#' @description Takes WeightedDocumentTermMatrix from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeWeightedDocumentTermMatrix <- function( lsaObj, functionName = "LSAMonTakeWeightedDocumentTermMatrix" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "WeightedDocumentTermMatrix", memberPrettyName = "WeightedDocumentTermMatrix", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$WeightedDocumentTermMatrix
}

##===========================================================
## W Taker
##===========================================================

#' Take W.
#' @description Takes W from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeW <- function( lsaObj, functionName = "LSAMonTakeW" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "W", memberPrettyName = "W", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$W
}

##===========================================================
## H Taker
##===========================================================

#' Take H.
#' @description Takes H from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeH <- function( lsaObj, functionName = "LSAMonTakeH" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "H", memberPrettyName = "H", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$H
}

##===========================================================
## TopicColumnPositions Taker
##===========================================================

#' Take TopicColumnPositions.
#' @description Takes TopicColumnPositions from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeTopicColumnPositions <- function( lsaObj, functionName = "LSAMonTakeTopicColumnPositions" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "TopicColumnPositions", memberPrettyName = "TopicColumnPositions", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$TopicColumnPositions
}

##===========================================================
## AutomaticTopicNames Taker
##===========================================================

#' Take AutomaticTopicNames.
#' @description Takes AutomaticTopicNames from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeAutomaticTopicNames <- function( lsaObj, functionName = "LSAMonTakeAutomaticTopicNames" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "AutomaticTopicNames", memberPrettyName = "AutomaticTopicNames", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  lsaObj$AutomaticTopicNames
}



##===========================================================
## Make document-term matrix
##===========================================================

#' Document-term matrix creation.
#' @description Make a document-term matrix for a collection of documents
#' using specified word splitting and optional word stemming.
#' @param lsaObj A LSAMon object.
#' @param splitPattern Pattern to use for splitting the documents into words.
#' @param stemWordsQ Should the words be stemmed or not?
#' @param stopWords A character vector with stop words to be removed.
#' @return A LSAMon object.
#' @details The obtained sparse matrix is \code{lsaObj$DocumentTermMatrix}.
#' The documents are expected to have ID's. If \code{is.null(names(lsaObj$Documents))}
#' then the ID's are just the indexes of the documents list/vector.
#' @export
LSAMonMakeDocumentTermMatrix <- function( lsaObj, splitPattern = "\\W", stemWordsQ = FALSE, stopWords = NULL ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  documents <- lsaObj %>% LSAMonTakeDocuments()

  if( is.null(documents) ) {
    warning("Assign documents first. (Using LSAMonSetDocuments.)", call. = TRUE )
    return(LSAMonFailureSymbol)
  }

  if( !( is.character(documents) || is.vector(documents) ) ) {
    warning("lsaObj$Documents is expected to be a character vector or a list of strings.", call. = TRUE )
    return(LSAMonFailureSymbol)
  }

  if( !is.character(splitPattern) ) {
    warning("The argument splitPattern is expected to be a string.", call. = TRUE )
    return(LSAMonFailureSymbol)
  }

  if( is.null(stemWordsQ) ) {
    stemWordsQ <- FALSE
  }

  if( !( is.null(stopWords) || is.character(stopWords) ) ) {
    warning("The argument stopWords is expected to be a character vector or NULL.", call. = TRUE )
    return(LSAMonFailureSymbol)
  }

  if( is.list(documents) ) {
    documents <- as.character(documents)
  }

  docIDs <- names(documents)
  if( is.null(docIDs) ) {
    docIDs <- as.character( 1:length(documents))
  }

  ## Split the documents into words
  ss <- setNames( strsplit( documents, split = splitPattern ), docIDs )

  ## Remove empty documents
  ss <- ss[ purrr::map_int(ss, length) > 0 ]

  ## Remove empty words
  ss <- purrr::map( ss, function(x) x[ nchar(x)>0 ] )

  ## Remove stop words
  if ( !is.null(stopWords) ) {
    stopWords <- tolower(stopWords)
    ss <- purrr::map( ss, function(x) setdiff( tolower(x),  stopWords ) )
  }
  ss <- ss[ purrr::map_int(ss, length) > 0 ]

  ## Convert all words to lower case and stem
  if ( stemWordsQ ) {
    ss <- purrr::map( ss, function(x) SnowballC::wordStem( tolower(x) ) )
  } else {
    ss <- purrr::map( ss, function(x) tolower(x) )
  }
  ss <- ss[ purrr::map_int(ss, length) > 0 ]

  ## Make document-term contingency matrix
  ssDF <-
    purrr::map_df( 1:length(ss),
                   function(i) {
                     data.frame( ID = names(ss)[i], Term = ss[[i]], stringsAsFactors = FALSE )
                   })

  dtMat <- xtabs( formula = ~ ID + Term, ssDF, sparse = TRUE )

  lsaObj$Documents <- documents
  lsaObj$DocumentTermMatrix <- dtMat

  lsaObj
}


##===========================================================
## Apply term-weight functions
##===========================================================

#' Apply term-weight functions.
#' @description Re-weight the entries of the document-term matrix.
#' @param lsaObj A LSAMon object.
#' @param globalWeightFunction Global weight function.
#' @param localWeightFunction Local weight function.
#' @param normalizerFunction Normalizer function.
#' @return A LSAMon object.
#' @details This function simply calls
#' \code{\link{SparseMatrixRecommender::SMRApplyTermWeightFunctions}} for the
#' document-term matrix \code{lsaObj$DocumentTermMatrix}.
#' The obtained matrix is assigned to \code{lsaObj$WeightedDocumentTermMatrix}.
#' @export
LSAMonApplyTermWeightFunctions <- function( lsaObj, globalWeightFunction = "Entropy", localWeightFunction = "None", normalizerFunction = "Cosine" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  docTermMat <- lsaObj %>% LSAMonTakeDocumentTermMatrix()

  if( is.null(docTermMat) ) {
    warning("Create the document-term matrix first.", call. = TRUE )
    return(LSAMonFailureSymbol)
  }

  wDocTermMat <-
    SparseMatrixRecommender::SMRApplyTermWeightFunctions( docTermMat = docTermMat,
                                                          globalWeightFunction = globalWeightFunction,
                                                          localWeightFunction = localWeightFunction,
                                                          normalizerFunction = normalizerFunction )

  lsaObj$WeightedDocumentTermMatrix <- wDocTermMat

  lsaObj
}


##===========================================================
## TopicExtraction
##===========================================================

# @param numberInitializingDocuments The number of documents to be used
# for (random) initialization of the left factor (\code{W}.)

#' Topic extraction.
#' @description Topic extraction from a document-term matrix using
#' Non Negative Matrix Factorization (NNMF).
#' @param lsaObj A LSAMon object.
#' @param numberOfTopics Number of topics to be extracted.
#' @param minNumberOfDocumentsPerTerm Minimal number of documents for the terms
#' to be considered in the topics.
#' @param maxSteps
#' @return A LSAMon object.
#' @details The obtained factor matrices are assigned to \code{lsaObj$W} and \code{lsaObj$H}.
#' The parameters \code{maxSteps, tolerance, profiling} are
#' for the function \code{\link{NonNegativeMatrixFactorization::NNMF}}.
#' If \code{is.null(minNumberOfDocumentsPerTerm)} then the following formula is used:
#' \code{minNumberOfDocumentsPerTerm = floor(0.05*nrow(lsaObj$DocumentTermMatrix))}.
#' @export
LSAMonTopicExtraction <- function( lsaObj, numberOfTopics, minNumberOfDocumentsPerTerm = NULL,
                                   maxSteps = 12, tolerance = 0.01, profiling = TRUE) {


  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  wDocTermMat <- lsaObj %>% LSAMonTakeWeightedDocumentTermMatrix()

  if( is.null(wDocTermMat) ) {
    return(
      lsaObj %>%
        LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                        localWeightFunction = "None",
                                        normalizerFunction = "Cosine" ) %>%
        LSAMonTopicExtraction( numberOfTopics = numberOfTopics,
                               minNumberOfDocumentsPerTerm = minNumberOfDocumentsPerTerm,
                               maxSteps = maxSteps,
                               tolerance = tolerance,
                               profiling = profiling )
    )
  }

  if( !( is.numeric(numberOfTopics) && numberOfTopics > 0 ) ) {
    warning( "The argument numberOfTopics is expected to be a positive integer.", call. = T)
    return(LSAMonFailureSymbol)
  }

  if( is.null(minNumberOfDocumentsPerTerm) ) {
    minNumberOfDocumentsPerTerm <- floor( 0.05 * nrow(wDocTermMat) )
  }

  if( !( is.numeric(minNumberOfDocumentsPerTerm) && numberOfTopics >= 0 ) ) {
    warning( "The argument minNumberOfDocumentsPerTerm is expected to be a non-negative integer or NULL.", call. = T)
    return(LSAMonFailureSymbol)
  }

  wDocTermMat01 <- wDocTermMat
  wDocTermMat01@x[wDocTermMat01@x > 0] <- 1

  wDocTermMat <- wDocTermMat[ , Matrix::colSums(wDocTermMat01) >= minNumberOfDocumentsPerTerm ]

  resNNMF <-
    NonNegativeMatrixFactorization::NNMF( V = wDocTermMat,
                                          k = as.integer(numberOfTopics),
                                          maxSteps = maxSteps,
                                          tolerance = tolerance,
                                          profiling = profiling )

  lsaObj$W <- resNNMF$W
  lsaObj$H <- resNNMF$H

  lsaObj
}


##===========================================================
## Basis vector interpretation
##===========================================================

#' Basis vectors interpretation.
#' @description Interpret specified basis vectors.
#' @param lsaObj A LSAMon object.
#' @param basisVectorIndexes Basis vectors to be interpretted.
#' If NULL all indexes are taken.
#' @param n Number of (top) coordinates per basis vector.
#' @param orderBySignificanceQ Should the basis vectors be ordered by their significance?
#' @return A LSAMon object.
#' @details This function is based on
#' \code{\link{NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct} and
#' \code{\link{NonNegativeMatrixFactorization::NNMFBasisVectorInterpretation}}.
#' The obtained list of data frames is assigned to \code{lsaObj$Value}.
#' @export
LSAMonBasisVectorInterpretation <- function( lsaObj, vectorIndices = NULL, n = 12, orderBySignificanceQ = TRUE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj, memberName = "H", functionName = "LSAMonBasisVectorInterpretation", logicalResult = T ) ) {
    return(LSAMonFailureSymbol)
  }

  if( is.null(vectorIndices) ){
    vectorIndices <- 1:nrow(lsaObj$H)
  }

  if( !( is.numeric(vectorIndices) && mean( vectorIndices > 0 ) == 1 ) ) {
    warning( "The argument vectorIndices is expected to be a vector of non-negative integers or NULL.", call. = T)
    return(LSAMonFailureSymbol)
  }

  nres <- NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct( lsaObj$W, lsaObj$H, normalizeLeft = FALSE )

  topicSFactors <- sqrt( colSums( nres$W * nres$W ) )

  if( orderBySignificanceQ ) {
    nres$W <- nres$W[ , rev(order(topicSFactors)) ]
    nres$H <- nres$H[ rev(order(topicSFactors)), ]
    topicSFactors <- topicSFactors[ rev(order(topicSFactors)) ]
  }

  topics <-
    purrr::map_df( vectorIndices, function(i) {

      basisVec <- NonNegativeMatrixFactorization::NNMFBasisVectorInterpretation( nres$H[i,], n, colnames(nres$H) )

      data.frame( Rank = i, SignificanceFactor = topicSFactors[i],
                  Term = names(basisVec), Coefficient = basisVec,
                  stringsAsFactors = F)
    })

  lsaObj$Value <- topics

  lsaObj
}

##===========================================================
## Statistical thesauri
##===========================================================

#' Statistical thesaurus derivation.
#' @description Derive a statistical thesaurus for specified words.
#' @param lsaObj A LSAMon object.
#' @param searchWords A character vector with words to to find statistical
#' tesauri for. The words can be patterns.
#' @param n Number of words in each thesaurus entry.
#' @param fixed Should \code{searchWords} be considered fixed or pattern strings?
#' @return A LSAMon object.
#' @details This function calls
#' \code{\link{NonNegativeMatrixFactorization::NearestWords}}.
#' The obtained list of thesaurus entries is assigned to \code{lsaObj$Value}.
#' @export
LSAMonStatisticalThesaurus <- function( lsaObj, searchWords, n = 12, fixed = TRUE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }


  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj, memberName = "H", functionName = "LSAMonBasisVectorInterpretation", logicalResult = T ) ) {
    return(LSAMonFailureSymbol)
  }

  res <-
    purrr::map(
      searchWords,
      function(word) {

        if ( word %in% colnames(lsaObj$H) ) {
          cbind( SearchTerm = word,
                 Word = NonNegativeMatrixFactorization::NearestWords( lsaObj$H, word, fixed = fixed, n = n ),
                 stringsAsFactors = FALSE )
        } else { NULL }
      })
  names(res) <- searchWords

  lsaObj$Value <- res

  lsaObj
}


