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
#' @param documents A character vector, a list of strings, or NULL.
#' @return An S3 class "LSAMon". In other words, a list with the attribute "class" set to "LSAMon".
#' @export
LSAMonUnit <- function( documents = NULL ) {

  res <- list( Value = NULL, Documents = NULL, DocumentTermMatrix = NULL, Terms = NULL, WeightedDocumentTermMatrix = NULL, W = NULL, H = NULL, TopicColumnPositions = NULL, AutomaticTopicNames = NULL )
  attr(res, "class") <- "LSAMon"

  if( !is.null(documents) ) {
    res <- res %>% LSAMonSetDocuments( documents = documents )
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

#' Print monad's value.
#' @description Prints the"Value" element/member of the monad object.
#' @param lsaObj An LSAMon object.
#' @return A LSAMon object.
#' @details Prints \code{flsaObj$Value}.
#' @export
LSAMonEchoValue <- function( lsaObj ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  print( lsaObj$Value )

  lsaObj
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
#' @param documents A list of documents.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetDocuments <- function( lsaObj, documents ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(documents) || is.character(documents)) ) {
    warning("The argument Documents is expected to be NULL or a character.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  if( is.null( names(documents) ) ) {
      documents <- setNames( documents, paste( "id", 1:length(documents), sep = "." ) )
  }

  lsaObj$Documents <- documents

  lsaObj
}


##===========================================================
## DocumentTermMatrix setter
##===========================================================

#' Set DocumentTermMatrix.
#' @description Sets DocumentTermMatrix into the monad object.
#' @param lsaObj An LSAMon object.
#' @param documentTermMatrix A document term sparse matrix.
#' (Of type \code{dgCMatrix}.)
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetDocumentTermMatrix <- function( lsaObj, documentTermMatrix ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(documentTermMatrix) || ( "dgCMatrix" %in% class(documentTermMatrix) ) ) ) {
    warning("The argument documentTermMatrix is expected to be NULL or a sparse matrix of type 'dgCMatrix'.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$DocumentTermMatrix <- documentTermMatrix

  lsaObj
}

##===========================================================
## Terms setter
##===========================================================

#' Set Terms.
#' @description Sets Terms into the monad object.
#' @param lsaObj An LSAMon object.
#' @param terms A list of terms.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetTerms <- function( lsaObj, terms ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(terms) || is.character(terms)) ) {
    warning("The argument terms is expected to be NULL or a character.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$Terms <- terms

  lsaObj
}

##===========================================================
## WeightedDocumentTermMatrix setter
##===========================================================

#' Set WeightedDocumentTermMatrix.
#' @description Sets WeightedDocumentTermMatrix into the monad object.
#' @param lsaObj An LSAMon object.
#' @param weightedDocumentTermMatrix A weighted document term sparse matrix.
#' (Of type \code{dgCMatrix}.)
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetWeightedDocumentTermMatrix <- function( lsaObj, weightedDocumentTermMatrix ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(weightedDocumentTermMatrix) || ( "dgCMatrix" %in% class(weightedDocumentTermMatrix) ) ) ) {
    warning("The argument weightedDocumentTermMatrix is expected to be NULL or a sparse matrix of type 'dgCMatrix'.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$WeightedDocumentTermMatrix <- weightedDocumentTermMatrix

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
#' @param topicColumnPositions A list of integer vectors.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetTopicColumnPositions <- function( lsaObj, topicColumnPositions ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(topicColumnPositions) || is.integer(topicColumnPositions)) ) {
    warning("The argument topicColumnPositions is expected to be NULL or a integer.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  lsaObj$TopicColumnPositions <- topicColumnPositions

  lsaObj
}

##===========================================================
## TopicNames setter
##===========================================================

#' Set AutomaticTopicNames.
#' @description Sets AutomaticTopicNames into the monad object.
#' @param lsaObj An LSAMon object.
#' @param topicNames A list of topic names.
#' @return An LSAMon object.
#' @family Set/Take functions
#' @export
LSAMonSetTopicNames <- function( lsaObj, topicNames ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !( is.null(topicNames) || is.character(topicNames) ) ) {
    warning("The argument topicNames is expected to be NULL or a character.", call. = TRUE)
    return(LSAMonFailureSymbol)
  }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "W", memberPrettyName = "W", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "H", memberPrettyName = "H", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  if( is.character(topicNames) ) {
    if( length(topicNames) == nrow(lsaObj$H) ) {
      colnames(lsaObj$W) <- topicNames
      rownames(lsaObj$H) <- topicNames
    } else {
      warning("If a character vector the argument topicNames is expected to have length that equals the number of rows of lsaObj$H", call. = T )
      return(LSAMonFailureSymbol)
    }
  }

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
## TopicNames Taker
##===========================================================

#' Take AutomaticTopicNames.
#' @description Takes AutomaticTopicNames from the monad object.
#' @param lsaObj An LSAMon object.
#' @param functionName A string that is a name of this function or a delegating function.
#' @return A list of functions or \code{LSAMonFailureSymbol}.
#' @family Set/Take functions
#' @export
LSAMonTakeTopicNames <- function( lsaObj, functionName = "LSAMonTakeTopicNames" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj, memberName = "H", memberPrettyName = "H", functionName = functionName,  logicalResult = TRUE) ) {
    return(LSAMonFailureSymbol)
  }

  rownames(lsaObj$H)
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
    docIDs <- as.character( 1:length(documents) )
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
  lsaObj$StopWords <- stopWords
  lsaObj$StemWordsQ <- stemWordsQ

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
LSAMonApplyTermWeightFunctions <- function( lsaObj, globalWeightFunction = "IDF", localWeightFunction = "None", normalizerFunction = "Cosine" ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  docTermMat <- lsaObj %>% LSAMonTakeDocumentTermMatrix()

  if( is.null(docTermMat) ) {
    warning("Create the document-term matrix first.", call. = TRUE )
    return(LSAMonFailureSymbol)
  }

  globalWeights <- SparseMatrixRecommender::SMRGlobalTermFunctionWeights( docTermMat = docTermMat, globalWeightFunction = globalWeightFunction )

  wDocTermMat <-
    SparseMatrixRecommender::SMRApplyTermWeightFunctions( docTermMat = docTermMat,
                                                          globalWeightFunction = globalWeights,
                                                          localWeightFunction = localWeightFunction,
                                                          normalizerFunction = normalizerFunction )

  lsaObj$WeightedDocumentTermMatrix <- wDocTermMat
  lsaObj$GlobalWeights <- globalWeights
  lsaObj$LocalWeightFunction <- localWeightFunction
  lsaObj$NormalizerFunction <- normalizerFunction

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
#' @param maxSteps Maximum iteration steps.
#' If NULL appropriate default values for the different methods are used.
#' (For NNMF 12, for SVD 1000.)
#' @param tolerance Tolerance for the relative norm difference.
#' @param profilingQ Should the computation be profiled?
#' @param orderBySignificanceQ Should the basis vectors be ordered by their significance?
#' @param automaticTopicNamesQ Should the extracted topics be given automatic names?
#' @param ... Additional parameters for the matrix decomposition functions.
#' See \code{\link{irlba::irlba}} and \code{\link{NonNegativeMatrixFactorization::NNMF}}.
#' Note that some of those overlap with some of function's arguments.
#' @return A LSAMon object.
#' @details The obtained factor matrices are assigned to \code{lsaObj$W} and \code{lsaObj$H}.
#' The parameters \code{maxSteps, tolerance, profilingQ} are
#' for the function \code{\link{NonNegativeMatrixFactorization::NNMF}}.
#' If \code{is.null(minNumberOfDocumentsPerTerm)} then the following formula is used:
#' \code{minNumberOfDocumentsPerTerm = floor(0.05*nrow(lsaObj$DocumentTermMatrix))}.
#' The automatic topic name is derived from topic's index and topic's top three words.
#' (The later is to ensure topic name uniqueness.)
#' @export
LSAMonExtractTopics <- function( lsaObj, numberOfTopics, minNumberOfDocumentsPerTerm = NULL,
                                   method = "NNMF",
                                   maxSteps = NULL, tolerance = 0.01, profilingQ = TRUE,
                                   orderBySignificanceQ = TRUE, automaticTopicNamesQ = TRUE,
                                   ...) {


  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj,
                                  memberName = "WeightedDocumentTermMatrix",
                                  functionName = "LSAMonExtractTopics",
                                  logicalResult = T) ) {

    warning( "Continuing by invoking LSAMonApplyTermWeightFunctions first.", call. = T )

    lsaObj <-
      lsaObj %>%
      LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                      localWeightFunction = "None",
                                      normalizerFunction = "Cosine" ) %>%
      LSAMonExtractTopics( numberOfTopics = numberOfTopics,
                             minNumberOfDocumentsPerTerm = minNumberOfDocumentsPerTerm,
                             method = method,
                             maxSteps = maxSteps,
                             tolerance = tolerance,
                             profilingQ = profilingQ,
                             orderBySignificanceQ = orderBySignificanceQ,
                             automaticTopicNamesQ = automaticTopicNamesQ,
                             ... )

    return(lsaObj)
  }

  wDocTermMat <- lsaObj %>% LSAMonTakeWeightedDocumentTermMatrix()

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

  ## Restrict the document-term matrix.
  wDocTermMat01 <- wDocTermMat
  wDocTermMat01@x[wDocTermMat01@x > 0] <- 1

  wDocTermMat <- wDocTermMat[ , Matrix::colSums(wDocTermMat01) >= minNumberOfDocumentsPerTerm ]

  ## Topic extraction.
  if( tolower(method) %in% tolower(c( "irlba", "SVD")) ) {

    if( is.null(maxSteps) ) { maxSteps = 1000 }

    ## Find SVD decomposition.
    resSVD <-
      irlba::irlba( A = wDocTermMat,
                    nv = as.integer(numberOfTopics),
                    maxit = maxSteps,
                    tol = tolerance,
                    ... )

    ## Re-fit the result to monad's data interpretation.
    W <- Matrix( resSVD$u, sparse = TRUE )
    S <- Diagonal( x = resSVD$d, n = length(resSVD$d) )
    H <- Matrix( t(resSVD$v), sparse = TRUE )
    H <- S %*% H

    rownames(W) <- rownames(wDocTermMat)
    colnames(H) <- colnames(wDocTermMat)

    ## Assign to lsaObj.
    lsaObj$W <- W
    lsaObj$H <- H

  } else if( tolower(method) %in% tolower(c( "NNMF", "NonNegativeMatrixfactorization")) ){

    if( is.null(maxSteps) ) { maxSteps = 12 }

    ## Find NNMF decomposition.
    resNNMF <-
      NonNegativeMatrixFactorization::NNMF( V = wDocTermMat,
                                            k = as.integer(numberOfTopics),
                                            maxSteps = maxSteps,
                                            tolerance = tolerance,
                                            profilingQ = profilingQ )

    ## Order by significance.
    if( orderBySignificanceQ ) {
      resNNMF <- NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct( resNNMF$W, resNNMF$H, normalizeLeft = FALSE )

      topicSFactors <- sqrt( colSums( resNNMF$W * resNNMF$W ) )

      if( orderBySignificanceQ ) {
        resNNMF$W <- resNNMF$W[ , rev(order(topicSFactors)) ]
        resNNMF$H <- resNNMF$H[ rev(order(topicSFactors)), ]
        topicSFactors <- topicSFactors[ rev(order(topicSFactors)) ]
      }
    }

    ## Assign to lsaObj.
    lsaObj$W <- resNNMF$W
    lsaObj$H <- resNNMF$H

  } else {

    warning( "The expected values for the argument method are 'NNMF' and 'SVD'.", call. = T )
    return(LSAMonFailureSymbol)

  }

  # Automatic topic naming.
  if( automaticTopicNamesQ ) {

    tLog <- floor(log10(nrow(lsaObj$H)))+1

    res <-
      purrr::map_chr( 1:nrow(lsaObj$H), function(i) {
        cInd <- formatC(i, width = tLog, flag = "0")
        paste( c( cInd, colnames(lsaObj$H)[ order(-lsaObj$H[i,])[ 1:min(3,ncol(lsaObj$H)) ] ] ), collapse = "." )
      })

    colnames(lsaObj$W) <- res
    rownames(lsaObj$H) <- res

  } else {

    colnames(lsaObj$W) <- 1:ncol(lsaObj$W)
    rownames(lsaObj$H) <- 1:nrow(lsaObj$H)

  }

  ## This place-holder is not used.
  lsaObj$AutomaticTopicNames <- NULL

  # assertthat::assert_that( mean( rownames(lsaObj$W) == rownames(wDocTermMat) ) == 1 )
  # assertthat::assert_that( mean( colnames(lsaObj$H) == colnames(wDocTermMat) ) == 1 )

  lsaObj
}


##===========================================================
## Take normalized matrix product components
##===========================================================

#' Take normalized matrix product components.
#' @description Normalizes the matrix factors and returns a list with them.
#' @param lsaObj A LSAMon object.
#' @param normalizeLeftQ Should the left factor be normalized?
#' @param orderBySignificanceQ Should the basis vectors be ordered by their significance?
#' @return A list of matrices
#' @export
LSAMonTakeNormalizedMatrixProductComponents <- function( lsaObj, normalizeLeftQ = TRUE, orderBySignificanceQ = FALSE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj, memberName = "H", functionName = "LSAMonTakeNormalizedMatrixProductComponents", logicalResult = T ) ) {
    return(LSAMonFailureSymbol)
  }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj, memberName = "W", functionName = "LSAMonTakeNormalizedMatrixProductComponents", logicalResult = T ) ) {
    return(LSAMonFailureSymbol)
  }

  nres <- NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct( lsaObj$W, lsaObj$H, normalizeLeftQ = normalizeLeftQ )

  if( orderBySignificanceQ ) {

    if( normalizeLeftQ ) {
      topicSFactors <- sqrt( rowSums( nres$H * nres$H ) )
    } else {
      topicSFactors <- sqrt( colSums( nres$W * nres$W ) )
    }

    nres$W <- nres$W[ , rev(order(topicSFactors)) ]
    nres$H <- nres$H[ rev(order(topicSFactors)), ]
    topicSFactors <- topicSFactors[ rev(order(topicSFactors)) ]
  }

  list( W = nres$W, H = nres$H )
}


##===========================================================
## Basis vector interpretation
##===========================================================

#' Basis vectors interpretation.
#' @description Interpret specified basis vectors.
#' @param lsaObj A LSAMon object.
#' @param basisVectorIndexes Basis vectors to be interpreted.
#' If NULL all indexes are taken.
#' @param n Number of (top) coordinates per basis vector.
#' @param orderBySignificanceQ Should the basis vectors be ordered by their significance?
#' @return A LSAMon object.
#' @details This function is based on
#' \code{\link{NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct}} and
#' \code{\link{NonNegativeMatrixFactorization::NNMFBasisVectorInterpretation}}.
#' The obtained long form data frame is assigned to \code{lsaObj$Value}.
#' Wide form can be produced with
#' \code{reshape2::dcast( data = lsaObj$Value, formula = TermRank ~ TopicRank, value.var = "Term" )}.
#' (Note that some columns are dropped.)
#' @export
LSAMonInterpretBasisVectors <- function( lsaObj, vectorIndices = NULL, n = 12, orderBySignificanceQ = TRUE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj, memberName = "H", functionName = "LSAMonInterpretBasisVectors", logicalResult = T ) ) {
    return(LSAMonFailureSymbol)
  }

  if( is.null(vectorIndices) ){
    vectorIndices <- 1:nrow(lsaObj$H)
  }

  if( !( is.numeric(vectorIndices) && mean( vectorIndices > 0 ) == 1 ) ) {
    warning( "The argument vectorIndices is expected to be a vector of non-negative integers or NULL.", call. = T)
    return(LSAMonFailureSymbol)
  }

  nres <- NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct( lsaObj$W, lsaObj$H, normalizeLeftQ = FALSE )

  ## Using these norms to order by significance is only valid only
  ## if normalizeLeftQ = FALSE in the line above.
  topicSFactors <- sqrt( colSums( nres$W * nres$W ) )

  if( orderBySignificanceQ ) {
    nres$W <- nres$W[ , rev(order(topicSFactors)) ]
    nres$H <- nres$H[ rev(order(topicSFactors)), ]
    topicSFactors <- topicSFactors[ rev(order(topicSFactors)) ]
  }

  topics <-
    purrr::map_df( vectorIndices, function(i) {

      basisVec <- NonNegativeMatrixFactorization::NNMFBasisVectorInterpretation( nres$H[i,], n, colnames(nres$H) )

      data.frame( TopicRank = i,
                  TopicName = rownames(nres$H)[[i]],
                  TopicSignificanceFactor = topicSFactors[[i]],
                  Term = names(basisVec),
                  TermCoefficient = basisVec,
                  TermRank = 1:length(basisVec),
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
#' @param searchWords A character vector with words to find statistical
#' thesauri for. The words can be patterns.
#' @param n Number of words in each thesaurus entry.
#' @param fixed Should \code{searchWords} be considered fixed or pattern strings?
#' @return A LSAMon object.
#' @details This function calls
#' \code{\link{NonNegativeMatrixFactorization::NearestWords}}.
#' The obtained list of thesaurus entries is assigned to \code{lsaObj$Value}.
#' @export
LSAMonExtractStatisticalThesaurus <- function( lsaObj, searchWords, n = 12, fixed = TRUE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }


  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj, memberName = "H", functionName = "LSAMonInterpretBasisVectors", logicalResult = T ) ) {
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


##===========================================================
## Represent by terms
##===========================================================

#' Represent by terms.
#' @description Find the terms representation of a matrix or a document.
#' @param lsaObj A LSAMon object.
#' @param query A character vector or a sparse matrix to be represented
#' in the space of monad's document-term matrix.
#' @param applyTermWeightFunctionsQ Should the weight term functions
#' be applied to the result matrix or not?
#' @return A LSAMon object.
#' @details This function applies the global weights computed in the
#' function \code{\link{LSAMonApplyTermWeightFunctions}} and stored in
#' the monad object together with the names of the local weight function,
#' and the normalizer function.
#' @export
LSAMonRepresentByTerms <- function( lsaObj, query, applyTermWeightFunctionsQ = TRUE ) {

  if( is.character(query) ) {

    qmat <-
      LSAMonUnit( query ) %>%
      LSAMonMakeDocumentTermMatrix( stemWordsQ = lsaObj$StemWordsQ, stopWords = lsaObj$StopWords ) %>%
      LSAMonTakeDocumentTermMatrix

    lsaObj %>% LSAMonRepresentByTerms( query = qmat, applyTermWeightFunctionsQ = applyTermWeightFunctionsQ )

  } else if( "dgCMatrix" %in% class(query) ) {

    qmat <- SparseMatrixRecommender::SMRImposeColumnIDs( colIDs = colnames(lsaObj %>% LSAMonTakeDocumentTermMatrix), smat = query )

    if( applyTermWeightFunctionsQ ) {

      if( length( intersect( names(lsaObj), c("GlobalWeights", "LocalWeightFunction", "NormalizerFunction" ) ) ) < 3 ) {
        warning(
          paste( "If the argument \"applyTermWeightFunctionsQ\" is TRUE",
                 "then the monad context is expected to have the elements \"GlobalWeights\", \"LocalWeightFunction\", \"NormalizerFunction\"."),
          call. = TRUE )
        return(LSAMonFailureSymbol)
      }

      qmat <- SparseMatrixRecommender::SMRApplyTermWeightFunctions( qmat, globalWeightFunction = lsaObj$GlobalWeights, lsaObj$LocalWeightFunction, lsaObj$NormalizerFunction )

    }

    if( max(abs(colSums(qmat))) == 0 ) {
      warning( "The terms of the argument query cannot be found in the document-term matrix.", call. = TRUE )
    }

    lsaObj$Value <- qmat
    lsaObj

  } else {
    warning( "Unknown type of the argument query.", call. = TRUE )
    return(LSAMonFailureSymbol)
  }
}


##===========================================================
## Topic representation
##===========================================================

#' Topic representation of document tags.
#' @description Find the topic representation of documents corresponding
#' to a list of tags. Each monad document is expected to have a tag.
#' One tag might correspond to multiple documents.
#' @param lsaObj A LSAMon object.
#' @param tags A character vector with tags that correspond to the documents.
#' The correspondence is ordinal or through the names of \code{tags}.
#' If NULL the documents ordinal numbers or ID's are used.
#' @param minThreshold The minimum absolute topic weight.
#' @param normalizeLeftQ Should the left matrix multiplication factor be normalized?
#' @return The result is a tag-topic contingency matrix that is assigned to
#' \code{lsaObj$Value}.
#' @details
#' Monad's document collection is given as a character vector \code{texts}.
#' If \code{texts} does not have element names, i.e. \code{names(texts) == NULL},
#' they are assigned to be the corresponding indexes.
#' Those element names of \code{texts} are used as document ID's.
#' The document ID's are used if \code{tags = NULL} is specified.
#' When this function is called, it is expected that the dimension reduction is
#' already computed. Hence, the rownames of the left matrix factor \code{lsaObj$W}
#' are expected to correspond to the document ID's.
#' The argument \code{tags} itself is a mapping vector: \code{names(tags)} is
#' supposed to correspond to the document ID's and the values of \code{tags} are
#' the tags to be represented with topics.
#' Using \code{tags=NULL} should produce a matrix derived from matrix
#' \code{lsaObj$W} by removing all entries from \code{lsaObj$W} that have
#' absolute values less than \code{minThreshold}.
#' Absolute values have to be considered for dimension reduction
#' algorithms as SVD and ICA.
#' @export
LSAMonRepresentDocumentTagsByTopics <- function( lsaObj, tags = NULL, minThreshold = 0.001, normalizeLeftQ = TRUE ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj,
                                  memberName = "W",
                                  functionName = "LSAMonRepresentDocumentTagsByTopics",
                                  logicalResult = T) ) {

    return(LSAMonFailureSymbol)
  }

  nres <- NonNegativeMatrixFactorization::NNMFNormalizeMatrixProduct( lsaObj$W, lsaObj$H, normalizeLeftQ = normalizeLeftQ )

  wMat <- nres$W
  wMat <- as( wMat, "sparseMatrix" )

  if( is.null(tags) ) {
    tags <- setNames( rownames(wMat), rownames(wMat) )
  }

  if( ! ( is.vector(tags) && length(tags) == nrow(wMat) ) ) {
    warning( "The argument tags is expected to be a vector with length equal to the number of documents.", call. = T )
    return(LSAMonFailureSymbol)
  }

  if( is.null( names(tags) ) ) {

    tags <- setNames( tags, rownames(wMat) )

  } else if ( length( intersect( rownames(wMat), names(tags) ) ) == 0 ) {

    warning( "None of the tags correspond to document ID's.", call. = T )
    return(LSAMonFailureSymbol)

  } else if ( length( setdiff( rownames(wMat), names(tags) ) ) > 0 ) {

    warning( "Some document ID's do not have corresponding tags.", call. = T )

  }

  dfTriplets <- setNames( SparseMatrixRecommender::SMRSparseMatrixToTriplets( wMat ), c( "DocumentIndex", "TopicIndex", "Weight" ) )

  dfTriplets <- dfTriplets[ abs(dfTriplets$Weight) >= minThreshold, ]

  dfTriplets <- cbind( dfTriplets, Tag = tags[ dfTriplets$DocumentIndex ], stringsAsFactors = FALSE )

  res <- xtabs( ~ Tag + TopicIndex, dfTriplets, sparse = T )

  lsaObj$Value <- res

  lsaObj
}


##===========================================================
## Find most important texts
##===========================================================

#' Finds most important documents.
#' @description Finds the most important documents in the monad.
#' @param lsaObj A LSAMon object
#' @param nTop The number of top most important texts.
#' @details The result is a data frame with columns \code{c("Score", "ID")};
#' it is assigned to \code{lsaObj$Value}.
#' @return A LSAMon object
#' @export
LSAMonFindMostImportantDocuments <- function( lsaObj, nTop = 5 ) {

  if( LSAMonFailureQ(lsaObj) ) { return(LSAMonFailureSymbol) }

  if( !LSAMonMemberPresenceCheck( lsaObj = lsaObj,
                                  memberName = "WeightedDocumentTermMatrix",
                                  functionName = "LSAMonRepresentDocumentTagsByTopics",
                                  logicalResult = T) ) {

    return(LSAMonFailureSymbol)
  }

  wswMat <- lsaObj %>% LSAMonTakeWeightedDocumentTermMatrix

  nTop <- min( nrow(wswMat)-1, nTop )

  ## Using Eigenvector decomposition
  # wstSMat <- wswMat %*% t(wswMat)
  # eres <- eigen( wstSMat )
  # svec <- eres$vectors[,1]

  ## Using SVD for most salient statements.
  svdRes <- irlba::irlba( A = wswMat, nv = nTop )

  svec <- svdRes$u[,1]

  inds <- rev(order(abs(svec)))[ 1 : nTop ]

  ## Final result
  res <- data.frame( Score = abs(svec)[inds], Index = inds, ID = rownames(wswMat)[inds], stringsAsFactors = FALSE )

  lsaObj$Value <- res

  lsaObj
}


##===========================================================
## Most important sentences (non-monadic)
##===========================================================

#' Most important sentences.
#' @description Finds the most important sentences in a list of sentences.
#' @param sentences A character vector with sentences.
#' @param nTopSentences The number of top most important sentences
#' to be returned.
#' @param globalTermWeightFunction The global term weight function to applied.
#' @param splitPattern A string with a regex split pattern.
#' With that pattern the sentences are split into words.
#' @param stemWordsQ Should word stemming be applied or not?
#' @param minWordLength Minimal word length.
#' @param stopWords A character vector with stop words to be removed.
#' @details The result is a data frame with columns \code{c("Score", "Sentence")}.
#' This is a stand-alone, non-monadic version of \code{\link{LSAMonFindMostImportantDocuments}}.
#' @return A data frame
#' @export
MostImportantSentences <- function( sentences,
                                    nTopSentences = 5,
                                    globalTermWeightFunction = "IDF",
                                    splitPattern = "\\W",
                                    stemWordsQ = FALSE,
                                    minWordLength = 2,
                                    stopWords = NULL ) {

  if( !is.character(sentences) ) {
    stop( "The argument sentences is expected to be a character vector.", ccall. = TRUE )
  }

  if( length(sentences) == 0 ) {

    return( NULL )

  } else if( length(sentences) == 1 ) {

    return( data.frame( Score = 1, Index = 1, ID = if( is.null( names(sentences) ) ) { "id1" } else { names(sentences) },
                        Sentence = sentences,
                        stringsAsFactors = FALSE ) )
  }

  ## Using LSAMon.
  lsaObj <-
    LSAMonUnit( sentences ) %>%
    LSAMonMakeDocumentTermMatrix( splitPattern = splitPattern, stemWordsQ = FALSE, stopWords = NULL ) %>%
    LSAMonApplyTermWeightFunctions( globalWeightFunction = globalTermWeightFunction, localWeightFunction = "None", normalizerFunction = "Cosine" ) %>%
    LSAMonFindMostImportantDocuments( nTop = nTopSentences )

  if( LSAMonFailureQ(lsaObj) ) {
    return(NULL)
  }

  res <- setNames( (lsaObj %>% LSAMonTakeDocuments )[ lsaObj %>% LSAMonTakeValue %>% dplyr::pull(ID) ], NULL )

  ## Final result
  cbind( lsaObj %>% LSAMonTakeValue, Sentence = res, stringsAsFactors = FALSE )
}

