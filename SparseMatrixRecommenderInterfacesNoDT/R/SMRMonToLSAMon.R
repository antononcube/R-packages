
#' @import Matrix
#' @import magrittr
#' @import dplyr
#' @import SparseMatrixRecommender
#' @import SMRMon
#' @import LSAMon
NULL

#' SMRMon to LSAMon
#' @description Make an LSAMon object from an SMRMon object.
#' @param smr A SMRMon object.
#' @param splitPattern Pattern to use for splitting the documents into words.
#' If NULL \code{"[[:space:]]|[[:punct:]]"} is used.
#' @param stemWordsQ Should the words be stemmed or not?
#' If \code{stemWordsQ = TRUE} and \code{stemRules = NULL}
#' then \code{\link{SnowballC::wordStem}} is used.
#' @param stemRules A character vector with named elements or NULL.
#' (A vector of "stemming rules".)
#' If \code{stemWordsQ = FALSE} then \code{stemRules} is ignored.
#' @param stopWords A character vector with stop words to be removed.
#' @param numberOfTopics Number of topics to be extracted.
#' @param minNumberOfDocumentsPerTerm Minimal number of documents for the terms
#' to be considered in the topics.
#' @param sep Separator of tag type and tag in the columns \code{smr$M}.
#' @details Takes as arguments the arguments of \code{\link{LSAMonMakeDocumentTermMatrix}}
#' and \code{\link{LSAMonExtractTopics}}.
#' @return A LSAMon object
#' @export
SMRMonToLSAMon <- function(smr,
                           splitPattern = "[[:space:]]|[[:punct:]]",
                           stemWordsQ = FALSE,
                           stemRules = NULL,
                           stopWords = NULL,
                           numberOfTopics = 60,
                           minNumberOfDocumentsPerTerm = 0,
                           sep = ":") {

  dfSMat <- smr %>% SMRMonGetLongFormData %>% SMRMonTakeValue

  dfSMat <- setNames(dfSMat, c("Item", "TagType", "Tag", "Value"))

  dfSMat <-
    dfSMat %>%
    dplyr::mutate( Tag = gsub(paste0("^", TagType[[1]], sep), "", Tag) )

  dfTexts <-
    dfSMat %>%
    dplyr::group_by(Item) %>%
    dplyr::summarize( Text = paste(Tag, collapse = " "), .groups = "drop_last")

  aDocs <- setNames(dfTexts$Text, dfTexts$Item)

  lsaObj <-
    LSAMonUnit(aDocs) %>%
    LSAMonMakeDocumentTermMatrix(splitPattern = splitPattern,
                                 stemWordsQ = stemWordsQ,
                                 stemRules = stemRules,
                                 stopWords = stopWords ) %>%
    LSAMonApplyTermWeightFunctions(globalWeightFunction = "IDF",
                                   localWeightFunction = "None",
                                   normalizerFunction = "Cosine") %>%
    LSAMonExtractTopics( numberOfTopics = numberOfTopics,
                         method = "SVD",
                         minNumberOfDocumentsPerTerm = minNumberOfDocumentsPerTerm)

  lsaObj
}
