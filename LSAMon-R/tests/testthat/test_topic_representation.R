context("Topic representation tests")
library(LSAMon)

## See the details for LSAMonRepresentDocumentTagsByTopics
## The explanations there are verified below.

lsaObj <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, splitPattern = "\\W") %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" ) %>%
  LSAMonExtractTopics( numberOfTopics = 60, minNumberOfDocumentsPerTerm = 1, method = "SVD", maxSteps = 30, profiling = FALSE )

## tags = NULL tests.
## We can say that is the most "standard" usage.

repWMat <-
  lsaObj %>%
  LSAMonRepresentDocumentTagsByTopics( tags = NULL, minThreshold = 0.01 ) %>%
  LSAMonTakeValue()

test_that("Topic representation for document ID's", {

  expect_true( nrow(lsaObj %>% LSAMonTakeW) == nrow(lsaObj %>% LSAMonTakeDocumentTermMatrix) )

  expect_true( nrow(lsaObj %>% LSAMonTakeW) == nrow(repWMat) )
  expect_true( ncol(lsaObj %>% LSAMonTakeW) == ncol(repWMat) )

  expect_true( mean( rownames(lsaObj %>% LSAMonTakeW) == rownames(repWMat) ) == 1 )

  expect_true( length((lsaObj %>% LSAMonTakeW)@x) > length(repWMat@x) )
  expect_true( mean( abs(repWMat@x) >= 0.01 ) == 1 )

})

## Mapping of tags when names(tags) == NULL .
## TBD...

## Mapping of tags when names(tags) == rownames(lsaObj %>% LSAMonTakeW)
## TBD...


## Test different topic extraction methods.

lsaObjSVD <-
  LSAMonUnit( textHamlet   ) %>%
  LSAMonMakeDocumentTermMatrix( stemWordsQ = TRUE, stopWords = NULL, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( "IDF", "None", "Cosine" )

lsaObjSVD <-
  lsaObjSVD %>%
  LSAMonExtractTopics( numberOfTopics = 10, minNumberOfDocumentsPerTerm = 5, maxSteps = 20, method = "SVD")

lsaObjNNMF <-
  lsaObjSVD %>%
  LSAMonExtractTopics( numberOfTopics = 10, minNumberOfDocumentsPerTerm = 5, maxSteps = 20, method = "NNMF", profilingQ = FALSE)

test_that("Topic representation by different methods", {

  query1 <-
    lsaObjSVD %>%
    LSAMonRepresentByTopics( "the night is coming again", method = "Algebraic") %>%
    LSAMonTakeValue
  query1 <- colSums(query1)[ order(-colSums(query1))]

  query2 <-
    lsaObjSVD %>%
    LSAMonRepresentByTopics( "the night is coming again", method = "Recommendation") %>%
    LSAMonTakeValue
  query2 <- colSums(query2)[ order(-colSums(query2))]

  expect_equal( query1, query2 )


  query3 <-
    lsaObjNNMF %>%
    LSAMonRepresentByTopics( "the night is coming again", method = "Algebraic") %>%
    LSAMonTakeValue
  query3 <- colSums(query3)[ order(-colSums(query3))]

  query4 <-
    lsaObjNNMF %>%
    LSAMonRepresentByTopics( "the night is coming again", method = "Recommendation") %>%
    LSAMonTakeValue
  query4 <- colSums(query4)[ order(-colSums(query4))]

  expect_equal( sort(names(query3)[1:2]), sort(names(query4)[1:2]) )

})
