context("Topic representation tests")
library(LSAMon)

## See the details for LSAMonTopicRepresentation.
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
  LSAMonTopicRepresentation( tags = NULL, minThreshold = 0.01 ) %>%
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

