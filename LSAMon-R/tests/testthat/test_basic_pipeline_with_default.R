context("Basic pipeline with default values")
library(LSAMon)

set.seed(1289)

lsaObj1 <-
  LSAMonUnit( Documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, splitPattern = "\\W") %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" ) %>%
  LSAMonTopicExtraction( numberOfTopics = 60, minNumberOfDocumentsPerTerm = NULL, maxSteps = 6, profiling = FALSE )


set.seed(1289)

lsaObj2 <-
  LSAMonUnit( textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( "\\W") %>%
  LSAMonTopicExtraction( 60, maxSteps = 6, profiling = FALSE )

## Tests
test_that("Topic extraction left factor", {
  expect_true( nrow(lsaObj1 %>% LSAMonTakeW) == nrow(lsaObj1 %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( ncol(lsaObj1 %>% LSAMonTakeW) == 60 )

  expect_true( nrow(lsaObj2 %>% LSAMonTakeW) == nrow(lsaObj2 %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( ncol(lsaObj2 %>% LSAMonTakeW) == 60 )
})

test_that("Topic extraction left factor equivalence", {
  expect_true( max( abs((lsaObj1 %>% LSAMonTakeW) - (lsaObj2 %>% LSAMonTakeW)) ) < 10E-6 )
})
