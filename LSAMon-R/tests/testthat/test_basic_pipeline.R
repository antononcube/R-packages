context("Basic pipeline")
library(LSAMon)


lsaObj <-
  LSAMonUnit( textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, splitPattern = "\\W") %>%
  LSAMonApplyTermWeightFunctions()

## Tests
test_that("Sanity check", {
  expect_is( lsaObj, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj) ) ) == 5 )
})

dfTopic <-
  lsaObj %>%
  LSAMonTopicExtraction( numberOfTopics = 60, maxSteps = 20, profiling = FALSE ) %>%
  LSAMonBasisVectorInterpretation(vectorIndices = 30, n = 30) %>%
  LSAMonTakeValue

## Tests
test_that("Topic extraction", {
  expect_is( dfTopic, "data.frame" )
  expect_true( length( intersect( c("Rank", "SignificanceFactor", "Term", "Coefficient"), colnames(dfTopic) ) ) == 4 )
})
