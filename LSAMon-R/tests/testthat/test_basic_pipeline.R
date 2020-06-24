context("Basic pipeline")
library(LSAMon)

lsaObj <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" )

## Tests
test_that("Sanity check", {
  expect_is( lsaObj, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj) ) ) == 5 )
})

## Note that with this code lines and corresponding test below
## we test that the monad (function LSAMonMakeDocumentTermMatrix)
## converts the text(s) to lower case and removes empty words.
words <- unique(c(unlist(strsplit(tolower(textHamlet), "[[:space:]]|[[:punct:]]"))))
words <- words[ nchar(words) > 0 ]

test_that("Document-term matrix check", {
  expect_true( length(words) == ncol(lsaObj %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( mean( words %in% colnames(lsaObj %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( grepl( "[[:punct:]]", colnames(lsaObj %>% LSAMonTakeDocumentTermMatrix) ) ) == 0 )
  expect_true( mean( nchar( colnames(lsaObj %>% LSAMonTakeDocumentTermMatrix) ) > 0 ) == 1 )
})

## Topic extraction
lsaObj <-
  lsaObj %>%
  LSAMonExtractTopics( numberOfTopics = 60, minNumberOfDocumentsPerTerm = 1, numberOfInitializingDocuments = 12,
                       maxSteps = 6, profiling = FALSE, initialTopics = NULL )

dfTopic <-
  lsaObj %>%
  LSAMonInterpretBasisVectors( basisVectorIndexes = 30, n = 30) %>%
  LSAMonTakeValue

## Tests
test_that("Topic extraction", {
  expect_true( nrow(lsaObj %>% LSAMonTakeW) == nrow(lsaObj %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( ncol(lsaObj %>% LSAMonTakeW) == 60 )

  expect_true( nrow(lsaObj %>% LSAMonTakeH) == 60 )
  expect_true( ncol(lsaObj %>% LSAMonTakeH) == ncol(lsaObj %>% LSAMonTakeDocumentTermMatrix) )

  expect_is( dfTopic, "data.frame" )
  expect_true( length( intersect( c("TopicRank", "TopicSignificanceFactor", "Term", "TermCoefficient", "TermRank"), colnames(dfTopic) ) ) == 5 )
})
