context("Stemming")
library(LSAMon)

## Note that with this code lines and corresponding test below
## we test that the monad (function LSAMonMakeDocumentTermMatrix)
## converts the text(s) to lower case and removes empty words.
lsWords <- unique(c(unlist(strsplit(tolower(textHamlet), "[[:space:]]|[[:punct:]]"))))
lsWords <- lsWords[ nchar(lsWords) > 0 ]

## Stems
lsStems <- SnowballC::wordStem(lsWords)

lsNotStemmed <- lsStems[ lsStems == lsWords ]

lsStemmed <- lsStems[ lsStems != lsWords ]
lsStemmed <- lsStemmed[ nchar(lsStemmed) > 0 ]

## Stemming rules
lsStemRules <-setNames(lsStems, lsWords)

## Expected column names
lsExpectedColnames <- lsStemRules[lsWords]
lsExpectedColnames <- unique(lsExpectedColnames[ nchar(lsExpectedColnames) > 0 ])


## No stemming specs
lsaObj0 <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, stemRules = NULL, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" )

## Automatic stemming spec
lsaObj1 <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = TRUE, stemRules = NULL, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" )


## Stemming with stemming rules spec
lsaObj2 <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = TRUE, stemRules = lsStemRules, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" )

## Do not stem and stemming rules spec
lsaObj3 <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, stemRules = lsStemRules, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" )



## Tests
test_that("Sanity checks", {

  expect_is( lsaObj0, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj0) ) ) == 5 )

  expect_is( lsaObj1, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj1) ) ) == 5 )

  expect_is( lsaObj2, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj2) ) ) == 5 )

  expect_is( lsaObj3, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj3) ) ) == 5 )

})



test_that("No stemming document-term matrix check", {
  expect_true( length(lsWords) == ncol(lsaObj0 %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( mean( lsWords %in% colnames(lsaObj0 %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( grepl( "[[:punct:]]", colnames(lsaObj0 %>% LSAMonTakeDocumentTermMatrix) ) ) == 0 )
  expect_true( mean( nchar( colnames(lsaObj0 %>% LSAMonTakeDocumentTermMatrix) ) > 0 ) == 1 )
})


test_that("Automatic stemming document-term matrix check", {
  expect_true( length(lsExpectedColnames) == ncol(lsaObj1 %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( mean( lsStemmed %in% colnames(lsaObj1 %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( lsNotStemmed %in% colnames(lsaObj1 %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( grepl( "[[:punct:]]", colnames(lsaObj1 %>% LSAMonTakeDocumentTermMatrix) ) ) == 0 )
  expect_true( mean( nchar( colnames(lsaObj1 %>% LSAMonTakeDocumentTermMatrix) ) > 0 ) == 1 )
})


test_that("Stemming with stemming rules spec document-term matrix check", {
  expect_true( length(lsExpectedColnames) == ncol(lsaObj2 %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( mean( lsStemmed %in% colnames(lsaObj2 %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( lsNotStemmed %in% colnames(lsaObj2 %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( grepl( "[[:punct:]]", colnames(lsaObj2 %>% LSAMonTakeDocumentTermMatrix) ) ) == 0 )
  expect_true( mean( nchar( colnames(lsaObj2 %>% LSAMonTakeDocumentTermMatrix) ) > 0 ) == 1 )
})


test_that("Do not stem and stemming rules document-term matrix check", {
  expect_true( length(lsWords) == ncol(lsaObj3 %>% LSAMonTakeDocumentTermMatrix) )
  expect_true( mean( lsWords %in% colnames(lsaObj3 %>% LSAMonTakeDocumentTermMatrix) ) == 1 )
  expect_true( mean( grepl( "[[:punct:]]", colnames(lsaObj3 %>% LSAMonTakeDocumentTermMatrix) ) ) == 0 )
  expect_true( mean( nchar( colnames(lsaObj3 %>% LSAMonTakeDocumentTermMatrix) ) > 0 ) == 1 )
})

