context("LSA object export and import")
library(SparseMatrixRecommender)

lsaObj0 <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, splitPattern = NULL )

lsWords <- colnames(lsaObj0 %>% LSAMonTakeDocumentTermMatrix)
aStemRules <- setNames( SnowballC::wordStem(lsWords), lsWords)

lsaObj <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = TRUE, stemRules = aStemRules, splitPattern = NULL ) %>%
  LSAMonApplyTermWeightFunctions( globalWeightFunction = "IDF",
                                  localWeightFunction = "None",
                                  normalizerFunction = "Cosine" ) %>%
  LSAMonExtractTopics( numberOfTopics = 12, minNumberOfDocumentsPerTerm = 2, method = "SVD", maxSteps = 30 )


## Tests
##-----------------------------------------------------------

test_that("Expected object", {
  expect_is( lsaObj, "LSAMon" )
  expect_true( length( intersect( c("Value", "Documents", "DocumentTermMatrix", "W", "H"), names(lsaObj) ) ) == 5 )
})


##-----------------------------------------------------------

test_that("Export as CSV-HarwellBoeing", {

  dirName <- tempdir()

  prefix1 <- "Hamlet"
  infix1 <- "SomeNew"

  lsaObj <- LSAMonExportToDirectory( lsaObj = lsaObj, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "CSVHarwellBoeing")

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-", infix1, ".mm")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-rownames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-colnames-", infix1, ".csv")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-", infix1, ".mm")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-rownames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-colnames-", infix1, ".csv")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-GlobalWeights-", infix1, ".csv")) ) )

})

test_that("Import from CSV-HarwellBoeing format", {

  dirName <- tempdir()

  prefix1 <- "Hamlet"
  infix1 <- "SomeNew"

  lsaObj2 <- LSAMonImportFromDirectory( lsaObj = NULL, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "CSVHarwellBoeing" )

  expect_equal( Matrix::norm(lsaObj$DocumentTermMatrix - lsaObj2$DocumentTermMatrix[, colnames(lsaObj$DocumentTermMatrix)], type = "1"), 0)

  #expect_true( norm(lsaObj$H - lsaObj2$H[, colnames(lsaObj$H)], "1") / norm(lsaObj$H, "1") < 0.2 )
})


##-----------------------------------------------------------

test_that("Export as feather", {

  dirName <- tempdir()

  prefix1 <- "Hamlet23232"
  infix1 <- "SomeNew232"

  lsaObj <- LSAMonExportToDirectory( lsaObj = lsaObj, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "feather")

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-rownames-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-colnames-", infix1, ".feather")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-rownames-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-colnames-", infix1, ".feather")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-GlobalWeights-", infix1, ".feather")) ) )

})



test_that("Import from feather format", {

  dirName <- tempdir()

  prefix1 <- "Hamlet23232"
  infix1 <- "SomeNew232"

  lsaObj3 <- LSAMonImportFromDirectory( lsaObj = NULL, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "feather" )

  expect_equal( Matrix::norm(lsaObj$DocumentTermMatrix - lsaObj3$DocumentTermMatrix[, colnames(lsaObj$DocumentTermMatrix)], type = "1"), 0)

  #expect_true( norm(lsaObj$H - lsaObj3$H[, colnames(lsaObj$H)]) / norm(lsaObj$H) < 0.2 )
})
