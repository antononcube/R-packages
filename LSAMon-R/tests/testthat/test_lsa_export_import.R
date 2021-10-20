context("LSA object export and import")
library(SparseMatrixRecommender)

lsaObj <-
  LSAMonUnit( documents = textHamlet ) %>%
  LSAMonMakeDocumentTermMatrix( stopWords = NULL, stemWordsQ = FALSE, splitPattern = NULL ) %>%
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

  lsaObj <- LSAMonExportToDirectory( lsaObj = lsaObj, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "CSVHarwellBoeing", digits = 4 )

  list.files( path = dirName, pattern = "*")
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-", infix1, ".mm")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-rownames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-colnames-", infix1, ".csv")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-", infix1, ".mm")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-rownames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-colnames-", infix1, ".csv")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-GlobalWeights-", infix1, ".csv")) ) )

})


##-----------------------------------------------------------

test_that("Export as feather", {

  dirName <- tempdir()

  prefix1 <- "Hamlet23232"
  infix1 <- "SomeNew232"

  lsaObj <- LSAMonExportToDirectory( lsaObj = lsaObj, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "feather", digits = 4 )

  list.files( path = dirName, pattern = "*")
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-rownames-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-DocumentTermMatrix-colnames-", infix1, ".feather")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-rownames-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-TopicMatrix-colnames-", infix1, ".feather")) ) )

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-LSAMon-GlobalWeights-", infix1, ".feather")) ) )

})
