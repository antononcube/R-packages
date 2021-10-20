context("Sparse matrix recommender export and import")
library(SparseMatrixRecommender)

## When creating the submatrices SMRCreateFromSpecificaiton
## sorts the obtained names (because it uses split.)
sMatNames <- sort(colnames(dfTitanic)[-1])

## Create SMR directly from data

smr <- SMRCreate( dfTitanic[, c("id", sMatNames)], 
                  tagTypes = sMatNames, 
                  itemColumnName = "id", 
                  addTagTypesToColumnNamesQ = TRUE,
                  sep = ":" )

## Tests
##-----------------------------------------------------------

test_that("Expected SMR object", {
  expect_s3_class( smr, "SMR" )
  
  expect_equal( names(smr), c("M", "M01", "TagTypeRanges","TagTypes",      
                              "ItemColumnName", "TagToIndexRules", "ItemToIndexRules") )
  
  expect_is( smr$M, "dgCMatrix" ) 
  
}) 

##-----------------------------------------------------------

test_that("Export as CSV-HarwellBoeing", {
  
  dirName <- tempdir()
  
  prefix1 <- "Titanic"
  infix1 <- "SomeNew"
  
  SMRExportToDirectory( smr = smr, directoryPath = dirName, dataNamePrefix = "Titanic", dataNameInfix = infix1, format = "CSVHarwellBoeing", digits = 4 )
  
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-", infix1, ".mm")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-rownames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-colnames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-TagTypeRanges-", infix1, ".csv")) ) )
  
}) 

test_that("Import from CSVHarwellBoeing files", {
  
  dirName <- tempdir()
  
  prefix1 <- "Titanic"
  infix1 <- "SomeNew"
  
  smrNew1 <- SMRImportFromDirectory( directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "CSVHarwellBoeing" )
  
  expect_equal( norm(smr$M - smrNew1$M[, colnames(smr$M)]), 0)
  
}) 

##-----------------------------------------------------------

test_that("Export to CSV files", {
  
  dirName <- tempdir()
  
  prefix1 <- "Titanic2"
  infix1 <- "SomeNew2"
  
  SMRExportToDirectory( smr = smr, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "CSV", digits = 4 )
  
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-rownames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-colnames-", infix1, ".csv")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-TagTypeRanges-", infix1, ".csv")) ) )
  
}) 

test_that("Import from CSV files", {
  
  dirName <- tempdir()
  
  prefix1 <- "Titanic2"
  infix1 <- "SomeNew2"
  
  smrNew2 <- SMRImportFromDirectory( directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "CSV" )
  
  expect_equal( norm(smr$M - smrNew2$M[, colnames(smr$M)]), 0)
  
}) 

##-----------------------------------------------------------

test_that("Export to feather files", {
  
  dirName <- tempdir()
  
  prefix1 <- "Titanic3"
  infix1 <- "SomeNew3"
  
  SMRExportToDirectory( smr = smr, directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "feather", digits = 4 )
  

  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-rownames-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-M01-colnames-", infix1, ".feather")) ) )
  expect_true( file.exists( file.path( dirName, paste0(prefix1, "-SMR-TagTypeRanges-", infix1, ".feather")) ) )
  
}) 


test_that("Import from feather files", {
  
  dirName <- tempdir()
  
  prefix1 <- "Titanic3"
  infix1 <- "SomeNew3"
  
  smrNew3 <- SMRImportFromDirectory( directoryPath = dirName, dataNamePrefix = prefix1, dataNameInfix = infix1, format = "feather" )
  
  expect_equal( norm(smr$M - smrNew3$M[, colnames(smr$M)]), 0)
  
}) 

