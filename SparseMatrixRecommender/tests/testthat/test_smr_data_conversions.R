context("Sparse matrix recommender data conversions")
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

test_that("Expected SMR object", {
  expect_s3_class( smr, "SMR" )
  
  expect_equal( names(smr), c("M", "M01", "TagTypeRanges","TagTypes",      
                              "ItemColumnName", "TagToIndexRules", "ItemToIndexRules") )
  
  expect_is( smr$M, "dgCMatrix" ) 
  
}) 

dfLongForm <- SMRMatricesToLongForm(smr, tagTypes = NULL, removeTagTypePrefixesQ = FALSE )

test_that("Expected long form shape", {
  expect_s3_class( object = dfLongForm, class = "data.frame" )
  expect_equal( colnames(dfLongForm), c( smr$ItemColumnName, "Value", "Weight", "TagType" ) )
  expect_equal( nrow(dfLongForm), nrow(dfTitanic) * (ncol(dfTitanic) - 1 ) )
}) 

dfLongForm <- SMRMatricesToLongForm(smr, tagTypes = NULL, removeTagTypePrefixesQ = TRUE )

test_that("Expected long form shape for removed tag type prefixes", {
  expect_s3_class( object = dfLongForm, class = "data.frame" )
  expect_equal( colnames(dfLongForm), c( smr$ItemColumnName, "Value", "Weight", "TagType" ) )
  expect_equal( nrow(dfLongForm), nrow(dfTitanic) * (ncol(dfTitanic) - 1 ) )
}) 

test_that("No tag type prefixes in the long form", {
  expect_equal( sort(unique(unlist(as.list(dfTitanic[,2:5])))), sort(unique(dfLongForm$Value)) )
}) 


dfWideForm <- SMRMatricesToWideForm(smr, tagTypes = NULL, removeTagTypePrefixesQ = TRUE )

test_that("Expected wide form shape for removed tag type prefixes", {
  expect_s3_class( object = dfWideForm, class = "data.frame" )
  expect_equal( dim(dfWideForm), dim(dfTitanic) )
  expect_equal( sort(colnames(dfWideForm)), sort(colnames(dfTitanic)) )
}) 

test_that("No tag type prefixes in the wide form", {
  expect_equal( sort(unique(unlist(as.list(dfTitanic[,2:5])))), sort(unique(dfLongForm$Value)) )
}) 




