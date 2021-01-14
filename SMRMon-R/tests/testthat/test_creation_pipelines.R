context("Creation pipelines")
library(SMRMon)
library(SparseMatrixRecommender)


test_that("Creation with data", {

  expect_s3_class( smrObj <- SMRMonUnit( data = dfTitanic ) %>% SMRMonCreate( itemColumnName = "id" ), "SMR" )

  expect_equal( length( intersect( names(smrObj),
                                   c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data") ) ),
                8 )

  expect_s3_class( smrObj1 <- SMRMonUnit( ) %>% SMRMonCreate( data = dfTitanic, itemColumnName = "id" ), "SMR" )

  expect_equal( length( intersect( names(smrObj1),
                                   c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data") ) ),
                8 )

})


smats <- purrr::map( names(dfTitanic)[-1], function(x) xtabs( formula = as.formula( paste( "~", names(dfTitanic)[[1]], "+", x ) ), data = dfTitanic, sparse = TRUE ) )

test_that("Creation with matrices", {

  expect_error( SMRMonUnit( ) %>% SMRMonCreateFromMatrices( matrices = smats, itemColumnName = "id" ),
                regexp = "When tagTypes = NULL the elements of the argument matrices are expected to have unique names.*")

  expect_s3_class( smrObj2 <- SMRMonUnit( ) %>% SMRMonCreateFromMatrices( matrices = smats, tagTypes = names(dfTitanic)[-1], itemColumnName = names(dfTitanic)[[1]] ), "SMR" )

  expect_equal( length( intersect( names(smrObj2),
                                   c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules" ) ) ),
                7 )

  expect_s3_class( smrObj3 <- SMRMonUnit( ) %>% SMRMonCreateFromMatrices( matrices = setNames( smats, names(dfTitanic)[-1]), itemColumnName = names(dfTitanic)[[1]] ), "SMR" )

  expect_equal( length( intersect( names(smrObj3),
                                   c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules" ) ) ),
                7 )

})
