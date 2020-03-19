context("Top-K statistic")
library(SMRMon)
library(SparseMatrixRecommender)


lsTrainingInds <- sample( 1:nrow(dfTitanic), floor( 0.7 * nrow(dfTitanic) ) )
dfTraining <- dfTitanic[ lsTrainingInds, ]
dfTesting <- dfTitanic[ setdiff( 1:nrow(dfTitanic), lsTrainingInds), ]

test_that("Creation with data", {

  expect_is( smrObj <- SMRMonUnit( data = dfTraining ) %>% SMRMonCreate( itemColumnName = "id" ), "SMR" )

  expect_equal( length( intersect( names(smrObj),
                                   c("M", "M01", "TagTypeRanges", "TagTypes", "ItemColumnName", "TagToIndexRules", "ItemToIndexRules", "Data") ) ),
                8 )

})


test_that("Top-K over ", {


})
