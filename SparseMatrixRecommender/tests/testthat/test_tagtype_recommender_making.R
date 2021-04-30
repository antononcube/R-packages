context("Tag type recommender making")
library(SparseMatrixRecommender)


test_that("Metadata recommender matrix same as contingency matrix", {

  expect_s3_class( smrObj <- SMRCreate( dataRows = dfTitanic, tagTypes = colnames(dfTitanic)[-1], itemColumnName = "id" ), "SMR" )

  expect_warning( smrMetaObj <- SMRToMetadataRecommenderByReplacement( smr = smrObj, tagTypeTo = "passengerClass", tagTypes = smrObj$TagTypes ), regexp = "^Removing" )
  
  expect_s3_class( smrMetaObj, "SMR" )

  expect_s4_class( smrMetaObj$M, "dgCMatrix" )
  
  expect_s3_class( smrMetaObj2 <- SMRToMetadataRecommenderByReplacement( smr = smrObj, tagTypeTo = "passengerClass", tagTypes = NULL ), "SMR" )
  
  expect_s4_class( smrMetaObj2$M, "dgCMatrix" )

  expect_s4_class(
    matXTabs <-
      cbind(
        xtabs( formula = ~ passengerClass + passengerAge, data = dfTitanic, sparse = TRUE ),
        xtabs( formula = ~ passengerClass + passengerSex, data = dfTitanic, sparse = TRUE ),
        xtabs( formula = ~ passengerClass + passengerSurvival, data = dfTitanic, sparse = TRUE )
      ),
    "dgCMatrix"
    )

  expect_equal( smrMetaObj$M, matXTabs )
  
  expect_equal( smrMetaObj2$M, matXTabs )

})
