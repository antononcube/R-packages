context("Term weights functions application")
library(SparseMatrixRecommender)


test_that("Column names are the same after term weights functions application", {

  expect_s3_class( smrObj <- SMRCreate( dataRows = dfTitanic, tagTypes = colnames(dfTitanic)[-1], itemColumnName = "id" ), "SMR" )

  mat <- SMRApplyTermWeightFunctions( docTermMat = smrObj$M, globalWeightFunction = "IDF", localWeightFunction = "None", normalizerFunction = "Cosine")
  
  expect_true( mean(colnames(mat) == colnames(smrObj$M)) == 1)

})
