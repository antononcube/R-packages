#context("ROC functions")
library(ROCFunctions)


test_that("ROC predicates", {

  expect_type( ROCVectorQ( list("TruePositive" = 15, "FalsePositive" = 149, "FalseNegative" = 15,"TrueNegative" = 1523) ), "logical" )

  expect_true( ROCVectorQ( list("TruePositive" = 15, "FalsePositive" = 149, "FalseNegative" = 15,"TrueNegative" = 1523) ) )

  expect_type( ROCVectorQ( c("TruePositive" = 15, "FalsePositive" = 149, "FalseNegative" = 15,"TrueNegative" = 1523) ), "logical" )

  expect_true( ROCVectorQ( c("TruePositive" = 15, "FalsePositive" = 149, "FalseNegative" = 15,"TrueNegative" = 1523) ) )

})


test_that("ROC functions computations", {

  expect_s3_class( ComputeROCFunctions( x = c("TruePositive" = 15, "FalsePositive" = 149, "FalseNegative" = 15,"TrueNegative" = 1523) ), "data.frame")

  dfRand <-
    data.frame(
      TruePositive = c(24, 56, 37),
      TrueNegative = c(38, 61, 9),
      FalsePositive = c(95, 6, 22),
      FalseNegative = c(26, 98, 74)
    )

  expect_s3_class( ComputeROCFunctions( x = dfRand ), "data.frame")

  expect_s3_class( ComputeROCFunctions( x = dfRand, rocs = ROCAcronymsDictionary()$Acronym ), "data.frame")

})


test_that("ROC comptations errors", {

  expect_error( object = ComputeROCFunctions( x = c(1,2) ),
                regexp = "The first argument is expected to be a numerical list or data frame with names" )

  expect_error( object = ComputeROCFunctions( x = list("TruePositive" = "df", "FalsePositive" = "149", "FalseNegative" = "df", "TrueNegative" = "232")  ),
                regexp = "The first argument is expected to be a numerical list or data frame with names" )

})
