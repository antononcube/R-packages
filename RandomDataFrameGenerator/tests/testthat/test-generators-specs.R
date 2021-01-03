#context("Column generators specifications")
library(RandomDataFrameGenerator)


test_that("Simple generator specifications", {

  expect_s3_class( RandomDataFrame( generators = rnorm ), "data.frame" )

  expect_s3_class( RandomDataFrame( generators = RandomDataFrameGenerator::RandomWord ), "data.frame" )

  expect_s3_class( RandomDataFrame( generators = c(RandomDataFrameGenerator::RandomString, runif) ), "data.frame" )

  expect_s3_class( RandomDataFrame( generators = list(RandomDataFrameGenerator::RandomString, rnorm) ), "data.frame" )

  expect_s3_class( RandomDataFrame( generators = function(k) rpois(k, 4) ), "data.frame" )

  expect_s3_class( RandomDataFrame( generators = c( function(k) rpois(k, 4), function(k) RandomString(k, lambda = 12)  ) ), "data.frame" )


  dfR1 <- RandomDataFrame( nrow = 31, ncol = 13, generators = c(RandomWord, rnorm) )
  expect_s3_class( dfR1, "data.frame" )
  expect_equal( dim(dfR1), c(31, 13))
  expect_equal( setNames(sapply( dfR1, class), NULL), rep_len( x = c("character", "numeric"), length.out = ncol(dfR1) ) )


  dfR2 <- RandomDataFrame( nrow = 31, ncol = 23, generators = c(RandomWord, rnorm, runif) )
  expect_s3_class( dfR2, "data.frame" )
  expect_equal( dim(dfR2), c(31, 23))
  expect_equal( setNames(sapply( dfR2, class), NULL), rep_len( x = c("character", "numeric", "numeric"), length.out = ncol(dfR2) ) )


  dfR3 <- RandomDataFrame( nrow = 31, ncol = 2, generators = c(RandomWord, RandomWord, rnorm) )
  expect_s3_class( dfR3, "data.frame" )
  expect_equal( dim(dfR3), c(31, 2))
  expect_equal( setNames(sapply( dfR3, class), NULL), rep_len( x = c("character"), length.out = ncol(dfR3) ) )

})


test_that("Named generator specifications", {

  expect_s3_class( RandomDataFrame( generators = c( "GenA" = RandomDataFrameGenerator::RandomString, "GenB" = runif) ), "data.frame" )


  dfR1 <- RandomDataFrame( nrow = 31, ncol = 13, generators = c(RandomWord, rnorm) )
  expect_s3_class( dfR1, "data.frame" )
  expect_equal( dim(dfR1), c(31, 13))
  expect_equal( setNames(sapply( dfR1, class), NULL), rep_len( x = c("character", "numeric"), length.out = ncol(dfR1) ) )

})
