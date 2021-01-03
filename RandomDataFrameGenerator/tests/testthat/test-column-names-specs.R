#context("Column names specifications")
library(RandomDataFrameGenerator)


test_that("Column names generator specifications", {

  expect_s3_class( RandomDataFrame( columnNamesGenerator = RandomDataFrameGenerator::RandomWord ), "data.frame" )

  expect_s3_class( RandomDataFrame( columnNamesGenerator = RandomDataFrameGenerator::RandomString ), "data.frame" )

  expect_s3_class( RandomDataFrame( columnNamesGenerator = rnorm ), "data.frame" )

  expect_s3_class( RandomDataFrame( columnNamesGenerator = function(k) rpois(k, 4) ), "data.frame" )

  dfR1 <- RandomDataFrame( nrow = 31, ncol = 13, columnNamesGenerator = function(k) rpois(k, 4) )
  expect_s3_class( dfR1, "data.frame" )
  expect_equal( dim(dfR1), c(31, 13))

  dfR2 <- RandomDataFrame( nrow = 31, ncol = 13, columnNamesGenerator = RandomDataFrameGenerator::RandomWord )
  expect_s3_class( dfR2, "data.frame" )
  expect_equal( dim(dfR2), c(31, 13))

})


test_that("Column names specifications", {

  expect_s3_class( RandomDataFrame( columnNames = c("a", "b", "c", "d", "z") ), "data.frame" )


  dfR1 <- RandomDataFrame( nrow = 31, columnNames = c("a", "b", "c") )
  expect_s3_class( dfR1, "data.frame" )
  expect_equal( dim(dfR1), c(31, 3))
  expect_equal( colnames(dfR1), c("a", "b", "c") )


  dfR2 <- RandomDataFrame( nrow = 31, ncol = 13, columnNames = c("a", "b", "c") )
  expect_s3_class( dfR2, "data.frame" )
  expect_equal( dim(dfR2), c(31, 3))
  expect_equal( colnames(dfR2), c("a", "b", "c") )


  dfR3 <- RandomDataFrame( nrow = 31, columnNames = c("a", "b", "c"), columnNamesGenerator = function(k) rpois(k, 4) )
  expect_s3_class( dfR3, "data.frame" )
  expect_equal( dim(dfR3), c(31, 3))
  expect_equal( colnames(dfR3), c("a", "b", "c") )


  dfR4 <- RandomDataFrame( nrow = 31, ncol = 13, columnNames = c("a", "b", "c"), columnNamesGenerator = function(k) rpois(k, 4) )
  expect_s3_class( dfR4, "data.frame" )
  expect_equal( dim(dfR4), c(31, 3))
  expect_equal( colnames(dfR4), c("a", "b", "c"))

})


test_that("Valid column names peculiarities", {

  lsColNames <- c(".5", "Mazar-i-Sharif", "shtik", "barber", "115th")
  dfR1 <- RandomDataFrame( nrow = 31, columnNames = lsColNames )
  expect_s3_class( dfR1, "data.frame" )
  expect_equal( dim(dfR1), c(31, length(lsColNames)))
  expect_equal( colnames(dfR1), make.names(lsColNames) )

})
