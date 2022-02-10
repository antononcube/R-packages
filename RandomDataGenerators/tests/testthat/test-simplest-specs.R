#context("Simplest specifications")
library(RandomDataFrameGenerator)


test_that("Simplest specifications", {

  expect_s3_class( RandomDataFrame(), "data.frame" )

  expect_s3_class( RandomDataFrame( 3 ), "data.frame" )

  expect_s3_class( RandomDataFrame( 5, 6 ), "data.frame" )

  dfR1 <- RandomDataFrame( nrow = 15, ncol = 23 )
  expect_s3_class( dfR1, "data.frame" )
  expect_equal( dim(dfR1), c(15, 23))

  dfR2 <- RandomDataFrame( nrow = 15 )
  expect_s3_class( dfR2, "data.frame" )
  expect_equal( nrow(dfR2), 15)

  dfR3 <- RandomDataFrame( ncol = 15 )
  expect_s3_class( dfR3, "data.frame" )
  expect_equal( ncol(dfR3), 15)

})

