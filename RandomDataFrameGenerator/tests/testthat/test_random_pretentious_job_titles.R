#context("Simplest specifications")
library(RandomDataFrameGenerator)


test_that("Random pretentious job titles", {

  expect_type( RandomPretentiousJobTitle(), "character" )

  expect_type( RandomPretentiousJobTitle( 3 ), "character" )

  expect_type( RandomPretentiousJobTitle( 5, 6 ), "character" )

  dfR1 <- RandomPretentiousJobTitle( size = 12, numberOfWords = runif(12,0,4), language = "English")
  expect_type( dfR1, "character" )

  dfR2 <- RandomPretentiousJobTitle( size = 12, numberOfWords = runif(12,0,4), language = "Bulgarian")
  expect_type( dfR2, "character" )

})

