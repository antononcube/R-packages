#context("Simplest specifications")
library(RandomDataFrameGenerator)


test_that("Random dates", {

  expect_true( "POSIXct" %in% class(RandomDate()) )

  expect_true( "POSIXct" %in% class(RandomDate( 3 )))

  expect_true( "POSIXct" %in% class(RandomDate(size = 3, min = "2021-01-12", max = "2022-02-21")))

  expect_true( "POSIXct" %in% class(RandomDate(size = 3, min = as.numeric(Sys.time()), max = as.numeric(Sys.time()) + 2000000, origin = "1900-01-01" )) )

  expect_error(
    object = RandomDate( size = -2 ),
    regexp = "The argument size is expected to be a positive integer"
  )

  expect_error(
    object = RandomDate(size = 3, min = as.numeric(Sys.time()), max = as.numeric(Sys.time()) + 2000000),
    regexp = "must be supplied"
  )


})

