#context("Too few values")
library(RandomDataFrameGenerator)


test_that("Too few values by list of values specification", {

  expect_s3_class(
    object =
      dfRes <-
      RandomDataFrame(
        nrow = 17,
        columnNames = c("ID", "Text", "Value"),
        generators = list( 1:10, RandomWord(4), rnorm(5))
      ),
    class = "data.frame" )

  expect_true( length(unique(dfRes$ID)) <= 10 )

  expect_true( length(unique(dfRes$Text)) <= 4 )

  expect_true( length(unique(dfRes$Value)) <= 5 )

})


test_that("Too few values by function specification", {

  expect_warning(
    object =
      dfRes <-
      RandomDataFrame(
        nrow = 12,
        columnNames = c("ID", "Text"),
        generators = list( 1:10, function(n) RandomWord(4) )
      ),
    regexp = "The generator specification for .* produced fewer random values" )


  expect_s3_class( object = dfRes, class = "data.frame" )

})


test_that("Enough values specification example", {

  expect_s3_class(
    object =
      dfRes <-
      RandomDataFrame(
        nrow = 8,
        columnNames = c("ID", "Text"),
        generators =
          list( 1:10, function(n) purrr::map_chr( 1:n, ~ paste(sample(c("java", "javascript", "ide", "agile"), 3, replace = T), collapse = " " )) )
      ),
    class = "data.frame" )


  })
