#context("Simplest specifications")
library(RandomDataFrameGenerator)


test_that("Random pet names", {

  expect_type( RandomPetName(), "character" )

  expect_type( RandomPetName( 3 ), "character" )

  expect_type( RandomPetName( 5, "Cat" ), "character" )

  expect_type( RandomPetName( size = 120, species = NULL, weighted = F ), "character" )

  expect_type( RandomPetName( size = 12, species = c("Cat", "Dog"), weighted = T ), "character" )

  expect_type( RandomPetName( size = 12, species = c("CAT", "dog"), weighted = T ), "character" )

  expect_error(
    object = RandomPetName( size = -2),
    regexp = "The argument size is expected to be a positive integer"
  )

  expect_error(
    object = RandomPetName( size = 2, species = 3 ),
    regexp = "The argument species is expected to be a character vector"
  )

  expect_error(
    object = RandomPetName( size = 120, species = c("Giraffe3", "Horse2") ),
    regexp = "The argument species has no known species"
  )

})

