context("Basic operations tidyverse")
library(tidyverse)
library(DataQueryWorkflowsTests)


test_that("Select columns", {

  ##
  lsCommands <- "use dfStarwars; select name, mass and height"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::select(name, mass, height) )
})


test_that("Mutate columns", {
  ##
  lsCommands <- "use dfStarwars; mutate bmi = `mass/height^2`"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::mutate( bmi = mass / height ^ 2 ) )

  ##
  lsCommands <- "use dfStarwars; transform bmi = `mass/height^2`"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::mutate( bmi = mass / height ^ 2 ) )
})


test_that("Rename columns", {
  ##
  lsCommands <- "use dfStarwars; rename mass, height, and gender as mass2, height2, gender2"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::rename( c("mass2" = "mass", "height2" = "height", "gender2" = "gender" ) ) )
})


test_that("Drop columns", {
  ##
  lsCommands <- "use dfStarwars; drop columns species, name"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::mutate( species = NULL, name = NULL ) )

})


test_that("Arrange columns", {
  ##
  lsCommands <- "use dfStarwars; arrange by species, gender, name"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::arrange( species, gender, name ) )

  ##
  lsCommands <- "use dfStarwars; sort descending by species, gender, name "
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::arrange( desc(species, gender, name) ) )

})


test_that("Filter rows", {
  ##
  lsCommands <- "use dfStarwars; filter by mass greater than 200 and height greater than 100 or height is 202"
  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent( eval(res), dfStarwars %>% dplyr::filter( mass > 200 & height > 100 | height == 202 ) )

})
