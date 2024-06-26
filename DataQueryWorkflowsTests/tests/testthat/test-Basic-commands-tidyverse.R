context("Basic commands tidyverse")
library(tidyverse)
library(DataQueryWorkflowsTests)
library(ExternalParsersHookUp)


test_that("Distinct rows", {

  ##
  lsCommands1 <- "use dfStarwarsFilms;
  select name;
  distinct values only"

  res1 <- ToDataQueryWorkflowCode( command = lsCommands1, parse = TRUE, target = "tidyverse" )

  expect_is( res1, "expression" )

  expect_equivalent(
    eval(res1),
    dfStarwarsFilms %>% dplyr::select(name) %>% dplyr::distinct()
  )

})


test_that("Select columns", {

  ##
  lsCommands1 <- "use dfStarwars;
  select name, mass and height"

  res1 <- ToDataQueryWorkflowCode( command = lsCommands1, parse = TRUE, target = "tidyverse" )

  expect_is( res1, "expression" )

  expect_equivalent(
    eval(res1),
    dfStarwars %>% dplyr::select(name, mass, height)
  )

  ##
  lsCommands2 <- "use dfStarwars;
  select 'name', 'mass', and 'height'"

  res2 <- ToDataQueryWorkflowCode( command = lsCommands2, parse = TRUE, target = "tidyverse" )

  expect_is( res2, "expression" )

  expect_equivalent(
    eval(res2),
    dfStarwars %>% dplyr::select_at( .vars = c("name", "mass", "height") )
  )

})


test_that("Mutate columns", {
  ##
  lsCommands <- "use dfStarwars;
  mutate bmi = `mass/height^2`"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::mutate( bmi = mass / height ^ 2 )
  )

  ##
  lsCommands <- "use dfStarwars;
  transform bmi = `mass/height^2`"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::mutate( bmi = mass / height ^ 2 )
  )

})


test_that("Rename columns", {
  ##
  lsCommands <- "use dfStarwars;
  rename mass, height, and gender as mass2, height2, gender2"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::rename( c("mass2" = "mass", "height2" = "height", "gender2" = "gender" ) )
  )

})


test_that("Drop columns", {
  ##
  lsCommands <- "use dfStarwars;
  drop columns species, name"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::mutate( species = NULL, name = NULL )
  )

})


test_that("Arrange columns", {

  ##
  lsCommands <- "use dfStarwars;
  arrange by species, gender, name"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::arrange( species, gender, name )
  )

  ##
  lsCommands <- "use dfStarwars;
  sort descending by species, gender, name "

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::arrange( desc(species, gender, name) )
  )

})


test_that("Filter rows", {

  ##
  lsCommands <- "use dfStarwars;
  filter by mass is greater or equal than 200 and height is greater than 100 or height is 202"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "tidyverse" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    dfStarwars %>% dplyr::filter( mass >= 200 & height > 100 | height == 202 )
  )

})
