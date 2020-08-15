context("Filter commands tidyverse")
library(tidyverse)
library(DataQueryWorkflowsTests)
library(ExternalParsersHookUp)


test_that("Filtering spelled out 1", {

  ##
  expectedRes <- dfStarwars %>% dplyr::filter( mass >= 200 & height > 100 | height == 202 )

  ##
  lsCommands <- "use dfStarwars;
  filter by mass greater or equal 200 and height greater than 100 or height equals 202"

  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

  ##
  lsCommands <- "use dfStarwars;
  filter by mass is greater or equal than 200 and height is greater than 100 or height is 202"

  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

})


test_that("Filtering shorthand", {

  ##
  expectedRes <- dfStarwars %>% dplyr::filter( mass >= 200 & height > 100 | height == 202 )

  ##
  lsCommands <- "use dfStarwars;
  filter rows by mass >= 200 && height > 100 || height == 202"

  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

  ##
  lsCommands <- "use dfStarwars;
  filter the rows by mass ≥ 200 and height > 100 or height = 202"

  res <- ToDataQueryWorkflowCode( lsCommands )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

})

