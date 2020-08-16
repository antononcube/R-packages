context("Basic commands base")
library(tidyverse)
library(DataQueryWorkflowsTests)
library(ExternalParsersHookUp)


test_that("Distinct rows", {

  ##
  lsCommands1 <- "use dfStarwarsFilms;
  select name;
  distinct values only"

  res1 <- ToDataQueryWorkflowCode( command = lsCommands1, parse = TRUE, target = "base" )

  expect_is( res1, "expression" )

  expect_equivalent(
    { eval(res1); obj },
    unique(dfStarwarsFilms$name)
  )

})


test_that("Select columns", {

  ##
  lsCommands1 <- "use dfStarwars;
  select name, mass and height"

  res1 <- ToDataQueryWorkflowCode( command = lsCommands1, parse = TRUE, target = "base" )

  expect_is( res1, "expression" )

  expect_equivalent(
    eval(res1),
    dfStarwars[ , c("name", "mass", "height") ]
  )

  ##
  lsCommands2 <- "use dfStarwars;
  select 'name', 'mass', and 'height'"

  res2 <- ToDataQueryWorkflowCode( command = lsCommands2, parse = TRUE, target = "base" )

  expect_is( res2, "expression" )

  expect_equivalent(
    eval(res2),
    dfStarwars[ , c("name", "mass", "height") ]
  )

})


test_that("Mutate columns", {

  ##
  expectedRes <-   {
    obj <- dfStarwars
    obj$bmi = obj$mass / obj$height ^ 2;
    obj
  }

  ##
  lsCommands <- "use dfStarwars;
  mutate bmi = `mass/height^2`"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

  ##
  lsCommands <- "use dfStarwars;
  transform bmi = `mass/height^2`"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

})


test_that("Rename columns", {

  ##
  expectedRes <- {
    obj <- as.data.frame(dfStarwars) ;
    colnames(obj) <- gsub( "mass", "mass2", colnames(obj) ) ;
    colnames(obj) <- gsub( "height", "height2", colnames(obj) ) ;
    colnames(obj) <- gsub( "gender", "gender2", colnames(obj) )
    obj
  }

  ##
  lsCommands <- "use dfStarwars;
  rename 'mass', 'height', and 'gender' as 'mass2', 'height2', 'gender2'"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    {eval(res); obj },
    expectedRes
  )

})


test_that("Drop columns", {

  ##
  lsCommands <- "use dfStarwars;
  drop columns 'species', 'name'"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    {
      obj <- dfStarwars ;
      obj <- obj[, setdiff( colnames(obj), c("species", "name") )];
      obj
    }
  )

})


test_that("Arrange columns", {

  ##
  expectedRes1 <- {
    obj <- dfStarwars ;
    obj <- obj[ order(obj[, c("species", "gender", "name")]), ] ;
    obj
  }

  ##
  lsCommands <- "use dfStarwars;
  arrange by 'species', 'gender', 'name'"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes1
  )

  ##
  expectedRes2 <- {
    obj <- dfStarwars ;
    obj <- obj[ rev(order(obj[, c("species", "gender", "name")])), ] ;
    obj
  }

  ##
  lsCommands <- "use dfStarwars;
  sort descending by 'species', 'gender', 'name'"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes2
  )

})


test_that("Filter rows", {

  ##
  expectedRes <- {
    obj <- dfStarwars ;
    obj <- obj[obj$mass >= 200 & obj$height > 100 | obj$height == 202, ]
    obj
  }

  ##
  lsCommands <- "use dfStarwars;
  filter by mass is greater or equal than 200 and height is greater than 100 or height is 202"

  res <- ToDataQueryWorkflowCode( command = lsCommands, parse = TRUE, target = "base" )

  expect_is( res, "expression" )

  expect_equivalent(
    eval(res),
    expectedRes
  )

})
