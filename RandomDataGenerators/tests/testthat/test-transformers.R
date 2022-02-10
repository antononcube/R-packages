library(RandomDataFrameGenerator)

set.seed(3353)
numberOfUsers <- 3
numberOfIngredients <- 3
numberOfCuisines <- 2
numberOfDays <- 90
numberOfItems <- 20

test_that("Variables split", {

  expect_s3_class( object =
                     dfRand <-
                     RandomDataFrame(
                       nrow = numberOfItems,
                       columnNames = c("ID", "UserID", "PeriodMeal", "Ingredient", "Cuisine", "Timestamp"),
                       generators =
                         list(
                           ID = function(n) as.character(round(runif(n = n, min = 10^6, max = 16^8 ))),
                           UserID = RandomString(numberOfUsers, charClasses = "[a-z]"),
                           PeriodMeal = c("lunch", "dinner", "breakfast"),
                           Ingredient = function(n) purrr::map_chr(1:n, function(x) paste( sample( x = c("protein", "carb", "low calorie", "fat", "saturated fat"), prob = c(1, 2, 0.5, 1, 0.4), size = numberOfIngredients, replace = F), collapse = "; " )),
                           Cuisine =    function(n) purrr::map_chr(1:n, function(x) paste( sample( x =c("Chinese", "Finnish", "Turkish", "Greek", "Scandinavian"),  prob = c(1, 2, 0.5, 1, 0.4), size = numberOfCuisines, replace = F),    collapse = "; " )),
                           Timestamp = function(n) as.character(sort(RandomDate(size = n, min = Sys.time(), max = Sys.time() + numberOfDays*24*3600)))
                         ),
                       form = "Long"
                     ), class = "data.frame" )


  expect_equal( nrow(dfRand), numberOfItems * 6 )

  expect_s3_class( object = dfRand2 <- SplitVariableValues( data = dfRand, splitPattern = ";", variableToSplit = c("Ingredient", "Cuisine"), valueColumn = "Value.character" ), "data.frame" )

  expect_equal( nrow(dfRand2), nrow(dfRand) + numberOfItems * (numberOfIngredients - 1) + numberOfItems * (numberOfCuisines - 1) )

})
