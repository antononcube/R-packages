context("Sparse matrix recommender mushroom classifier")
#library(SparseMatrixRecommender)
devtools::load_all()

## Here we separate the indices of the data.
set.seed(6404)
trainingInds <- sample( 1:nrow(dfMushroom), round(0.75*nrow(dfMushroom)))
testInds <- setdiff(1:nrow(dfMushroom), trainingInds)

trainingColumnNames <- c("id", "cap.Shape", "cap.Surface", "cap.Color", "bruises.", "odor", "edibility" )

trainingData <- dfMushroom[trainingInds, trainingColumnNames]
testData <- dfMushroom[testInds, trainingColumnNames]

## Class labels
labelColumnName <- "edibility"


## Here we set the class label to focus on:
focusLabel <- "edible"

## Create SMR object
dfSpec <- SMREmptySpecification( ncol(trainingData) - 1 )
dfSpec$ColumnName <- setdiff( colnames(trainingData), "id" )
dfSpec$GlobalWeightFunction <- "IDF"
dfSpec$NormalizeByMax <- TRUE

smr <- SMRCreateFromSpecification( trainingData, metaDataSpec = dfSpec, itemColumnName = "id" )

# smr <- SMRCreate( trainingData, 
#                   tagTypes = setdiff( colnames(trainingData), "id" ), 
#                   itemColumnName = "id" )

## Classification
clRes <- predict( smr, 
                  testData[, grep( labelColumnName, colnames(testData), invert = T )], 
                  nTopNNs = 40, type = "decision", tagType = labelColumnName )

## Confusion matrix
confusionMatrix <- xtabs( ~ Actual + Predicted,
                          data.frame( Actual = testData[[labelColumnName]], 
                                      Predicted = clRes, 
                                      stringsAsFactors = F) )

## Tests
test_that("Reasonable classification results", {
  expect_true( confusionMatrix["edible","edible"] / sum(testData[[labelColumnName]] == "edible") > 0.62 )
  expect_true( confusionMatrix["poisonous","poisonous"] / sum(testData[[labelColumnName]] == "poisonous") > 0.62 )
}) 
