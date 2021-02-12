# Geometric Nearest Neighbors Monad in R

## In brief

The primary motivation for making this Geometric Nearest Neighbors (GNN) software monad
(GNNMon) is the implementation of a simple Nearest Neighbors (NN's) classifier that tells 
does a point belong to a set of points.

That classification functionality can be also used to find outliers in a set points.

## Installation

Here is an installation command from GitHub's repository:

    devtools::install_github("antononcube/R-packages", subdir = "GNNMon-R")

## Usage example

Library load:

    library(GNNMon)
    
Make random data:

    set.seed(234)
    data <- matrix( c( rnorm( n = 30, mean = 0, sd = 5 ), rnorm( n = 30, mean = 12, sd = 3) ), ncol = 2 )
    colnames(data) <- c("X", "Y")

Make the monad object and compute proximity thresholds:

    gnnObj <- 
      GNNMonUnit( data ) %>% 
      GNNMonComputeNearestNeighborDistances( nTopNNs = 3, method = "euclidean" ) %>% 
      GNNMonComputeThresholds( nnsRadiusFunction = mean, thresholdsIdentifier = HampelIdentifierParameters ) %>% 
      
Points to test:

    dfPoints <- data.frame( X = c(-6, 2), Y = c(4.5, 16) )

Classification of the testing points:

    gnnObj %>% 
      GNNMonClassify( points = as.matrix(dfPoints) ) %>% 
      GNNMonTakeValue

Here is how the result looks like:

      Index  Radius Label
    1     1 4.57958 FALSE
    2     2 1.43192  TRUE


Here is more detailed documentation:
["Geometric Nearest Neighbors Examples"](https://htmlpreview.github.io/?https://github.com/antononcube/R-packages/blob/master/GNNMon-R/notebooks/Geometric-nearest-neighbors-examples.nb.html)

-----

Anton Antonov
Florida, USA, July, 2019.
