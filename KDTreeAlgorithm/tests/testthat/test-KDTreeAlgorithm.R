library(KDTreeAlgorithm)

test_that("basic usage", {

  # Create a set of 2D points as a matrix
  points <- matrix(c(2, 3, 5, 4, 9, 6, 4, 7, 8, 1, 7, 2), ncol = 2, byrow = TRUE)
  rownames(points) <- letters[1:nrow(points)]

  # Initialize the K-D tree
  kd_tree <- KDimensionalTree(points)

  expect_equal(class(kd_tree), "KDimensionalTree")
  expect_equal(names(kd_tree), c( "points", "tree", "distanceFunction"))

  # Find the nearest neighbors for a given point
  query_point <- c(5, 5)
  matNNs <- KNearest(kd_tree, query_point, k = 3, format = "values")
  expect_contains(class(matNNs), "matrix")
  expect_equal(dim(matNNs), c(3L, 2L))

  dfNNs <- KNearest(kd_tree, query_point, k = 5, format = "data.frame")
  expect_equal(class(dfNNs), "data.frame")

  expect_equal(dim(dfNNs), c(5L, 3L))
  expect_equal(colnames(dfNNs), c("RowName", "Index", "Distance"))

})

test_that("correctness of k-neighbors and whithin ball", {

  set.seed(123) # For reproducibility
  points <- matrix(runif(2000, min = 0, max = 100), ncol = 2)
  points <- round(points, digits = 5)
  rownames(points) <- paste0("P", seq_len(nrow(points)))

  kdObj <- KDimensionalTree(points)
  expect_equal(class(kdObj), "KDimensionalTree")

  # Find the nearest neighbors for a given point
  qPoint <- c(17, 44)
  matNNs <- KNearest(kdObj, qPoint, k = 12, format = "matrix")

  # Expected (approximate) result
  matRes <- matrix(c(17.17434, 15.32549, 14.70841, 15.72953, 20.40959, 13.92036, 13.78584, 17.50527, 18.38285, 12.60828, 15.71678, 11.94048,
                    41.99829, 45.12470, 44.24819, 41.24192, 43.71595, 46.59294, 41.55437, 48.01264, 40.18139, 44.92705, 39.05170, 45.65182),
                    nrow = 12, ncol = 2, byrow = FALSE)

  # Exact expectation because of the rounding avove. Otherwise use
  #   expect_true(abs(max(matNNs - matRes)) < 10^-12)
  expect_equal(matNNs, matRes)

  # Nearest withing ball
  dfNNs <- NearestWithinBall(kdObj, query_point, radius = 10, format = "data.frame")
  expect_true(max(dfNNs$Distance) < 10)

  # Same NNs result if using simple scanning
  calculate_distances <- function(point, matrix_2d) {
    sqrt((matrix_2d[,1] - point[1])^2 + (matrix_2d[,2] - point[2])^2)
  }

  distances <- calculate_distances(query_point, points)
  expect_equal(mean(sort(names(distances[distances<10])) == sort(dfNNs$RowName)), 1)
})
