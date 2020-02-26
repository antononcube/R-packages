context("Basic NNMF calls")
library(NonNegativeMatrixFactorization)

set.seed(1289)

mat <- matrix( c(4, 7, 4, 10, 8, 8, 5, 3, 4, 5, 4, 5), nrow = 4, byrow = TRUE )

## Simple call

res1 <- NNMF( mat, 2 )

## Tests
test_that("Left and right matrix factors", {
  expect_true( is.list(res1) )
  expect_true( mean( names(res1) == c("W", "H") ) == 1 )

  expect_true( is.matrix(res1$W) || SparseMatrixQ(res1$W) )
  expect_true( is.matrix(res1$H) || SparseMatrixQ(res1$H) )
})

test_that("Good approximation", {
  expect_true( norm( mat - res1$W %*% res1$H ) / norm(mat) < 0.05 )
})

## Call with initial W and H
initialW <- matrix( c(1.5, 0.1, 1.2, 1.2, 0.5, 0.8, 0.7, 0.7), nrow = 4, byrow = T )

res2 <- NNMF( mat, 2, initialW = initialW, maxSteps = 12 )

## Tests
test_that("Initial W, left and right matrix factors", {
  expect_true( is.list(res2) )
  expect_true( mean( names(res2) == c("W", "H") ) == 1 )

  expect_true( is.matrix(res2$W) || SparseMatrixQ(res2$W) )
  expect_true( is.matrix(res2$H) || SparseMatrixQ(res2$H) )
})

test_that("Inital W, Good approximation", {
  expect_true( norm( mat - res2$W %*% res2$H ) / norm(mat) < 0.05 )
})
