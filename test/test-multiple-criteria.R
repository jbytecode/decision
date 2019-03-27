library("testthat")

source ("../src/multiple-criteria.R")

test_that("Test Topsis - best index, 3 alternatives, 4 criteria",{
  A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
  W <- c(4, 2, 6, 8)
  w <- W / sum(W)
  result <- topsis(A, w)
  expect_equal(result$best.index, 2)
})

test_that("Test Topsis - best alternative, 3 alternatives, 4 criteria",{
  A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
  W <- c(4, 2, 6, 8)
  w <- W / sum(W)
  result <- topsis(A, w)
  expect_equal(result$best, "Alternative 2")
})

test_that("Test Topsis - scores, 3 alternatives, 4 criteria",{
  A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
  W <- c(4, 2, 6, 8)
  w <- W / sum(W)
  result <- topsis(A, w)
  expect_equal(result$scores, c(0.3876870, 0.6503238, 0.0834767), tolerance = 1 / 1000)

})



