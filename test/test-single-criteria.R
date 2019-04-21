library("testthat")

source ("../src/single-criteria.R")

testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Laplace - Laplace",{
  testStatus("Laplace - Laplace")
  mat <- matrix(c(
    3000, 2750, 2500, 2250,
    1500, 4750, 8000, 7750,
    2000, 5250, 8500, 11750
  ), nrow = 3, byrow = TRUE)
  result <- laplace(mat)
  expect_equal(result$expected.values, c(2625, 5500, 6875))
  expect_equal(result$best.strategy, 3)
})


test_that("Maximin - maximin",{
  testStatus("Maximin - maximin")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- maximin(mat)
  expect_equal(result$row.mins, c(18, 18, 24, 20))
  expect_equal(result$best.strategy, 3)
})


test_that("Maximax - maximax",{
  testStatus("Maximax - maximax")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- maximax(mat)
  expect_equal(result$row.max, c(26, 34, 34, 30))
  expect_equal(sort(result$best.strategy), c(2, 3))
})