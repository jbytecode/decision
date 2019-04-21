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