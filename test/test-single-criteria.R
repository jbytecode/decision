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


test_that("Savage - savage",{
  testStatus("Savage - savage")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- savage(mat)
  expect_equal(sort(result$best.strategy), 4)
})


test_that("Hurwicz - hurwicz",{
  testStatus("Hurwicz - hurwicz")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- hurwicz(mat)
  expect_equal(sort(result$best.strategy), 3)
})

test_that("Maximum likelihood",{
  testStatus("Maximum Likelihood")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- maximum.likelihood(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$best.strategy, 2)
})


test_that("Maximum likelihood - Scores",{
  testStatus("Maximum Likelihood - Scores")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- maximum.likelihood(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$expected.values, c(24, 29.2, 27, 27))
})
  


test_that("Expected Regret - Best Strategy",{
  testStatus("Expected Regret - Best Strategy")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- expected.regret(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$best.strategy, 2)
})


test_that("Expected Regret - expected values",{
  testStatus("Expected Regret - expected values")
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- expected.regret(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$expected.values, c(8, 2.8, 5, 5))
})



test_that("Expected value of full (exact) information",{
  testStatus("Expected value of full (exact) information - 1")
  mat <- matrix(c(
    3000, 2750, 2500, 2250,
    1500, 4750, 8000, 7750,
    2000, 5250, 8500, 11750
  ), nrow = 3, byrow = TRUE)
  result <- (expected.value.under.full.information(mat, weights = c(0.2, 0.3, 0.3, 0.2)))
  expect_equal(200, result$expected.value.of.full.information)
})


test_that("Expected value of full (exact) information",{
  testStatus("Expected value of full (exact) information - 2")
  mat <- matrix(c(
    3000, 2750, 2500, 2250,
    1500, 4750, 8000, 7750,
    2000, 5250, 8500, 11750
  ), nrow = 3, byrow = TRUE)
  result <- (expected.value.under.full.information(mat, weights = c(0.2, 0.3, 0.3, 0.2)))
  expect_equal(7075, result$expected.value.under.full.information)
})