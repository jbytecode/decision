library("testthat")

test_that("Laplace - Laplace", {
  mat <- matrix(c(
    3000, 2750, 2500, 2250,
    1500, 4750, 8000, 7750,
    2000, 5250, 8500, 11750
  ), nrow = 3, byrow = TRUE)
  result <- laplace(mat)
  expect_equal(result$expected.values, c(2625, 5500, 6875))
  expect_equal(result$best.strategy, 3)
})


test_that("Maximin - maximin", {
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


test_that("Minimax - minimax", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- minimax(mat)
  expect_equal(result$row.max, c(26, 34, 34, 30))
  expect_equal(result$best.strategy, 1)
})



test_that("Maximax - maximax", {
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


test_that("Minimin - minimin", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- minimin(mat)
  expect_equal(result$row.mins, c(18, 18, 24, 20))
  expect_equal(sort(result$best.strategy), c(1, 2))
})


test_that("Savage - savage", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- savage(mat)
  expect_equal(sort(result$best.strategy), 4)
})


test_that("Hurwicz - hurwicz", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- hurwicz(mat)
  expect_equal(sort(result$best.strategy), 3)
})

test_that("Maximum likelihood", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- maximum.likelihood(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$best.strategy, 2)
})


test_that("Maximum likelihood - Scores", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- maximum.likelihood(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$expected.values, c(24, 29.2, 27, 27))
})



test_that("Expected Regret - Best Strategy", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- expected.regret(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$best.strategy, 2)
})


test_that("Expected Regret - expected values", {
  mat <- matrix(c(
    26, 26, 18, 22,
    22, 34, 30, 18,
    28, 24, 34, 26,
    22, 30, 28, 20
  ), nrow = 4, byrow = TRUE)
  result <- expected.regret(mat, c(0.2, 0.5, 0.2, 0.1))
  expect_equal(result$expected.values, c(8, 2.8, 5, 5))
})



test_that("Expected value of full (exact) information", {
  mat <- matrix(c(
    3000, 2750, 2500, 2250,
    1500, 4750, 8000, 7750,
    2000, 5250, 8500, 11750
  ), nrow = 3, byrow = TRUE)
  result <- (expected.value.of.perfect.information(mat, weights = c(0.2, 0.3, 0.3, 0.2)))
  expect_equal(200, result$expected.value.of.perfect.information)
})


test_that("Expected value of full (exact) information", {
  mat <- matrix(c(
    3000, 2750, 2500, 2250,
    1500, 4750, 8000, 7750,
    2000, 5250, 8500, 11750
  ), nrow = 3, byrow = TRUE)
  result <- (expected.value.of.perfect.information(mat, weights = c(0.2, 0.3, 0.3, 0.2)))
  expect_equal(7075, result$expected.value.under.full.information)
})