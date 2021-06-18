library("testthat")

epsilon <- 0.001


test_that("Game", {
  mat <- matrix(c(-2, 6, 3, 3, -4, 7, -1, 2, 4), byrow = TRUE, nrow = 3)
  result <- game(mat)
  expect_equal(result$g1, result$g2)
  expect_equal(result$g1, 0.6666667, epsilon)
  expect_equal(result$p.row, c(0.4666667, 0.5333333, 0.0000000), epsilon)
  expect_equal(result$p.col, c(0.6666667, 0.3333333, 0.0000000), epsilon)
})

test_that("Game - Rock / Paper / Scissors", {
  a <- matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), nrow = 3, byrow = TRUE)
  result <- game(a + 2)
  expect_equal(result$g1, result$g2)
  expect_equal(result$g1, 2, epsilon)
  expect_equal(result$p.row, c(0.333, 0.333, 0.333), epsilon)
  expect_equal(result$p.col, c(0.333, 0.333, 0.333), epsilon)
})