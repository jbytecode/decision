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



test_that("Test Vikor - best.index",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- vikor(A, w)
  expect_equal(result$best.index, 4)
})


test_that("Test Vikor - best",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- vikor(A, w)
  expect_equal(result$best, "Alternative 4")
})


test_that("Test Electre - best",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- electre(A, w)
  expect_equal(result$best, "Alternative 4")
})


test_that("Test Electre - C vector",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- electre(A, w)
  expect_equal(result$C, c(0.36936937,  0.01501502, -2.47347347,  2.08908909), tolerance = 1 / 1000)
})


test_that("Test Electre - D vector",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- electre(A, w)
  expect_equal(result$D, c(0.1914244, -0.1903929,  2.8843076, -2.8853391), tolerance = 1 / 1000)
})


test_that("Test Moora - best",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- moora(A, w)
  expect_equal(result$best, "Alternative 4")
})


test_that("Test Moora - scores",{
  w <- c(0.110, 0.035, 0.379, 0.384, 0.002, 0.002, 0.010, 0.077)
  A <- matrix(c(100, 92, 10, 2, 80, 70, 95, 80,
                80, 70, 8, 4, 100, 80, 80, 90,
                90, 85, 5, 0, 75, 95, 70, 70,
                70, 88, 20, 18, 60, 90, 95, 85), nrow = 4, byrow = TRUE)
  result <- moora(A, w)
  expect_equal(result$scores, c(0.33159387, 0.29014464, 0.37304311, 0.01926526), tolerance = 1 / 1000)
})
