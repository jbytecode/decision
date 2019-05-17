library("testthat")

testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Markov Chain - 1 period ahead",{
  testStatus("Markov Chain - 1 period ahead")
  transition <- matrix(
    c(
      0.5, 0.4, 0.1,
      0.1, 0.1, 0.8,
      0.4, 0.4, 0.2
    ),nrow = 3, ncol = 3, byrow = TRUE)
  state <- c(1, 0, 0)
  result <- markov(transition, state, periodAhead = 1)
  expect_equal(c(0.5, 0.4, 0.1), result)
})

test_that("Markov Chain - 1000 period ahead",{
  testStatus("Markov Chain - 1000 period ahead")
  transition <- matrix(
    c(
      0.5, 0.4, 0.1,
      0.1, 0.1, 0.8,
      0.4, 0.4, 0.2
    ),nrow = 3, ncol = 3, byrow = TRUE)
  state <- c(1, 0, 0)
  result <- markov(transition, state, periodAhead = 1000)
  expect_equal(c(0.342, 0.308, 0.350), result, tolerance = 0.001)
})



test_that("Markov Chain Equilibrium",{
  testStatus("Markov Chain Equilibrium")
  transition <- matrix(
    c(
      0.5, 0.4, 0.1,
      0.1, 0.1, 0.8,
      0.4, 0.4, 0.2
    ),nrow = 3, ncol = 3, byrow = TRUE)
  state <- c(1, 0, 0)
  result <- markov_equilibrium(transition, state)
  expect_equal(c(0.342, 0.308, 0.350), result$solution, tolerance = 0.001)
  expect_equal(TRUE, result$converged)
  expect_less_than(result$tries, 200)
})

