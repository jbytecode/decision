library("testthat")

source ("../src/game.R")

epsilon <- 0.001

testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Game",{
  testStatus("Game")
  mat <- matrix(c(-2,6,3,3,-4,7,-1,2,4), byrow = TRUE, nrow = 3)
  result <- game(mat)
  expect_equal(result$g1, result$g2)
  expect_equal(result$g1, 0.6666667, epsilon)
  expect_equal(result$p.row, c(0.4666667, 0.5333333, 0.0000000), epsilon)
  expect_equal(result$p.col, c(0.6666667, 0.3333333, 0.0000000), epsilon)
})