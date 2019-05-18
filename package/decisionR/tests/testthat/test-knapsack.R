library("testthat")



testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Knapsack Problem",{
  testStatus("Knapsak Problem")
  utility <- c(6,5,6,5)
  w = c(10,20,30,40)
  result = knapsack_problem(utility, w, 55)
  expect_equal(12, result$total.profit)
  expect_equal(c(1, 0, 1, 0), result$solution)
  expect_equal(40, result$total.weights.used)
})
