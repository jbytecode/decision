library("testthat")



testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Assignment Problem",{
  testStatus("Assignment Problem")
  m <- matrix(c(
    5, 6, 4,
    2, 1, 3,
    9, 10, 11
  ), nrow = 3, ncol = 3)
  result <- assignment_problem(m)
  expect_equal(result$optimum, 14)
  expect_equal(result$solution, c(0, 0, 1, 0, 1, 0, 1, 0, 0))
})
