library("testthat")

testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Scheduling Problem - 2 machines",{
  testStatus("Scheduling Problem - 2 machines")
  A <- c(5,2,4,3,1)
  B <- c(6,7,8,9,10)
  result <- scheduling_2_machines(cbind(A, B))
  expect_equal(c(5,2,4,3,1), result$ordering)
})
