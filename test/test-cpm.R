library("testthat")

source ("../src/cpm.R")

testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("CPM - critical path",{
  testStatus("CPM - critical path")
  dt <- data.frame(
    Activity = c("A", "B", "C", "D", "E", "F", "G"),
    Dependency = c("-", "-", "A, B", "B,C", "C", "D", "A,B, D "),
    Duration = c(10, 5, 7, 3, 2, 18, 5),
    stringsAsFactors =  FALSE
  )
  result <- CPM(dt)
  expect_equal(c("F", "D", "C", "A", "-"), result$critical.path)
})

test_that("CPM - critical path length",{
  testStatus("CPM - critical path length")
  dt <- data.frame(
    Activity = c("A", "B", "C", "D", "E", "F", "G"),
    Dependency = c("-", "-", "A, B", "B,C", "C", "D", "D,E,F"),
    Duration = c(10, 5, 7, 3, 2, 18, 5),
    stringsAsFactors =  FALSE
  )
  result <- CPM(dt)
  expect_equal(43, result$critical.path.length)
})
