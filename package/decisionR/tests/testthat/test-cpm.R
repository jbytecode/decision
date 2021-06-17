library("testthat")

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
  expect_equal(c("F", "D", "C", "A"), result$critical.path)
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

test_that("PERT - critical path length",{
  testStatus("PERT - critical path length")
  dt <- data.frame(
    Activity = c("A", "B", "C", "D", "E", "F", "G"),
    Dependency = c("-", "-", "A, B", "B,C", "C", "D", "A,B, D "),
    O = c(9, 1, 5, 2, 2, 12, 5),
    M = c(10, 5, 7, 3, 5, 18, 5),
    P = c(11, 10, 9, 4, 6, 28, 9),
    stringsAsFactors =  FALSE
  )
  result <- PERT(dt)
  expect_lt(abs(38.66667 - result$critical.path.length), 0.001)
  expect_lt(abs(38.66667 - sum(result$means)), 0.001)
})

test_that("PERT - confidence interval",{
  testStatus("PERT - confidence interval")
  dt <- data.frame(
    Activity = c("A", "B", "C", "D", "E", "F", "G"),
    Dependency = c("-", "-", "A, B", "B,C", "C", "D", "A,B, D "),
    O = c(9, 1, 5, 2, 2, 12, 5),
    M = c(10, 5, 7, 3, 5, 18, 5),
    P = c(11, 10, 9, 4, 6, 28, 9),
    stringsAsFactors =  FALSE
  )
  result <- PERT(dt)
  z1 <- sum(result$means) + qnorm(0.05/2) * sqrt(sum(result$variances))
  z2 <- sum(result$means) + qnorm(1 - 0.05/2) * sqrt(sum(result$variances))
  expect_equal(result$confidence95[1], z1)
  expect_equal(result$confidence95[2], z2)
})










