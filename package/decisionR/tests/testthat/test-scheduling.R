library("testthat")


test_that("Scheduling Problem - 2 machines", {
  A <- c(5, 2, 4, 3, 1)
  B <- c(6, 7, 8, 9, 10)
  result <- scheduling_2_machines(cbind(A, B))
  expect_equal(c(5, 2, 4, 3, 1), result$ordering)
})


test_that("Scheduling Problem - 3 machines", {
  A <- c(3, 8, 7, 5, 2)
  B <- c(3, 4, 2, 1, 5)
  C <- c(5, 8, 10, 7, 6)
  result <- scheduling_3_machines(cbind(A, B, C))
  expect_equal(c(1, 4, 5, 3, 2), result$ordering)
})



test_that("Scheduling Problem - m machines", {
  A <- c(7, 6, 5, 8)
  B <- c(5, 6, 4, 3)
  C <- c(2, 4, 5, 3)
  D <- c(3, 5, 6, 2)
  E <- c(9, 10, 8, 6)
  result <- scheduling_m_machines(cbind(A, B, C, D, E))
  expect_equal(c(1, 3, 2, 4), result$ordering)
})