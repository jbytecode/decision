library("testthat")


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


test_that("Test Ahp Consistency - consistent",{
  K <- matrix(c(
    1,7,1/5,1/8,1/2,1/3,1/5,1,
    1/7,1,1/8,1/9,1/4,1/5,1/9,1/8,
    5,8,1,1/3,4,2,1,1,
    8,9,3,1,7,5,3,3,
    2,4,1/4,1/7,1,1/2,1/5,1/5,
    3,5,1/2,1/5,2,1,1/3,1/3,
    5,9,1,1/3,5,3,1,1,
    1,8,1,1/3,5,3,1,1
  ), nrow = 8, byrow = TRUE)
  result <- ahp.consistency(K)
  expect_equal(result$consistent, TRUE)
})

test_that("Test Ahp Consistency - CR",{
  K <- matrix(c(
    1,7,1/5,1/8,1/2,1/3,1/5,1,
    1/7,1,1/8,1/9,1/4,1/5,1/9,1/8,
    5,8,1,1/3,4,2,1,1,
    8,9,3,1,7,5,3,3,
    2,4,1/4,1/7,1,1/2,1/5,1/5,
    3,5,1/2,1/5,2,1,1/3,1/3,
    5,9,1,1/3,5,3,1,1,
    1,8,1,1/3,5,3,1,1
  ), nrow = 8, byrow = TRUE)
  result <- ahp.consistency(K)
  expect_equal(result$CR, 0.0736, tolerance = 1 / 1000)
})


test_that("Test Ahp Consistency - CI",{
  K <- matrix(c(
    1,7,1/5,1/8,1/2,1/3,1/5,1,
    1/7,1,1/8,1/9,1/4,1/5,1/9,1/8,
    5,8,1,1/3,4,2,1,1,
    8,9,3,1,7,5,3,3,
    2,4,1/4,1/7,1,1/2,1/5,1/5,
    3,5,1/2,1/5,2,1,1/3,1/3,
    5,9,1,1/3,5,3,1,1,
    1,8,1,1/3,5,3,1,1
  ), nrow = 8, byrow = TRUE)
  result <- ahp.consistency(K)
  expect_equal(result$CI, 0.1038, tolerance = 1 / 1000)
})

test_that("Test Ahp Consistency - Lambda max",{
  K <- matrix(c(
    1,7,1/5,1/8,1/2,1/3,1/5,1,
    1/7,1,1/8,1/9,1/4,1/5,1/9,1/8,
    5,8,1,1/3,4,2,1,1,
    8,9,3,1,7,5,3,3,
    2,4,1/4,1/7,1,1/2,1/5,1/5,
    3,5,1/2,1/5,2,1,1/3,1/3,
    5,9,1,1/3,5,3,1,1,
    1,8,1,1/3,5,3,1,1
  ), nrow = 8, byrow = TRUE)
  result <- ahp.consistency(K)
  expect_equal(result$lambda.max, 8.7264, tolerance = 1 / 1000)
})


test_that("Test Ahp Consistency - p / c vector",{
  K <- matrix(c(
    1,7,1/5,1/8,1/2,1/3,1/5,1,
    1/7,1,1/8,1/9,1/4,1/5,1/9,1/8,
    5,8,1,1/3,4,2,1,1,
    8,9,3,1,7,5,3,3,
    2,4,1/4,1/7,1,1/2,1/5,1/5,
    3,5,1/2,1/5,2,1,1/3,1/3,
    5,9,1,1/3,5,3,1,1,
    1,8,1,1/3,5,3,1,1
  ), nrow = 8, byrow = TRUE)
  result <- ahp.consistency(K)
  expect_equal(result$pc.matrix, c(8.409822, 8.227924, 8.952010,
                                   8.848075, 8.860427, 8.941498,
                                   8.946071, 8.625595), tolerance = 1 / 1000)
})



test_that("Test Ahp - Big example",{
  K <- matrix(c(
    1,7,1/5,1/8,1/2,1/3,1/5,1,
    1/7,1,1/8,1/9,1/4,1/5,1/9,1/8,
    5,8,1,1/3,4,2,1,1,
    8,9,3,1,7,5,3,3,
    2,4,1/4,1/7,1,1/2,1/5,1/5,
    3,5,1/2,1/5,2,1,1/3,1/3,
    5,9,1,1/3,5,3,1,1,
    1,8,1,1/3,5,3,1,1
  ), nrow = 8, byrow = TRUE)
  candidateComparisonMatrixList <- list(
    A1 = matrix(c(1,3,1/5,2,
                  1/3,1,1/7,1/3,
                  5,7,1,4,
                  1/2,3,1/4,1), nrow = 4, byrow = TRUE),
    A2 = matrix(c(1,1/2,4,5,
                  2,1,6,7,
                  1/4,1/6,1,3,
                  1/5,1/7,1/3,1), nrow = 4, byrow = TRUE),
    A3 = matrix(c(1,1/2,1/6,3,
                  2,1,1/4,5,
                  6,4,1,9,
                  1/3,1/5,1/9,1), nrow = 4, byrow = TRUE),
    A4 = matrix(c(1,7,1/4,2,
                  1/7,1,1/9,1/5,
                  4,9,1,5,
                  1/2,5,1/5,1), nrow = 4, byrow = TRUE),
    A5 = matrix(c(1,6,2,3,
                  1/6,1,1/4,1/3,
                  1/2,4,1,2,
                  1/3,3,1/2,1), nrow = 4, byrow = TRUE),
    A6 = matrix(c(1,1/4,1/2,1/7,
                  4,1,2,1/3,
                  2,1/2,1,1/5,
                  7,3,5,1), nrow = 4, byrow = TRUE),
    A7 = matrix(c(1,3,7,1,
                  1/3,1,4,1/3,
                  1/7,1/4,1,1/7,
                  1,3,7,1), nrow = 4, byrow = TRUE),
    A8 = matrix(c(1,2,5,8,
                  1/2,1,3,6,
                  1/5,1/3,1,3,
                  1/8,1/6,1/3,1), nrow = 4, byrow = TRUE)
  )
  result <- ahp(candidateComparisonMatrixList, K)
  expect_equal(result$best.index, 3)
  expect_equal(result$best, "Alternative 3")
  expect_equal(as.vector(result$ordering.result), 
               c(0.2801050, 0.1482273, 0.3813036, 0.1903641), tolerance = 1 / 100)
})


test_that("Test Dematel - Threshold",{
  K <- matrix(c(0,3,0,2,0,0,0,0,3,0,
                3,0,0,0,0,0,0,0,0,2,
                4,1,0,2,1,3,1,2,3,2,
                4,1,4,0,1,2,0,1,0,0,
                3,2,3,1,0,3,0,2,0,0,
                4,1,4,4,0,0,0,1,1,3,
                3,0,0,0,0,2,0,0,0,0,
                3,0,4,3,2,3,1,0,0,0,
                4,3,2,0,0,1,0,0,0,2,
                2,1,0,0,0,0,0,0,3,0), nrow = 10, byrow = TRUE)
  result <- dematel(K)
  expect_equal(result$threshold, 0.0629, tolerance = 1 / 1000)
})


test_that("Test Dematel - c vector",{
  K <- matrix(c(0,3,0,2,0,0,0,0,3,0,
                3,0,0,0,0,0,0,0,0,2,
                4,1,0,2,1,3,1,2,3,2,
                4,1,4,0,1,2,0,1,0,0,
                3,2,3,1,0,3,0,2,0,0,
                4,1,4,4,0,0,0,1,1,3,
                3,0,0,0,0,2,0,0,0,0,
                3,0,4,3,2,3,1,0,0,0,
                4,3,2,0,0,1,0,0,0,2,
                2,1,0,0,0,0,0,0,3,0), nrow = 10, byrow = TRUE)
  result <- dematel(K)
  expect_equal(result$c, c(0.3991458, 0.2261648, 1.0204318, 0.7538625,
                           0.8096760, 0.9780926, 0.2717874,
                           0.9455390, 0.5960514, 0.2937537), tolerance = 1 / 1000)
})


test_that("Test Dematel - r vector",{
  K <- matrix(c(0,3,0,2,0,0,0,0,3,0,
                3,0,0,0,0,0,0,0,0,2,
                4,1,0,2,1,3,1,2,3,2,
                4,1,4,0,1,2,0,1,0,0,
                3,2,3,1,0,3,0,2,0,0,
                4,1,4,4,0,0,0,1,1,3,
                3,0,0,0,0,2,0,0,0,0,
                3,0,4,3,2,3,1,0,0,0,
                4,3,2,0,0,1,0,0,0,2,
                2,1,0,0,0,0,0,0,3,0), nrow = 10, byrow = TRUE)
  result <- dematel(K)
  expect_equal(result$r, c(1.5527024, 0.7251791, 0.8551461, 0.6895615,
                           0.2059141, 0.6790404, 0.1057168,
                           0.3163574, 0.6484014, 0.5164858), tolerance = 1 / 1000)
})


test_that("Test Dematel - Influence matrix",{
  K <- matrix(c(0,3,0,2,0,0,0,0,3,0,
                3,0,0,0,0,0,0,0,0,2,
                4,1,0,2,1,3,1,2,3,2,
                4,1,4,0,1,2,0,1,0,0,
                3,2,3,1,0,3,0,2,0,0,
                4,1,4,4,0,0,0,1,1,3,
                3,0,0,0,0,2,0,0,0,0,
                3,0,4,3,2,3,1,0,0,0,
                4,3,2,0,0,1,0,0,0,2,
                2,1,0,0,0,0,0,0,3,0), nrow = 10, byrow = TRUE)
  result <- dematel(K)
  expect_equal(as.vector(result$influence.matrix), c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                           1, 0, 1, 1, 1, 1, 0, 0, 1, 0,
                           0, 0, 0, 1, 1, 1, 0, 1, 1, 0,
                           1, 0, 1, 0, 1, 1, 0, 1, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
                           0, 0, 1, 1, 1, 0, 1, 1, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 1, 0, 1, 0, 0, 0, 0, 0,
                           1, 0, 1, 0, 0, 1, 0, 0, 0, 1,
                           0, 1, 1, 0, 0, 1, 0, 0, 1, 0), tolerance = 1 / 1000)
})


