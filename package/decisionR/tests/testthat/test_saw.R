library(testthat)

test_that("SAW -  Simple Additive Weighting Method", {
    tol <- 0.0001
    df <- data.frame(
        c1 = c(25.0, 21, 19, 22),
        c2 = c(65.0, 78, 53, 25),
        c3 = c(7.0, 6, 5, 2),
        c4 = c(20.0, 24, 33, 31)
    )
    weights <- c(0.25, 0.25, 0.25, 0.25)
    fns <- c(max, max, min, max)
    result <- saw(df, weights, fns)

    expect_equal(
        result$scores, c(0.681277, 0.725151, 0.709871, 0.784976),
        tolerance = tol
    )
})