library(testthat)

test_that("MABAC - (Multi-Attributive Border Approximation area Comparison) ", {
    tol <- 0.10
    decmat <-
        rbind(
            c(2, 1, 4, 7, 6, 6, 7, 3000),
            c(4, 1, 5, 6, 7, 7, 6, 3500),
            c(3, 2, 6, 6, 5, 6, 8, 4000),
            c(5, 1, 5, 7, 6, 7, 7, 3000),
            c(4, 2, 5, 6, 7, 7, 6, 3000),
            c(3, 2, 6, 6, 6, 6, 6, 3500)
        )
    weights <- c(0.293, 0.427, 0.067, 0.027, 0.053, 0.027, 0.053, 0.053)

    fns <- c(max, max, max, max, max, max, max, min)

    result <- mabac(decmat, weights, fns)

    expect_equal(
        result$scores,
        c(-0.31132, -0.10898, 0.20035, 0.04218, 0.34452, 0.20035),
        tolerance = tol
    )
})