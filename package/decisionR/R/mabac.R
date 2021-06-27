#' @name mabac
#' @title Multi-Attributive Border Approximation area Comparison Method
#' @description Apply Multi-Attributive Border Approximation area Comparison
#' (mabac) method
#' for a given decision matrix, weight vector, and vector of functions
#' with elements either min or max.
#' @param mat Decision matrix
#' @param w Weights
#' @param funcs Vector of functions with elements either min or max
#' @return Returns the results list
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' tol <- 0.10
#' decmat <-
#'     rbind(
#'         c(2, 1, 4, 7, 6, 6, 7, 3000),
#'         c(4, 1, 5, 6, 7, 7, 6, 3500),
#'         c(3, 2, 6, 6, 5, 6, 8, 4000),
#'         c(5, 1, 5, 7, 6, 7, 7, 3000),
#'         c(4, 2, 5, 6, 7, 7, 6, 3000),
#'         c(3, 2, 6, 6, 6, 6, 6, 3500)
#'     )
#' weights <- c(0.293, 0.427, 0.067, 0.027, 0.053, 0.027, 0.053, 0.053)
#'
#' fns <- c(max, max, max, max, max, max, max, min)
#'
#' result <- mabac(decmat, weights, fns)
mabac <- function(mat, w, funcs = NULL) {
    if (sum(w) != 1) {
        w <- w / sum(w)
    }

    n <- dim(mat)[1]
    p <- dim(mat)[2]

    if (is.null(funcs)) {
        funcs <- rep(max, p)
    }

    col_max <- apply(mat, 2, max)
    col_min <- apply(mat, 2, min)

    A <- matrix(0, nrow = n, ncol = p)

    for (i in 1:n) {
        for (j in 1:p) {
            if (identical(funcs[[i]], max)) {
                A[i, j] <- (mat[i, j] - col_min[j]) / (col_max[j] - col_min[j])
            } else if (identical(funcs[[i]], min)) {
                A[i, j] <- (mat[i, j] - col_max[j]) / (col_min[j] - col_max[j])
            } else {
                stop(sprintf("Function %s is not defined", funcs[i]))
            }
        }
    }


    w_a <- t(t(A + 1.0) * w)


    g <- apply(
        w_a, 2,
        geometricmean
    )


    Q <- apply(w_a, 1, function(x) {
        return(x - g)
    })

    Q <- t(Q)

    scores <- rowSums(Q)

    rankings <- order(scores)

    bestindex <- rankings[n]

    return(
        list(
            scores = scores,
            rankings = rankings,
            bestindex = bestindex
        )
    )
}