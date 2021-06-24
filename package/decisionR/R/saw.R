#' @name saw
#' @title Simple Additive Weighting Method
#' @description Apply Single Additive Weighting (SAW) method
#' for a given decision matrix, weight vector, and vector of functions
#' with elements either min or max.
#' @param mat Decision matrix
#' @param w Weights
#' @param funcs Vector of functions with elements either min or max
#' @return Returns the results list
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' df <- data.frame(
#'     c1 = c(25.0, 21, 19, 22),
#'     c2 = c(65.0, 78, 53, 25),
#'     c3 = c(7.0, 6, 5, 2),
#'     c4 = c(20.0, 24, 33, 31)
#' )
#' weights <- c(0.25, 0.25, 0.25, 0.25)
#' fns <- c(max, max, min, max)
#' result <- saw(df, weights, fns)
saw <- function(mat, w, funcs = NULL) {
    if (sum(w) != 1) {
        w <- w / sum(w)
    }
    n <- dim(mat)[1]
    p <- dim(mat)[2]

    if (is.null(funcs)) {
        funcs <- rep(max, p)
    }

    normalized_decision_mat <- matrix(0, ncol = p, nrow = n)

    colminmax <- rep(0, p)

    for (i in 1:p) {
        colminmax[i] <- funcs[[i]](mat[, i])
        if (identical(funcs[[i]], max)) {
            normalized_decision_mat[, i] <- mat[, i] / colminmax[i]
        } else if (identical(funcs[[i]], min)) {
            normalized_decision_mat[, i] <- colminmax[i] / mat[, i]
        } else {
            stop(sprintf("Function %s is not defined", funcs[i]))
        }
    }

    weighted_mat <- t(t(normalized_decision_mat) * w)

    scores <- rowSums(weighted_mat)

    rankings <- rev(order(scores))

    best_index <- rankings[1]

    return(
        list(
            decision_matrix = mat,
            normalized_decision_mat = normalized_decision_mat,
            weight = w,
            best_index = best_index,
            rankings = rankings,
            scores = scores
        )
    )
}
