assignment_problem <- function(costmatrix){
  n <- dim(costmatrix)[1]
  n.square <- n * n
  n2 <- 2 * n
  const.mat <- matrix(0, nrow = n2, ncol = n.square)
  k <- 0 
  for (i in 1:n){
    for (j in 1:(n)){
      const.mat[i, j + k] <- 1
    }
    k <- k + n
  }
  k <- 0
  for (i in (n+1):n2){
    for (j in seq(1, n.square, n)){
      const.mat[i, j + k] <- 1
    }
    k <- k + 1
  }
  rhs <- rep(1, n2)
  ops <- rep("==", n2)
  types <- rep("B", n.square)
  obj <- rep(NA, n.square)
  k <- 1
  for (i in 1:n){
    for (j in 1:n){
      obj[k] <- costmatrix[i, j]
      k <- k + 1
    }
  }
  result <- Rglpk_solve_LP(
    obj = obj,
    mat = const.mat,
    rhs = rhs,
    dir = ops,
    types = types
  )
  return(list(
    optimum = result$optimum,
    solution = result$solution
  ))
}

