library("Rglpk")

knapsack_problem <- function(profit, weights, capacity){
  n <- length(profit)
  result <- Rglpk_solve_LP(
    obj = profit,
    mat = matrix(weights, nrow = 1), 
    dir = rep("<=", 1),
    rhs = capacity,
    types = rep("B", n), 
    max = TRUE
  )
  return(
    list(
      total.profit = result$optimum,
      solution = result$solution,
      total.weights.used = sum(weights[which(result$solution == 1)])
    )
  )
}

