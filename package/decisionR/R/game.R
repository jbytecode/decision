library("Rglpk")

game <- function(mat){
  m <- dim(mat)[1]
  n <- dim(mat)[2]
  result1 <- Rglpk_solve_LP(
    obj = rep(1, m),
    mat = t(mat),
    dir = rep(">=", n),
    rhs = rep(1, n),
    max = FALSE
  )
  g1 <- 1 / result1$optimum
  p1 <- g1 * result1$solution
  
  result2 <- Rglpk_solve_LP(
    obj = rep(1, n),
    mat =  mat,
    dir = rep("<=", m),
    rhs = rep(1, m),
    max = TRUE
  )
  
  g2 <- 1 / result2$optimum
  p2 <- g2 * result2$solution
  return(
    list(
      g1 = g1, 
      p.row = p1,
      g2 = g2,
      p.col = p2
    )
  )
}

