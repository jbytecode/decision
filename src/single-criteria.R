laplace <- function(mat){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  probs <- rep(1 / p, p)
  expecteds <- apply(mat, 1, function(x){
    return (sum(x * probs))
  })
  best.strategy <- which(expecteds == max(expecteds))
  return(list(
    expected.values = expecteds,
    best.strategy = best.strategy
  ))
}

