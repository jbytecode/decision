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

maximin <- function(mat){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  row.mins <- apply(mat, 1, function(x){
    return (min(x))
  })
  best.strategy <- which(row.mins == max(row.mins))
  return(list(
    row.mins = row.mins,
    best.strategy = best.strategy
  ))
}

maximax <- function(mat){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  row.max <- apply(mat, 1, function(x){
    return (max(x))
  })
  best.strategy <- which(row.max == max(row.max))
  return(list(
    row.max = row.max,
    best.strategy = best.strategy
  ))
}


savage <- function(mat){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  col.max <- apply(mat, 2, function(x){
    return (max(x))
  })
  new.mat <- mat
  for (i in 1:p){
    new.mat[,i] <- col.max[i] - new.mat[,i]
  }
  row.max <- apply(new.mat, 1, function(x){
    return(max(x))
  })
  best.strategy <- which(row.max == min(row.max))
  return(
    list(
      regret.matrix = new.mat,
      best.strategy = best.strategy
    )
  )
}

mat <- matrix(c(
  26, 26, 18, 22,
  22, 34, 30, 18,
  28, 24, 34, 26,
  22, 30, 28, 20
), nrow = 4, byrow = TRUE)
print(savage(mat))