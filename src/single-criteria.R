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

minimax <- function(mat){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  row.max <- apply(mat, 1, function(x){
    return (max(x))
  })
  best.strategy <- which(row.max == min(row.max))
  return(list(
    row.max = row.max,
    best.strategy = best.strategy
  ))
}


minimin <- function(mat){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  row.mins <- apply(mat, 1, function(x){
    return (min(x))
  })
  best.strategy <- which(row.mins == min(row.mins))
  return(list(
    row.mins = row.mins,
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

hurwicz <- function(mat, alpha = 0.5){
  n <- dim(mat)[1]
  p <- dim(mat)[2]
  row.max <- apply(mat, 1, function(x){
    return (max(x))
  })
  row.min <- apply(mat, 1, function(x){
    return (min(x))
  })
  total <- alpha * row.max + (1 - alpha) * row.min
  best.strategy <- which(total == max(total))
  return(list(
    row.max = row.max,
    row.min = row.min,
    total = total,
    best.strategy = best.strategy
  ))
}

maximum.likelihood <- function(mat, weights){
  result <- mat %*% weights
  best.strategy <- which(result == max(result))
  return(
    list(
      expected.values = as.vector(result),
      best.strategy = best.strategy
    )
  )
}

expected.regret <- function(mat, weights){
  col.max <- apply(mat, 2, max)
  regret.mat <- mat
  p <- dim(mat)[2]
  for (i in 1:p){
    regret.mat[,i] <- col.max[i] - mat[,i]
  }
  result <- regret.mat %*% weights
  best.strategy <- which(result == min(result))
  return(
    list(
      expected.values = as.vector(result),
      best.strategy = best.strategy
    )
  )
}

expected.value.of.perfect.information <- function(mat, weights){
  max.like <- maximum.likelihood(mat, weights)
  max.score <- max(max.like$expected.values)
  col.max <- apply(mat, 2, max)
  expected.value.under.full.info <- col.max %*% weights
  expected.value.of.full.info <- expected.value.under.full.info - max.score
  return (
    list(
      max.score = max.score,
      expected.value.under.full.information = as.vector(expected.value.under.full.info),
      expected.value.of.perfect.information = as.vector(expected.value.of.full.info)
    )
  )
}

