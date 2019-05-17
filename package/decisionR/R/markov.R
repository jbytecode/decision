markov <- function(transition, state, periodAhead = 1){
  result = state
  for (i in 1:periodAhead){
    result = result %*% transition
  }
  return(as.vector(result))
}


markov_equilibrium <- function (transition, state, delta = 10^(-15), maxtry = 5000){
  converged <- TRUE
  dist <- function(p1, p2){
    return(sum((p1 - p2)^2))
  }
  k <- 1
  result.first <- markov(transition, state)
  while(TRUE){
    result.second <- markov(transition, result.first, periodAhead = 1)
    if(dist(result.second, result.first) < delta){
      converged <- TRUE
      break
    }
    result.first <- result.second
    k <- k + 1
    if (k > maxtry){
      converged <- FALSE
      break
    }
  }
  return(list(
    converged = converged,
    tries = k,
    solution = result.first
  ))
}

