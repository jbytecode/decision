euclidean <- function(p1, p2){
  return(sqrt(sum((p2 - p1)^2)))
}

euclideanToOrigin <- function(p1){
  return (euclidean(p1, rep(0, length(p1))))
}

normalize <- function(vector){
  return(vector / euclideanToOrigin(vector))
}

prepareDecisionMatrixHeaders <- function(A){
  n <- dim(A)[1]
  p <- dim(A)[2]
  if(is.null(colnames(A))){
    cn <- rep("", p)
    for (i in 1:p){
      cn[i] <- paste0("Criteria ", toString(i))
    }
    colnames(A) <- cn
  }
  if(is.null(rownames(A))){
    rn <- rep("", n)
    for (i in 1:n){
      rn[i] <- paste0("Alternative ", toString(i))
    }
    rownames(A) <- rn
  }
  return(A)
}

