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

# Topsis
topsis <- function(decisionMatrix, weights){
	A <- prepareDecisionMatrixHeaders(decisionMatrix)
	w <- weights
	if(sum(w) != 1){
	  w <- w / sum(w)
	}
	num.criterias <- dim(A)[2]
	num.alternatives <- dim(A)[1]
	newA <- apply(A, 2, normalize)
	newA.weighted <- t(w * t(newA))
	col.maxs <- apply(newA.weighted, 2, max)
	col.mins <- apply(newA.weighted, 2, min)
  distances.plus <- rep(NA, num.alternatives)
	distances.minus <- rep(NA, num.alternatives)
	scores <- rep(NA, num.alternatives)
	for (i in 1:num.alternatives){
		distances.plus[i] <- euclidean(col.maxs, newA.weighted[i,])
		distances.minus[i] <- euclidean(col.mins, newA.weighted[i,])
		scores[i] <- distances.minus[i] / (distances.minus[i] + distances.plus[i])
	}
	best.index <- order(scores, decreasing = TRUE)[1]
	best.alternative <- rownames(A)[best.index]
	return(
		list(
		  decision.matrix = A,
		  weights = w,
			scores = scores,
			best = best.alternative,
			best.index = best.index
		)
	)
}


