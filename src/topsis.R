source("library.R")

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


