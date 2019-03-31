#' @name euclidean
#' @title Euclidean distance between two vectors
#' @description This function simply returns the euclidean distance between two vectors.
#' @param p1 First n-vector
#' @param p2 Second n-vector
#' @return Euclidean distance
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' x1 <- c(0,0,0)
#' x2 <- c(4,3,2)
#' distance <- euclidean(x1, x2)
#' print(distance)
euclidean <- function(p1, p2){
  return(sqrt(sum((p2 - p1)^2)))
}



#' @name euclideanToOrigin
#' @title Euclidean distance between origin and an n-vector
#' @description This function is a wrapper for \code{euclidean} function in which the first argument
#' is always origin of euclidean n-space
#' @param p1 An n-vector
#' @return Euclidean distance
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' x1 <- c(4,3,2)
#' distance <- euclideanToOrigin(x1)
#' print(distance)
#' print(euclidean(x1, c(0,0,0)) == euclideanToOrigin(x1))
euclideanToOrigin <- function(p1){
  return (euclidean(p1, rep(0, length(p1))))
}

#' @name normalize
#' @title Normalizes a vector
#' @description Normalizes a vector by dividing all of the elements in the vector by the vector norm.
#' Vector norm is the euclidean distance of the vector with the origin. Squared elements of the vector
#'  sum up to 1.
#' @param vector A vector
#' @return Returns the normalized vector
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' x1 <- c(4,3,2)
#' x1.normalized <- normalize(x1)
#' print(x1.normalized)
#' print(sum(x1.normalized ^ 2) == 1)
normalize <- function(vector){
  return(vector / euclideanToOrigin(vector))
}

#' @name prepareDecisionMatrixHeaders
#' @title prepareDecisionMatrixHeaders
#' @description This function prepares the matrix headers for a given decision matrix.
#' The column names are set to Criateri 1, Criteria 2, ..., Criteria M and 
#' the row names are set to Alternative 1, Alternative 2, ..., Alternative N where 
#' M is the number of criteria and N is the number of alternatives. This function is not generally
#' called by the user. The headers are set in the multiple-criteria decision making tools such as Topsis. 
#' @param A Decision matrix
#' @return Same decision matrix with row and column names
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' x <- matrix(1:30, nrow = 5, ncol = 6)
#' decision.matrix <- prepareDecisionMatrixHeaders(x)
#' print(decision.matrix)
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

#' @name topsis
#' @title Topsis Method
#' @description This function implements the Topsis method for the multiple-criteria decision problem.
#' @param A Decision matrix
#' @param weights Weights for criteria (Measure of how important the criteria are.)
#' @return A list of results.
#' @references Celikbilek Yakup, Cok Kriterli Karar Verme Yontemleri, Aciklamali ve Karsilastirmali
#' Saglik Bilimleri Uygulamalari ile. Editor: Muhlis Ozdemir, Nobel Kitabevi, Ankara, 2018
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
#' W <- c(4, 2, 6, 8)
#' w <- W / sum(W)
#' result <- topsis(A, w)
#' print(result)
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
		  decision.matrix.weighted = newA.weighted,
		  weights = w,
			scores = scores,
			best = best.alternative,
			best.index = best.index
		)
	)
}


#' @name vikor
#' @title Vikor Method
#' @description This function implements the Vikor method for the multiple-criteria decision problem.
#' @param A Decision matrix
#' @param weights Weights for criteria (Measure of how important the criteria are.)
#' @param v Another weight parameter which is defined in range 0 < v < 1 and set to 0.5 by default.
#' @return A list of results.
#' @references Celikbilek Yakup, Cok Kriterli Karar Verme Yontemleri, Aciklamali ve Karsilastirmali
#' Saglik Bilimleri Uygulamalari ile. Editor: Muhlis Ozdemir, Nobel Kitabevi, Ankara, 2018
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
#' W <- c(4, 2, 6, 8)
#' w <- W / sum(W)
#' result <- vikor(A, w)
#' print(result)
vikor <- function(decisionMatrix, weights, v = 0.5){
  A <- prepareDecisionMatrixHeaders(decisionMatrix)
  w <- weights
  if(sum(w) != 1){
    w <- w / sum(w)
  }
  num.criterias <- dim(A)[2]
  num.alternatives <- dim(A)[1]
  col.max <- apply(A, 2, max)
  col.min <- apply(A, 2, min)
  
  for (i in 1:num.alternatives){
    for (j in 1:num.criterias){
      A[i, j] <- abs((col.max[j] - A[i, j])/(col.max[j] - col.min[j]))
    }
  }
  newA.weighted <- t(w * t(A))
  s <- rep(NA, num.alternatives)
  r <- rep(NA, num.alternatives)
  q <- rep(NA, num.alternatives)
  for (i in 1:num.alternatives){
    s[i] <- sum(newA.weighted[i,])
    r[i] <- max(newA.weighted[i,])
  }
  q <- v * ((s - min(s)) / (max(s) - min(s))) + (1 - v) * ((r - min(r)) / (max(r) - min(r)))
  best.index <- order(q, decreasing = FALSE)[1]
  best.alternative <- rownames(A)[best.index]
  return(
    list(
      decision.matrix = A,
      decision.matrix.weighted = newA.weighted,
      weights = w,
      scores = q,
      best = best.alternative,
      best.index = best.index
    )
  )
}

#' @name electre
#' @title Electre Method
#' @description This function implements the Electre method for the multiple-criteria decision problem.
#' @param A Decision matrix
#' @param weights Weights for criteria (Measure of how important the criteria are.)
#' @return A list of results.
#' @references Celikbilek Yakup, Cok Kriterli Karar Verme Yontemleri, Aciklamali ve Karsilastirmali
#' Saglik Bilimleri Uygulamalari ile. Editor: Muhlis Ozdemir, Nobel Kitabevi, Ankara, 2018
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
#' W <- c(4, 2, 6, 8)
#' w <- W / sum(W)
#' result <- electre(A, w)
#' print(result)
electre <- function(decisionMatrix, weights){
  A <- prepareDecisionMatrixHeaders(decisionMatrix)
  w <- weights
  if(sum(w) != 1){
    w <- w / sum(w)
  }
  num.criterias <- dim(A)[2]
  num.alternatives <- dim(A)[1]
  newA <- apply(A, 2, normalize)
  newA.weighted <- t(w * t(newA))
  fitness.table <- list()
  nonfitness.table <- list()
  for (i in 1:num.alternatives){
    for (j in 1:num.alternatives){
      if(i != j){
      better.list <- c()
      worse.list <- c()
      for (h in 1:num.criterias){
        if(newA.weighted[i, h] >= newA.weighted[j, h]){
          better.list <- append(better.list, h)
        }else{
          worse.list <- append(worse.list, h)
        }
      }
      fitness.table.element <- list(i = i, j = j, set = better.list)
      fitness.table[[length(fitness.table) + 1]] <- fitness.table.element
      
      nonfitness.table.element <- list(i = i, j = j, set = worse.list)
      nonfitness.table[[length(nonfitness.table) + 1]] <- nonfitness.table.element
      }
    }
  }
  
  fitness.matrix <- matrix(NA, ncol = num.alternatives, nrow = num.alternatives)
  nonfitness.matrix <- matrix(NA, ncol = num.alternatives, nrow = num.alternatives)
  for (elements in fitness.table){
    i <- elements[["i"]]
    j <- elements[["j"]]
    elem.list <- elements[["set"]]
    
    CC <- sum(w[elem.list])
    fitness.matrix[i, j] <- CC
  }
  
  nonfitness.matrix <- matrix(NA, ncol = num.alternatives, nrow = num.alternatives)
  for (elements in nonfitness.table){
    i <- elements[["i"]]
    j <- elements[["j"]]
    elem.list <- elements[["set"]]
    
    r.ik <- newA.weighted[i, elem.list]
    r.jk <- newA.weighted[j, elem.list]
    r.ik.full <- newA.weighted[i, ]
    r.jk.full <- newA.weighted[j, ]
    nom <- max(abs(r.ik - r.jk))
    dom <- max(abs(r.ik.full - r.jk.full))
    nonfitness.matrix[i, j] <- nom / dom
  }
  
  C <- rep(NA, num.alternatives)
  D <- rep(NA, num.alternatives)
  for (i in 1:num.alternatives){
    C[i] <- sum(fitness.matrix[i, ], na.rm = TRUE) - sum(fitness.matrix[ ,i], na.rm = TRUE)
    D[i] <- sum(nonfitness.matrix[i, ], na.rm = TRUE) - sum(nonfitness.matrix[ ,i], na.rm = TRUE)
  }
  best.C.index <- order(C, decreasing = TRUE)[1]
  best.D.index <- order(D, decreasing = FALSE)[1]
  if(best.C.index == best.D.index){
    best <- row.names(A)[best.C.index]
  }else{
    best <- c(row.names(A)[best.C.index], row.names(A)[best.D.index])
  }
  return(
    list(
      decision.matrix = A,
      decision.matrix.weighted = newA.weighted,
      weights = w,
      fitness.table,
      nonfitness.table,
      fitness.matrix,
      nonfitness.matrix,
      C = C, 
      D = D,
      best = best,
      best.index = list(C = best.C.index, D = best.D.index)
    )
  )
}

#' @name moora
#' @title Moora Method
#' @description This function implements the Moora method for the multiple-criteria decision problem.
#' @param A Decision matrix
#' @param weights Weights for criteria (Measure of how important the criteria are.)
#' @return A list of results.
#' @references Celikbilek Yakup, Cok Kriterli Karar Verme Yontemleri, Aciklamali ve Karsilastirmali
#' Saglik Bilimleri Uygulamalari ile. Editor: Muhlis Ozdemir, Nobel Kitabevi, Ankara, 2018
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
#' W <- c(4, 2, 6, 8)
#' w <- W / sum(W)
#' result <- moora(A, w)
#' print(result)
moora <- function(decisionMatrix, weights){
  A <- prepareDecisionMatrixHeaders(decisionMatrix)
  w <- weights
  if(sum(w) != 1){
    w <- w / sum(w)
  }
  num.criterias <- dim(A)[2]
  num.alternatives <- dim(A)[1]
  newA <- apply(A, 2, normalize)
  newA.weighted <- t(w * t(newA))
  col.max <- apply(newA.weighted, 2, max)
  newA.weighted.reference <- t(col.max - t(newA.weighted)) 
  row.max <- apply(newA.weighted.reference, 1, max)
  best.index <- order(row.max, decreasing = FALSE)[1]
  best <- rownames(A)[best.index]
  return(
    list(
      decision.matrix = A,
      decision.matrix.weighted = newA.weighted,
      decision.matrix.weighted.ref = newA.weighted.reference,
      weights = w,
      scores = as.double(row.max),
      best.index = best.index,
      best = best
    )
  )
}

#' @name ahp.RI
#' @title Random Index for AHP method.
#' @description This function returns returns the Random Index (RI) for a given n where n >= 3
#' @param n an integer greater or equals to 3. 
#' @return Random Index.
#' @references Celikbilek Yakup, Cok Kriterli Karar Verme Yontemleri, Aciklamali ve Karsilastirmali
#' Saglik Bilimleri Uygulamalari ile. Editor: Muhlis Ozdemir, Nobel Kitabevi, Ankara, 2018
#' @author Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
#' @examples
#' ahp.RI(3)
#' # Returns 0.58
ahp.RI <- function (n){
    # First index is n = 3, so RI[3] = 0.58
    RI <- c(0.58, 0.90, 1.12, 1.24, 1.32,
          1.41, 1.45, 1.49, 1.51, 1.53,
          1.56, 1.57, 1.59)
  if(n < 3){
    return(0)
  }else if(n <= 15){
    return (RI[n - 2])
  }else{
    return(RI[length(RI)])
  }
}

ahp.consistency <- function(CriteriaComparisonMatrix){
  n <- dim(CriteriaComparisonMatrix)[1]
  m <- n
  colSums.comparison <- apply(CriteriaComparisonMatrix, 2, sum)
  normalized.comparison <- matrix(NA, nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      normalized.comparison[i, j] <- CriteriaComparisonMatrix[i, j] / colSums.comparison[j]
    }
  }
  priority.vector <- apply(normalized.comparison, 1, mean)
  consistency.vector <- CriteriaComparisonMatrix %*% priority.vector
  pc.matrix <- consistency.vector / priority.vector
  lambda.max <- sum(pc.matrix) / n
  CI <- (lambda.max - n) / (n - 1)
  RI <- ahp.RI(n)
  CR <- CI / RI
  consistent <- (CR < 0.1)
  return(
    list(
      normalized.comparison = normalized.comparison,
      priority.vector = priority.vector,
      consistency.vector = consistency.vector,
      pc.matrix = as.vector(pc.matrix),
      lambda.max = lambda.max,
      CI = CI,
      RI = RI,
      CR = CR,
      consistent = consistent
    )
  )
}

ahp <- function(candidateComparisonMatrixList, CriteriaComparisonMatrix){
  result.list <- lapply(candidateComparisonMatrixList, ahp.consistency)
  n <- length(result.list)
  num.criterian <- dim(CriteriaComparisonMatrix)[1]
  num.candidates <- dim(candidateComparisonMatrixList[[1]])[1]
  decision.matrix <- matrix(NA, nrow = num.candidates, ncol = num.criterian)
  for (i in 1:n){
    decision.matrix[,i] <- result.list[[i]]$priority.vector
  }
  decision.matrix <- prepareDecisionMatrixHeaders(decision.matrix)
  criteria.consistency <- ahp.consistency(CriteriaComparisonMatrix)
  weights <- criteria.consistency$priority.vector
  ordering.result <- decision.matrix %*% weights
  best.index <- order(ordering.result, decreasing = TRUE)[1]
  best <- row.names(decision.matrix)[best.index]
  return(
    list(
      decision.matrix = decision.matrix,
      weights = weights,
      ordering.result = ordering.result,
      best.index = best.index,
      best = best
    )
  )
}




