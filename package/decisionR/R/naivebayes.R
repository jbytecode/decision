# Naive Bayes Classifier
# P(C \ x1, x2, ..., xn) ~ P(x1 \ C) x P(x2 \ C) x ... x P(xn \ C) x P(C)

naivebayes <- function(formula, data, input){
  mf <- model.frame(formula)
  classes <- mf[,1]
  uniqueclasses <- unique(classes)
  inputs <- mf[,2:dim(mf)[2]]
  n <- dim(inputs)[1]
  p <- dim(inputs)[2]
  inputnames <- names(input)
  result <- list()
  for (clz in uniqueclasses){
    n1 <- length(which(classes == clz))
    myprob <- 1
    indices <- which(mf[,1] == clz)
    mysubset <- mf[indices,]
    for (i in 1:p){
      currentName <- inputnames[i]
      indices2 <- which(mysubset[,currentName] == input[,currentName])
      mysubsubset <- mysubset[indices2,]
      prob <- length(indices2) / n1
      myprob <- myprob * prob
    }
    myprob <- myprob * (n1 / n)
    result[toString(clz)] <- myprob
    #cat(clz, " -> ",  myprob, "\n")
  }
  return(result)
}
