# Naive Bayes Classifier
# P(C \ x1, x2, ..., xn) ~ P(x1 \ C) x P(x2 \ C) x ... x P(xn \ C) x P(C)

naivebayes <- function(formula, data, input){
  stringdata <- k <- as.data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  mf <- model.frame(formula, data = stringdata)
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

gaussianNaiveBayes <- function (formula, data, input) 
{
  mf <- model.frame(formula, data = data)
  classes <- mf[, 1]
  uniqueclasses <- unique(classes)
  inputs <- mf[, 2:dim(mf)[2]]
  n <- dim(inputs)[1]
  p <- dim(inputs)[2]
  inputnames <- names(input)
  result <- list()
  for (clz in uniqueclasses) {
    n1 <- length(which(classes == clz))
    myprob <- 1
    indices <- which(mf[, 1] == clz)
    mysubset <- mf[indices, ]
    for (i in 1:p) {
      currentName <- inputnames[i]
      indices2 <- which(mysubset[, currentName] == input[, 
                                                         currentName])
      mysubsubset <- mysubset[indices2, ]
      prob <- dnorm(as.double(input[currentName]), mean = mean(mysubset[, currentName]), sd = sd(mysubset[, currentName]))
      myprob <- myprob * prob
    }
    myprob <- myprob * (n1/n)
    result[toString(clz)] <- myprob
  }
  return(result)
}


golfdata <- data.frame(
gorunum = c(
  "Gunesli", 
  "Gunesli", 
  "Bulutlu", 
  "Yagmurlu", 
  "Yagmurlu",
  "Yagmurlu",
  "Bulutlu",
  "Gunesli",
  "Gunesli",
  "Yagmurlu",
  "Gunesli",
  "Bulutlu",
  "Bulutlu",
  "Yagmurlu")
,
sicaklik = c(
  "Sicak",
  "Sicak",
  "Sicak",
  "Ilik",
  "Soguk",
  "Soguk",
  "Soguk",
  "Ilik",
  "Soguk",
  "Ilik",
  "Ilik",
  "Ilik",
  "Sicak",
  "Ilik"
)
,
nem = c(
  "Yuksek",
  "Yuksek",
  "Yuksek",
  "Yuksek",
  "Normal",
  "Normal",
  "Normal",
  "Yuksek",
  "Normal",
  "Normal",
  "Normal",
  "Yuksek",
  "Normal",
  "Yuksek"
)
,
ruzgar = c(
  "Zayif",
  "Guclu",
  "Zayif",
  "Zayif",
  "Zayif",
  "Guclu",
  "Guclu",
  "Zayif",
  "Zayif",
  "Zayif",
  "Guclu",
  "Guclu",
  "Zayif",
  "Guclu"
)
,
karar = c(
  "Hayir",
  "Hayir",
  "Evet",
  "Evet",
  "Evet",
  "Hayir",
  "Evet",
  "Hayir",
  "Evet",
  "Evet",
  "Evet",
  "Evet",
  "Evet",
  "Hayir"
)
)
