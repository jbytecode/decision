scheduling_assign <- function(orderingtable, element, assignFirst = TRUE){
  if(!assignFirst){
    orderingtable <- rev(orderingtable)
  }
  for (i in 1:length(orderingtable)){
    if(is.na(orderingtable[i])){
      orderingtable[i] <- element
      break
    }
  }
  if(!assignFirst){
    orderingtable <- rev(orderingtable)
  }
  return(orderingtable)
}

scheduling_2_machines <- function(timetable){
  n <- dim(timetable)[1]
  p <- dim(timetable)[2]
  if(p != 2){
    stop("This function calculates two machine scheduling costs")
  }
  if(is.null(colnames(timetable))){
      colnames(timetable) <- c("A", "B")
  }
  ctimetable <- timetable 
  orderingtable <- rep(NA, n)
  A <- ctimetable[,1]
  B <- ctimetable[,2]
  for (i in 1:n){
    minA <- min(A, na.rm = TRUE)
    minB <- min(B, na.rm = TRUE)
    if(minA < minB){
      minindex <- which(A == minA)[1]
      orderingtable <- scheduling_assign(orderingtable, minindex, assignFirst = TRUE)
    }else{
      minindex <- which(B == minB)[1]
      orderingtable <- scheduling_assign(orderingtable, minindex, assignFirst = FALSE)
    }
    A[minindex] <- NA
    B[minindex] <- NA
    if(all(is.na(A))){
      break
    }
  }
  return(list(ordering = orderingtable))
}

