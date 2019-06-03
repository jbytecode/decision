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



scheduling_3_machines <- function(timetable){
  # if MinA >= Max B
  # or
  # minC >= max B
  # then
  # G = A + B 
  # H = B + C
  # A <- c(3,8,7,5,2)
  # B <- c(3,4,2,1,5)
  # C <- c(5,8,10,7,6)
  # ordering <- c(1,4,5,3,2)
  A <- timetable[,1]
  B <- timetable[,2]
  C <- timetable[,3]
  mina <- min(A)
  maxb <- max(B)
  minc <- min(C)
  if(mina >= maxb || minc >= maxb){
    G = A + B 
    H = B + C
    mydata <- cbind(G, H)
    colnames(mydata) <- c("G", "H")
    return(scheduling_2_machines(mydata))
  }else{
    stop("This problem can not be solved using 2-machines algorithm: minA >= maxB or minC >= maxB")
  }
}


scheduling_m_machines <- function(timetable){
  # if MinA >= Maks(B,C, ..., (M-1))
  # or
  # min M >= Maks(B,C, ..., (M-1))
  # then
  # G = A + B + C + ... + (M-1)
  # and
  # H = B + C + D + ... + M
  # A <- c(7,6,5,8)
  # B <- c(5,6,4,3)
  # C <- c(2,4,5,3)
  # D <- c(3,5,6,2)
  # E <- c(9,10,8,6)
  # ordering <- c(1,3,2,4)
  p <- dim(timetable)[2]
  n <- dim(timetable)[1]
  minA <- min(timetable[,1])
  maxRem <- max(timetable[,2:(p - 1)])
  minM <- min(timetable[,p])
  if(minA >= maxRem || minM >= maxRem){
    Gmat <- timetable[1:(p-1)]
    Hmat <- timetable[2:p]
    H <- rep(0, n)
    G <- rep(0, n)
    for (i in 1:(p-1)){
      G <- G + timetable[,i]
    }
    for (i in 2:p){
      H <- H  + timetable[,i]
    }
    mydata <- cbind(G, H)
    return (scheduling_2_machines(mydata))
  }else{
    stop("This problem can not be solved using m-machines algorithm: if MinA >= Maks(B,C, ..., (M-1)) or min M >= Maks(B,C, ..., (M-1))")
  }
}



 
