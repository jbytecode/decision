getMaxDuration <- function(dat, i){
  strActivities <- toString(dat$Dependency[i])
  listActivities <- unlist(strsplit(strActivities, ","))
  listActivities <- sapply(listActivities, function(s){
    return(gsub(" ", "", s, fixed = TRUE))
  })
  amax <- 0
  for (e in listActivities){
    row.index <- which(dat$Activity == e)
    current.duration <- dat$End[row.index]
    if(current.duration > amax){
      amax <- current.duration
    }
  }
  return(amax)
}

getPrevious <- function(dat, current){
  i <- which(dat$Activity == current)
  strActivities <- toString(dat$Dependency[i])
  listActivities <- unlist(strsplit(strActivities, ","))
  listActivities <- sapply(listActivities, function(s){
    return(gsub(" ", "", s, fixed = TRUE))
  })
  amax <- 0
  anc <- "-"
  for (e in listActivities){
    row.index <- which(dat$Activity == e)
    if(length(row.index) < 1){
      return("-")
    }
    current.duration <- dat$End[row.index]
    if(current.duration > amax){
      amax <- current.duration
      anc <- dat$Activity[row.index]
    }
  }
  return(anc)
}

CPM <- function(dat){
  n <- dim(dat)[1]
  start <- rep(0, n)
  end <- rep(0, n)
  dat[["Start"]] <- start
  dat[["End"]] <- end
  
  for (i in 1:n){
    # Activity has no dependency
    if(dat$Dependency[i] == "-"){
      dat$Start[i] <- 0
      dat$End[i] <- dat$Duration[i]
    }else{ # Activity has dependencies
      amax <- getMaxDuration(dat, i)
      dat$Start[i] <- amax
      dat$End[i] <- amax + dat$Duration[i]
    }
  }
  
  # Find Critical Path
  cp <- c()
  max.end <- order(dat$End, decreasing = TRUE)[1]
  current <- dat$Activity[max.end]
  cp <- c(cp, current)
  while(TRUE){
    prev <- getPrevious(dat, current)
    current <- prev
    if(prev == "-"){
      break
    }
    cp <- c(cp, current)
  }
  path.length <- max(dat$End)
  result <- list(
    data = dat,
    critical.path = cp,
    critical.path.length = path.length
  )
  return(result)
}




PERT <- function(dat){
  n <- dim(dat)[1]
  start <- rep(0, n)
  end <- rep(0, n)
  dat[["Start"]] <- start
  dat[["End"]] <- end
  dat[["Duration"]] <- (dat[["O"]] + 4 * dat[["M"]] + dat[["P"]]) / 6
  dat[["Variance"]] <- ((dat[["O"]] - dat[["P"]]) / 6) ^ 2
  
  for (i in 1:n){
    # Activity has no dependency
    if(dat$Dependency[i] == "-"){
      dat$Start[i] <- 0
      dat$End[i] <- dat$Duration[i]
    }else{ # Activity has dependencies
      amax <- getMaxDuration(dat, i)
      dat$Start[i] <- amax
      dat$End[i] <- amax + dat$Duration[i]
    }
  }
  
  # Find Critical Path
  cp <- c()
  max.end <- order(dat$End, decreasing = TRUE)[1]
  current <- dat$Activity[max.end]
  cp <- c(cp, current)
  while(TRUE){
    prev <- getPrevious(dat, current)
    current <- prev
    if(prev == "-"){
      break
    }
    cp <- c(cp, current)
  }
  path.length <- max(dat$End)
  critical.path.indices <- which(dat$Activity %in% cp)
  variances <- dat$Variance[critical.path.indices]
  means <- dat$Duration[critical.path.indices]
  means.sum <- sum(means)
  variances.sum <- sum(variances)
  conf95 <- c(
    means.sum + qnorm(0.05/2) * sqrt(variances.sum),
    means.sum + qnorm(1-0.05/2) * sqrt(variances.sum))
  conf99 <- c(
    means.sum + qnorm(0.01/2) * sqrt(variances.sum),
    means.sum + qnorm(1-0.01/2) * sqrt(variances.sum)
  
  result <- list(
    data = dat,
    critical.path = cp,
    critical.path.length = path.length,
    means = means,
    variances = variances,
    confidence95 = conf95,
    confidence99 = conf99
  )
  return(result)
}


