getMaxDuration <- function(dat, i){
  strActivities <- toString(dat$Dependency[i])
  listActivities <- unlist(strsplit(strActivities, ","))
  listActivities <- sapply(listActivities, function(s){
    return(gsub(" ", "", s, fixed = TRUE))
  })
  amax <- 0
  for (e in listActivities){
    row.index <- which(dat$Activity == e)
    current.duration <- dat$end[row.index]
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
    current.duration <- dat$end[row.index]
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
  dat[["start"]] <- start
  dat[["end"]] <- end
  
  for (i in 1:n){
    # Activity has no dependency
    if(dat$Dependency[i] == "-"){
      dat$start[i] <- 0
      dat$end[i] <- dat$Duration[i]
    }else{ # Activity has dependencies
      amax <- getMaxDuration(dat, i)
      dat$start[i] <- amax
      dat$end[i] <- amax + dat$Duration[i]
    }
  }
  
  # Find Critical Path
  cp <- c()
  max.end <- order(dat$end, decreasing = TRUE)[1]
  current <- dat$Activity[max.end]
  cp <- c(cp, current)
  while(TRUE){
    prev <- getPrevious(dat, current)
    current <- prev
    cp <- c(cp, current)
    if(prev == "-"){
      break
    }
  }
  path.length <- max(dat$end)
  result <- list(
    data = dat,
    critical.path = cp,
    critical.path.length = path.length
  )
  return(result)
}
