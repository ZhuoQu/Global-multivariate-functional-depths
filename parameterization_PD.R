parameterization_PD <- function(PD) {
  ## PD is a list with the length n
  ## each sublist contains argvals, subj and y with |argvals| * nbvar.
  nbvar <- ncol(PD[[1]]$y)
  sample_size <- length(PD)
  
  new_PD <- lapply(1:sample_size, function(k) {
    argvals <- PD[[k]]$argvals
    subj <- PD[[k]]$subj
    y <- PD[[k]]$y
    rownames(y) <- rep(k, length(argvals))
    time_percentage <- (argvals - min(argvals)) / (max(argvals) - min(argvals))
    
    result <- list(argvals = argvals,
                   subj = subj,
                   y = y,
                   time_percentage = time_percentage)
    return (result)
  })
  return (new_PD)
}

## the estimation of the mean and the pointwise depth will depends on the parameterization