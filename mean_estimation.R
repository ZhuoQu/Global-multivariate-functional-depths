packages <- c("fdapace")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#source("parameterization_PD.R")
## param_PD (the output of parameterization_PD(PD)) is a list with the length n
## each sublist contains argvals, subj, y with |argvals| * nbvar, time_percentage
### arc_length, and arc_length_percentage
mean_estimation <- function(parameterization, nbvar, param_PD, plotfigure = TRUE) {
  sample_size <- length(param_PD)
  Ly_var <- vector(mode = "list", length = nbvar)
  for (var_ind in 1:nbvar) {
    Ly_var[[var_ind]] <- lapply(param_PD, function(k) {
      if (nbvar > 1) return (k$y[, var_ind])
      else return (k$y)})
  }
  
  if (length(grep("arc length", parameterization, fixed = TRUE)) > 0 ) {
    for (var_ind in 1:nbvar) {
      Ly_var[[var_ind]] <- lapply(param_PD, function(k) {
        if (nbvar > 1) return (k$y[-1, var_ind])
        else return (k$y[-1])})
    }
  }
  
  if (parameterization == "time") {
    Lt <- lapply(param_PD, function(k){k$argvals})
  } else if (parameterization == "time percentage") {
    Lt <- lapply(param_PD, function(k){k$time_percentage})
  } else if (parameterization == "arc length") {
    Lt <- lapply(param_PD, function(k){k$arc_length})
  } else if (parameterization == "arc length percentage") {
    Lt <- lapply(param_PD, function(k){k$arc_length_percentage})
  }
  
  obsGrid <- unlist(Lt)
  denseGrid <- unique(obsGrid)[order(unique(obsGrid))]

 
  res = lapply(Ly_var, function(Ly) {
    GetMeanCurve(Ly, Lt, optns = list(plot = FALSE))})
  for (l in 1:nbvar) {
    res[[l]]$muDense = ConvertSupport(res[[l]]$workGrid, denseGrid, mu = res[[l]]$mu)
    if (plotfigure == TRUE) {
      plot(Lt[[1]], Ly_var[[l]][[1]], type = "n", 
           ylim = range(Ly_var[[l]]), xlim = range(Lt),
           xlab = parameterization, ylab = paste("Variable ", l, sep = ""), 
           main = paste("Functions v.s. ", parameterization, sep = ""))
      lapply(1:sample_size, function(k) {
        lines(Lt[[k]], Ly_var[[l]][[k]], col = k)
        points(Lt[[k]], Ly_var[[l]][[k]], cex = 0.3)
      })
      lines(denseGrid, res[[l]]$muDense, lty = 2, lwd = 3, col = "purple")
    }
  }
  mu = sapply(res, function(k) {k$muDense})
  subject_Ly <- Ly_var
  for (l in 1:sample_size) {
    index <- which(denseGrid %in% Lt[[l]])
    subject_Ly[[l]] <- sapply(Ly_var, function(k) {k[[l]]}) - mu[index, ]
  }
  return (mean_subject_est = list(denseGrid = denseGrid, mu = mu, Lt = Lt, subject_Ly = subject_Ly))
}

