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
bin_estimation <- function(parameterization, nbvar, param_PD) {
  
  sample_size <- length(param_PD)
  
  if (length(grep("time", parameterization, fixed = TRUE)) > 0) {
    subject_Ly <- lapply(param_PD, function(k) {k$y})
    Ly <- list.rbind(subject_Ly)
  }
  
  if (parameterization == "time") {
    Lt <- lapply(1:length(param_PD), function(k){
      values <- param_PD[[k]]$argvals
      names(values) <- rep(k, length(values))
      return (values)
    })
  } 
  
  obsGrid <- unlist(Lt)
  obs_count <- sapply(Lt, function(k) {length(k)})
  
  prob_thres <- seq(0, 1, by = sample_size / length(obsGrid) )
  prob_thres[length(prob_thres)] <- 1
  denseGrid <- quantile(obsGrid, probs = prob_thres)
  
  
  bin_observation <- lapply(1:(length(denseGrid) - 1), function(bin) {
    index <- intersect(which(obsGrid < denseGrid[bin + 1]),
                       which(obsGrid >= denseGrid[bin]))
    mat <- Ly[index, ]
  })
  
  
  weight_time <- sapply(bin_observation, function(w) {
    nrow(w) / sample_size}) 
  
  return (list(denseGrid = denseGrid, 
               bin_observation = bin_observation,
               weight_time = weight_time))
}

## The output of bin_estimation is a list of denseGrid, standard_Lt, and subject_Ly.
## where standard_Lt is a list of length n, and each list shows the index at the standard grid.
## subject_Ly is a list of length n, each list shows the observation corresponding to standard_Lt at the standard grid.
