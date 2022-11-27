
## 1. mean_est_result is the output of the function mean_estimation
## mean_est_result is a list with denseGrid, mu, Lt, and subject_Ly
## 2. mt_dpth #c("halfspace depth",
#"projection depth",
#"skewness-adjusted projection depth",
#"directional projection depth",
#"simplicial depth")
packages <- c("pcaPP", "spatstat.sparse", "mrfDepth")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
options <- list(type = "Rotation",
                ndir = 750,
                approx = FALSE)

data_for_global_point_depth <- function(bin_est, mt_depth) {
  bin_observation <- bin_est$bin_observation
  
  st1 <- Sys.time()
  
  standard_data_opt <- lapply(bin_observation, function(mat) {
    robust_cov_est <- covPCAproj(mat)
    center <- robust_cov_est$center
    mat_remove_mean <-  t(apply(mat, 1, function(k) {k - center}))
    cov <- robust_cov_est$cov
    invsqrtcov <- matrixinvsqrt(cov)
    value <- mat_remove_mean %*% invsqrtcov
    return (value)
  })

 st2 <- Sys.time() 
 
 running_time_standard <- difftime(st2, st1, unit = "secs")
 
 standard_time_index <- unlist(lapply(1:length(standard_data_opt), function(k) {
   rep(k, nrow(standard_data_opt[[k]]))
 }))
 subject <- unlist(lapply(1:length(standard_data_opt), function(k) {
   as.numeric(rownames(standard_data_opt[[k]]))
 }))
#### subsampling if needed
 standard_data <- list.rbind(standard_data_opt)
  thres <- ifelse(nrow(standard_data) < 1000, 
                  nrow(standard_data), 1000)
  
  if (nrow(standard_data) < thres) {
    basismat <- standard_data
  } else {
    basismat_ind <- sample(1:nrow(standard_data), thres, replace = FALSE)
    basismat <- standard_data[basismat_ind, ]
  }
  
  start_time_global <- Sys.time()
  point_depth <- hdepth(x = basismat, z = standard_data, options = options)
  pointdepth <- point_depth$depthZ
  end_time_global <- Sys.time()
  running_time_global <- difftime(end_time_global, start_time_global, unit = "secs")
  #st3 <- Sys.time()
  subject_point_data <- data.frame(bin_index = standard_time_index, 
                                   subject = subject,
                                   depth = pointdepth)
  
  return (subject_point_data
    #list(subject_point_data = subject_point_data, 
    #           running_time_standard = running_time_standard, 
    #           running_time_global = running_time_global)
    )
}
