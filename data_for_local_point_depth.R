## 1. bin_est is the output of the function bin_estimation
## bin_est is a list of denseGrid, standard_Lt, and subject_Ly.
## where standard_Lt is a list of length n, and each list shows the index at the standard grid.
## subject_Ly is a list of length n, each list shows the observation corresponding to standard_Lt
## 2. mt_depth #c("halfspace depth", "projection depth", "skewness-adjusted projection depth",
#"directional projection depth", "simplicial depth")
packages <- c("rlist")

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

data_for_local_point_depth <- function(nbvar, sample_size, bin_est, mt_depth) {
  bin_observation <- bin_est$bin_observation
  
  st1 <- Sys.time()
  pd_opt <- lapply(1:length(bin_observation), function(num) {
    point_depth <- hdepth(x = bin_observation[[num]], 
                          z = bin_observation[[num]], 
                          options = options)
    value <- point_depth$depthZ
    names(value) <- as.numeric(rownames(bin_observation[[num]]))
    return (value)
  })
  st2 <- Sys.time()
  
  running_time_local <- difftime(st2, st1, unit = "secs")
  
  standard_time_index <- unlist(lapply(1:length(pd_opt), function(k) {
    rep(k, length(pd_opt[[k]]))
  }))
  subject <- unlist(lapply(1:length(pd_opt), function(k) {
    as.numeric(names(pd_opt[[k]]))
  }))
  
  pointdepth <- Reduce(append, pd_opt)
  
  subject_point_data <- data.frame(bin_index = standard_time_index, 
                                   subject = subject,
                                   depth = pointdepth)
  
  return (subject_point_data
    #list(subject_point_data = subject_point_data, 
    #           running_time_local = running_time_local)
    )
}
