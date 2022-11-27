

source("data_for_local_point_depth.R")
source("integrated_depth.R")

multi_poifd <- function(param, standard_grid, n, nbvar) {
  
    subject_Ly <- lapply(param, function(k) {k$y})
  
    Lt <- lapply(1:length(param), function(k){
      values <- param[[k]]$argvals
      names(values) <- rep(k, length(values))
      return (values)
    })
  
  obsGrid <- unlist(Lt)
  Ly <- list.rbind(subject_Ly)
  rownames(Ly) <- names(obsGrid)
  
  bin_obs_index <- lapply(1:length(standard_grid), function(point) {
    if (point < length(standard_grid)) {
      index1 <- intersect(which((obsGrid - standard_grid[point]) >= 0),
                          which((obsGrid -standard_grid[point]) <= 0.5 * (standard_grid[point+1]-standard_grid[point])))
    } else {
      index1 <- c()
    }
    if (point > 1) {
      index2 <- intersect(which((obsGrid - standard_grid[point]) <= 0),
                          which((standard_grid[point] - obsGrid) <= 0.5 * (standard_grid[point]-standard_grid[point - 1])))
    } else {
      index2 <- c()
    }
    index <- unique(c(index1, index2))
  })
  
  standard_grid_index <- unlist(sapply(1:length(bin_obs_index), function(k) {
    if (length(bin_obs_index[[k]]) > 0) {
      return (k)
    }
  }))
  
  
  upd_standard_grid <- standard_grid[standard_grid_index]
  bin_observation <- lapply(standard_grid_index, function(k) {
    index <- bin_obs_index[[k]]
    value <- Ly[index, ]
    return (value)
  })
  
  weight_time <- unlist(lapply(bin_observation, length)) / n
  
  point_est <- list(denseGrid = upd_standard_grid, 
                    bin_observation = bin_observation, 
                    weight_time  = weight_time)
  
  pointdepth_dataframe <- data_for_local_point_depth(nbvar, 
                                                     sample_size = n, 
                                                     bin_est = point_est, 
                                                     mt_depth)
  
  mfhd <- integrated_depth(nbvar, point_est, pointdepth_dataframe, weight = "time")
  return (mfhd)
}

