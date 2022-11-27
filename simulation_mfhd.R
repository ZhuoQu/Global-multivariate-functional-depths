packages <- c("mrfDepth")

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
source("bootstrap_mfpca.R")
source("data_for_local_point_depth.R")
source("integrated_depth.R")


fit_sparsedata <- function(bootstrapTimes, standard_grid,
                          true_data, 
                          sparse_data, reconstruction_type) {
  
  if ("matrix" %in% class(sparse_data)) {
    nbvar <- 1
    n <- nrow(sparse_data)
  }  else {
    nbvar <- length(sparse_data)
    n <- nrow(sparse_data[[1]])
  }
  
  if (reconstruction_type == "mfpca") {
    mfpca_full <- bootstrap_mfpca(standard_grid, true_data, 
                                sparse_data, isbootstrap = FALSE)
    fit_data <- mfpca_full$fit
  } else {
    train_bootstrap <- lapply(1:bootstrapTimes, function(bt) {
      cat(bt, "\n")
      mfpca <- bootstrap_mfpca(standard_grid, true_data, sparse_data, isbootstrap = TRUE)
      mfpca_fit <- lapply(1:nbvar, function(k) {
        temp_mat <- matrix(NA, nrow = n, ncol = length(standard_grid))
        temp_mat[mfpca$sample_index, ] <- mfpca$fit[[k]]
        return (temp_mat)
      })
      return(mfpca_fit)
    })
    
    fit_data <- lapply(1:nbvar, function(l) {
      fit <- matrix(0, nrow = n, ncol = length(standard_grid))
      for (obs in 1:n) {
        temp_mat <- t(sapply(train_bootstrap, function(k) {
          k[[l]][obs, ]}))
        fit_value <- apply(temp_mat, 2, function(w) {
          mean(w, na.rm = TRUE)})
        fit[obs, ] <- fit_value
      }
      return(fit)
    })
  }
  
  if (length(true_data) == 0) {
    true_data <- fit_data
  }
  return(fit_data)
}



simulation_mfhd <- function(fit_data, standard_grid, n, nbvar) {
 ### fit_data is a list of nbvar. In each list, it is a matrix of obs_nb * |grid|

  bin_observation <- lapply(1:length(standard_grid), function(point) {
    mat <- sapply(fit_data, function(k) {
      k[, point]
    })
    rownames(mat) <- 1:n
    return (mat)
  })
  
  
  weight_time <- rep(1, length(standard_grid))
  
  point_est <- list(denseGrid = standard_grid, 
                    bin_observation = bin_observation, 
                    weight_time  = weight_time)
  
  pointdepth_dataframe <- data_for_local_point_depth(nbvar, 
                                                     sample_size = n, 
                                                     bin_est = point_est, mt_depth)
  
  mfhd <- integrated_depth(nbvar, point_est, pointdepth_dataframe, weight = "time")
  return (mfhd)
}
