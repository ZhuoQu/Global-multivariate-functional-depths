source("mean_estimation.R")
source("bin_estimation.R")
source("data_for_global_point_depth.R")
source("data_for_local_point_depth.R")
source("integrated_depth.R")
source("extremal_depth.R")

mulitvariate_functional_depth <- function(nbvar, param_PD, 
                                          point_depth_comp = "global", 
                                          type, mt_depth, weight) {
  ## 1. nbvar is the number of variables
  ## 2. param_PD is the output of the function parameterization_PD
  ### 3. point_depth_comp is either "global" or "local"
  ## 4. type is an option of c("integrated depth", "extremal depth")
  ## 5. mt_depth is an option of multivariate depths: 
  ### c("halfspace depth", "projection depth", "skewness-adjusted projection depth", "directional projection depth",
  #"simplicial depth")")
  sample_size <- length(param_PD)
  #### To obtain the global pointwise depth, we compute the mean, obtain the residual, 
  #### and calculate the global pointwise depth for the residual data
  start_time_bin <- Sys.time()
  bin_time <- bin_estimation(parameterization = "time", nbvar, param_PD)
  end_time_bin <- Sys.time()
  ## The output of bin_estimation is a list of denseGrid, standard_Lt, and subject_Ly.
  ## where standard_Lt is a list of length n, and each list shows the index at the standard grid.
  ## subject_Ly is a list of length n, each list shows the observation corresponding to standard_Lt at the standard grid.
  if (point_depth_comp == "local") {
    pointdepth_dataframe_time <- data_for_local_point_depth(nbvar, sample_size, bin_time, mt_depth)
  }
  if (point_depth_comp == "global") {
    pointdepth_dataframe_time <- data_for_global_point_depth(bin_time, mt_depth)
    #pointdepth_dataframe_time_pcg <- data_for_global_point_depth(bin_time_pcg, mt_depth)
    #pointdepth_dataframe_arc <- data_for_global_point_depth(bin_arc, mt_depth)
    #pointdepth_dataframe_arc_pcg <- data_for_global_point_depth(bin_arc_pcg, mt_depth)
  } 
  D <- matrix(data = NA, nrow = sample_size, ncol = 1)
  if (type == "integrated depth") {
    D[, 1] <- integrated_depth(nbvar, bin_time, pointdepth_dataframe_time, weight)
    #D[, 2] <- integrated_depth(pointdepth_dataframe_time_pcg)
    #D[, 3] <- integrated_depth(pointdepth_dataframe_arc)
    #D[, 4] <- integrated_depth(pointdepth_dataframe_arc_pcg)
  } else if (type == "extremal depth") {
    D[, 1] <- extremal_depth(bin_time, pointdepth_dataframe_time)
    #D[, 2] <- extremal_depth(pointdepth_dataframe_time_pcg)
    #D[, 3] <- extremal_depth(pointdepth_dataframe_arc)
    #D[, 4] <- extremal_depth(pointdepth_dataframe_arc_pcg)
  }
  rownames(D) <- 1:sample_size
  #colnames(D) <- c("time", "time_pcg"
                   #, "arc", "arc_pcg"
  #                 )
  return (D)
}
