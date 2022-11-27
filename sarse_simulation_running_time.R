#setwd("~/Dropbox/Mac/Desktop/KAUST/project/Depthnotion/new_code")

packages <- c("randomcoloR")

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

source("normal_scenario_setting.R")
source("outlier_scenario_setting.R")
source("sparsify_scenario.R")
source("parameterization_PD.R")
source("multivariate_functional_depth.R")
source("simulation_assessment_criteria.R")
source("data_structure_transform.R")
source("simulation_poifd.R")
source("simulation_mfhd.R")
###############

no_sparse_simulation <- function(n, nbvar, standard_grid, sim_setting, outlier_type) {
  
  ##################### Construct the normal curves and the outliers
  simul_set <- normal_scenario_setting(sim_setting, n, 
                                       nbvar, standard_grid, visualize = TRUE)
  ### simul_set is a setting under no contamination and no sparseness.
  ### we obtain its median and the dispersion of the central region.
  no_contamination_scenario <- sparsify_scenario(sparse_type = "point", ps = 0, 
                                                 pc_band = c(0.3, 0.7), 
                                                 standard_grid, simul_set)
  param_sim0_PD <- parameterization_PD(PD = no_contamination_scenario[[1]])
  
  ####################### Introduce contamination
  outlier_set <- outlier_scenario_setting(simul_set, n, outlier_prop = 0.1, 
                                          outlier_type,
                                          nbvar, standard_grid, visualize = TRUE)
  
  return (list(simul_set, param_sim0_PD, outlier_set))
}




sparse_simulation_running_time <- function(simul_set, param_sim0_PD, n, nbvar, standard_grid, 
                              outlier_set, 
                              sparse_type, pc_lower, pc_upper) {
  
  ############################# Introduce sparseness
  sparse_scenario <- sparsify_scenario(sparse_type, ps = 1, pc_band = c(pc_lower, pc_upper), 
                                       standard_grid, outlier_set[[1]])
  
  ###############################################################
  param_sim1_PD <- parameterization_PD(PD = sparse_scenario[[1]])
  
  ##############################################
  ######################### Procedure based on the global extremal depth
  perspectives <- rep(c("global", "local"), each = 3)
  depth_types <- rep(c(rep("integrated depth", 2), "extremal depth"), 2)
  weights <- rep(c("time", "region", "time"), 2)
  notes <- c("GFID_w_time", "GFID_w_region", "GFED", 
             "LFID_w_time", "LFID_w_region", "LFED", 
             "MPOIFD", "MFHD (mfpca)", "MFHD (bmfpca)")
  reconstructions <- c("mfpca", "bmfpca")
  median_measure_sum <- c()
  central_measure_sum <- c()
  outliers_measure_sum <- c()
  rank_measure_sum <- c()
  running_time_sum <- c()
  ###### median
  true_median <- lapply(simul_set, function(k) {apply(k, 1, mean)})
  ###################
  for (depth_count in 1:(length(notes) - 1)) {
    cat(depth_count, sep = "\n")
    if (depth_count <= 6) {
      sim0_mfd <- mulitvariate_functional_depth(nbvar, 
                                                param_PD = param_sim0_PD,
                                                point_depth_comp = perspectives[depth_count],
                                                type = depth_types[depth_count], 
                                                mt_depth = "halfspace depth", 
                                                weight = weights[depth_count])
      start_time <- Sys.time()
      sim1_mfd <- mulitvariate_functional_depth(nbvar, 
                                                param_PD = param_sim1_PD,
                                                point_depth_comp = perspectives[depth_count],
                                                type = depth_types[depth_count], 
                                                mt_depth = "halfspace depth", 
                                                weight = weights[depth_count])
      end_time <- Sys.time()
      if(is.na(sum(sim0_mfd)) == TRUE || is.na(sum(sim0_mfd)) == TRUE) {
        stop("there exists at least a NA depth")
      }
    } else if (depth_count == 7) {
      sim0_mfd <-  multi_poifd(param = param_sim0_PD, standard_grid, n, nbvar)
      start_time <- Sys.time()
      sim1_mfd <- multi_poifd(param = param_sim1_PD, standard_grid, n, nbvar) 
      end_time <- Sys.time()
    } else if (depth_count >= 8 && depth_count <= 9) {
      true_data <- data_structure_transform(nbvar, standard_grid, n, param = param_sim0_PD)
      sim0_mfd <- simulation_mfhd(true_data, standard_grid, n, nbvar)
      sparse_data <- data_structure_transform(nbvar, standard_grid, n, param = param_sim1_PD)
      start_time <- Sys.time()
      mfpca_fit <- fit_sparsedata(bootstrapTimes = 50, standard_grid,
                                  true_data, sparse_data, 
                                  reconstruction_type = reconstructions[depth_count - 7])
      sim1_mfd <- simulation_mfhd(mfpca_fit, standard_grid, n, nbvar)
      end_time <- Sys.time()
    }
    running_time <- difftime(end_time, start_time, unit = "secs")
    # measures <- simulation_assessment_criteria(n, nbvar, standard_grid, 
    #                                            simul_set, outlier_set, true_median,
    #                                            sim0_mfd, sim1_mfd, running_time, param_sim1_PD)  
    running_time_sum[depth_count] <- running_time
    
  }  
  names(running_time_sum) <- notes
  return (running_time_measure = running_time_sum)
}

