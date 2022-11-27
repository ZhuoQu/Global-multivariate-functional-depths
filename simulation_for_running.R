source("simulation_for_depth_stage_1.R")

packages <- c("parallel")

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

numCores <- detectCores()
numCores

n <- 200
nbvar <- 2
standard_grid <- seq(0, 1, length.out = 50)
sim_setting <- 4
outlier_type <- "shape outlier I"
trial_times <- 150

simul_no_sparse <- mclapply(1:trial_times, function(k) {
  result_no_contam <- no_sparse_simulation(n, nbvar, standard_grid, sim_setting, outlier_type)
  simul_set <- result_no_contam[[1]]  
  param_sim0_PD <- result_no_contam[[2]]
  outlier_set <- result_no_contam[[3]]
  
  result_contam <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                            outlier_set, 
                                            sparse_type = "point", pc_lower = 0, pc_upper = 0)
  
  result_med_point <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                        outlier_set, 
                                        sparse_type = "point", pc_lower = 0.1, pc_upper = 0.3)
  
  result_high_point <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                        outlier_set,  
                                        sparse_type = "point", pc_lower = 0.4, pc_upper = 0.6)
  
  result_med_peak <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                        outlier_set, 
                                        sparse_type = "peak", pc_lower = 0.1, pc_upper = 0.3)
  
  result_high_peak <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                         outlier_set, 
                                         sparse_type = "peak", pc_lower = 0.4, pc_upper = 0.6)
  
  result_med_partial <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                       outlier_set, 
                                       sparse_type = "partial", pc_lower = 0.1, pc_upper = 0.3)
  
  result_high_partial <- sparse_simulation(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                        outlier_set, 
                                        sparse_type = "partial", pc_lower = 0.4, pc_upper = 0.6)
  return (list(result_contam,
       result_med_point,
       result_high_point,
       result_med_peak,
       result_high_peak,
       result_med_partial,
       result_high_partial))
       
  },  mc.cores = numCores - 2)




