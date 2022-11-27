source("sparse_simulation_running_time.R")

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

grid_candidate <- c(20, 50, seq(100, 1000, by = 50))
#sample_size_candidate <- c(50, 100, 200)
nbvar <- 2
n <- 2000
sim_setting <- 1
outlier_type <- "magnitude outlier I"
trial_times <- 10

summary_time_list <- lapply(1:trial_times, function(trial) {
simul_runningtime <- matrix(NA, nrow = length(grid_candidate), ncol = 8)

for (count in 1:length(grid_candidate)) {
  cat(count, "\n")
  standard_grid <- seq(0, 1, length.out = grid_candidate[count])
  result_no_contam <- no_sparse_simulation(n, nbvar, standard_grid, sim_setting, outlier_type)
  simul_set <- result_no_contam[[1]]  
  param_sim0_PD <- result_no_contam[[2]]
  outlier_set <- result_no_contam[[3]]
  
  result_contam <- sparse_simulation_running_time(simul_set, param_sim0_PD, n, nbvar, standard_grid,
                                                  outlier_set, 
                                                  sparse_type = "point", pc_lower = 0,  pc_upper = 0)
  simul_runningtime[count, ] <- result_contam
}
rownames(simul_runningtime) <- grid_candidate
#, mc.cores = numCores - 2
return (simul_runningtime)
})



## if we only have the result of one trial

save(simul_runningtime, file = paste("N=",n, "_time_grid.RData", sep = ""))

load(file = paste("N=",n, "_time_grid.RData", sep = ""))

packages <- c("RColorBrewer")

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

color_palette <- c(brewer.pal(n = 6, name = "Paired"), 
                   "grey35", "grey80")
color_palette <- color_palette[c(2, 4, 6, 1, 3, 5, 7, 8)]

mar_param <- c(5.5, 3.5, 1.5, 1)
mgp_param <- c(2.0, 0.9, 0)
par(mar = mar_param, mgp = mgp_param, mfrow = c(1, 1))
line_type <- c(1, 1, 1, 2, 2, 2, 4, 4)
lwd_type <- c(2, 2, 2, 2, 2, 2, 1, 1)
plot(grid_candidate, simul_runningtime[, 1], type = "n",
     ylim = c(0, 800), cex.lab = 1.2, 
     xlab = "Grid size", ylab = "Running time (s)", 
     main = paste("Running time of multivariate functional depths (N = ", n, ")", sep = ""))
lapply(1:ncol(simul_runningtime), function(k) {
  lines(grid_candidate, simul_runningtime[, k], col = color_palette[k], lty = line_type[k], lwd = lwd_type[k])
})

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# Add a legend
legend("bottom", title = "Depths",
       legend = expression('GFID (w_t)','GFID (w_d)', 'GFED',
                           'LFID (w_t)','LFID (w_d)', 'LFED', 
                           'MPOIFD', 'MFHD (mfpca)'), 
       col = color_palette, lty = line_type, lwd = lwd_type,
       pch = 15, bty = "o",  cex = 0.57, text.font = 1, 
       horiz = T, inset = c(0.01, 0.01))


