load("time_size.RData")
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

color_palette <- c(brewer.pal(n = 6, name = "Paired"), "grey35", "grey80")
color_palette <- color_palette[c(2, 4, 6, 1, 3, 5, 7, 8)]

mar_param <- c(5.5, 3.5, 1.5, 1)
mgp_param <- c(2.0, 0.9, 0)
par(mar = mar_param, mgp = mgp_param, mfrow = c(1, 1))
line_type <- c(1, 1, 1, 2, 2, 2, 4, 4)
lwd_type <- c(2, 2, 2, 2, 2, 2, 1, 1)
plot(sample_size_candidate, simul_runningtime[, 1], type = "n",
     ylim = c(0, 800), cex.lab = 1.2,
     xlab = "Sample size", ylab = "Running time (s)", 
     main = "Running time of multivariate functional depths (T=50)")
lapply(1:ncol(simul_runningtime), function(k) {
  lines(sample_size_candidate, simul_runningtime[, k], col = color_palette[k], lty = line_type[k], lwd = lwd_type[k])
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




######################################################################################
######################################################################################
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

