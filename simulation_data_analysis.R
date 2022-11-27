#setwd("~/Dropbox/Mac/Desktop/KAUST/project/Depthnotion/new_code")

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

dataframe_summary <- function(sim_setting, outlier_type) {
  label <- c("GFID (w_t)", "GFID (w_d)", "GFED", "LFID (w_t)", "LFID (w_d)", "LFED", "MPOIFD", "MFHD (mfpca)", "MFHD (bmfpca)")
  load(paste("sim_", sim_setting, "_",outlier_type,".RData", sep = ""))
  cases <- c("no_sparse", "med_point", "high_point", "med_peak", "high_peak", "med_partial", "high_partial")
  summary_dataframe <- c()
  for (case_tp in 1:length(cases)) {
    simul_subset <- lapply(simul, function(k) {
      if (length(k) > 1) {
        k[[case_tp]]} 
      })
    median_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {
      k$median_measure
      }}))
    central_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {
      k$central_measure
    }}))
    outlier_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {k$outlier_measure}}))
    
    rank_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {k$rank_measure}}))
    
    runtime_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {k$running_time_measure}}))
    
    subject <- rep(label, length(outlier_measure) / length(label))
    sparseness <- rep(cases[case_tp], length(subject))
    
    summary_subset <- data.frame(subject, sparseness, median_measure, central_measure, outlier_measure, rank_measure, runtime_measure)
    summary_dataframe <- rbind(summary_dataframe, summary_subset)
  }
  
  
  summary_dataframe$subject <- factor(summary_dataframe$subject,
                                      levels = c("GFID (w_t)", "GFID (w_d)", "GFED",
                                                 "LFID (w_t)", "LFID (w_d)", "LFED", "MPOIFD", "MFHD (mfpca)", "MFHD (bmfpca)"))
  return (summary_dataframe)
} 


plot_simulation_boxplot <- function(summary_dataframe, sim_setting, outlier_type, measure, measure_sparseness_type) {
  measure_range <- c("median", "central", "outlier", "rank", "running time")
  ylab_range <- c("ASE median", "ASE central dispersion", 
                  "True outlier detection percentage", "Spearman Correlation",
                  "Running time (s)")
  color_palette <- brewer.pal(n = 9, name = "Paired")
  if (measure == "running time") {
    height <- 4.2
    mar_param <- c(5.5, 4.5, 1, 1)
    mgp_param <- c(2.1, 1, 0)
  } else {
    height <- 3.7
    mar_param <- c(3, 4, 3, 1)
    mgp_param <- c(2, 1, 0)
  }
  pdf(file = paste("model", sim_setting, "_", outlier_type, "_", measure,"_", measure_sparseness_type, ".pdf", sep = ""), 
        width = 10, height = height)
  par(mar = mar_param, mgp = mgp_param, mfrow = c(1, 1))

  
    if (measure_sparseness_type %in% c("point", "peak", "partial")) {
      data_for_plot <- subset(summary_dataframe, sparseness %in% c("no_sparse", 
                                                                  paste("med_",measure_sparseness_type, sep = ""), 
                                                                  paste("high_",measure_sparseness_type, sep = "")))
      xlim_range <- c(1, 27)
      cut_distance <- seq(3.5, 21.5, length.out = 3)
      my_names <- c(expression(italic(p)['curve']), paste(seq(0, 60, length.out = 3),"%", sep = ""))
      grey_line_distance <- seq(0.5, 18.6, length.out = 3)
    } else if (measure_sparseness_type %in% c("med", "high")) {
      data_for_plot <- subset(summary_dataframe, sparseness %in% c("no_sparse", 
                                                                   paste(measure_sparseness_type,"_point", sep = ""), 
                                                                   paste(measure_sparseness_type,"_peak", sep = ""),
                                                                   paste(measure_sparseness_type,"_partial", sep = "")))
      xlim_range <- c(1, 36)
      my_names <- c( "type", "no sparseness", "point sparseness", "peak sparseness", "partial sparseness")
      cut_distance <- seq(3.5, 31, length.out = 4)
      grey_line_distance <- seq(0.5, 27.5, length.out = 4)
    }
  match_measure_index <- which(measure_range == measure)
  myplot <- boxplot(data_for_plot[, match_measure_index + 2] ~ subject * sparseness, data = data_for_plot, 
                  col = color_palette,
                  main = paste("Model ", 
                               sim_setting, " (", outlier_type, " case)", sep = ""),
                  varwidth = FALSE, xlim = xlim_range, 
                  ylim = c(min(data_for_plot[, match_measure_index + 2], na.rm = TRUE), 
                           max(data_for_plot[, match_measure_index + 2], na.rm = TRUE)), xaxt = "n", 
                  xlab = NULL, ylab = ylab_range[match_measure_index], cex.names = 0.6, 
                  cex.lab = 1.1, cex = 0.8)
   
axis(1, at = c(0.3, cut_distance), 
     labels = my_names, 
     tick = FALSE, cex.axis = 0.8)

# Add the grey vertical lines
for (i in grey_line_distance) { 
  abline(v = i, lty = 1, col = "grey")
}

if (measure == "running time") {
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  # Add a legend
  legend("bottom", title = "Depths", 
         legend = expression('GFID (w_t)','GFID (w_d)', 'GFED',
                             'LFID (w_t)','LFID (w_d)', 'LFED', 
                             'MPOIFD', 'MFHD (mfpca)', 'MFHD (bmfpca)'), 
         col = color_palette,
         pch = 15, bty = "o",  cex = 0.7, text.font = 1, 
         horiz = T, inset = c(0.01, 0.01))
 }
dev.off()
}

measure_range <- c("median", "central", "outlier", "rank", "running time")
measure_sparseness_range <- c("point", "peak", "partial", "med", "high")
sim_setting <- 1
outlier_type <- "amplitude outlier I"

summary_dataframe <- dataframe_summary(sim_setting, outlier_type) 
for (measure_criterion in measure_range) {
  for (sparseness_tp in measure_sparseness_range) {
  plot_simulation_boxplot(summary_dataframe, sim_setting, outlier_type, 
                          measure = measure_criterion, 
                          measure_sparseness_type = sparseness_tp)
  }
}

