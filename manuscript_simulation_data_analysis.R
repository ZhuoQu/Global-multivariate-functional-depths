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

simplified_dataframe_summary <- function(sim_setting, outlier_type) {
  label <- c("GFID (w_t)", "GFID (w_d)", "GFED", "LFID (w_t)", "LFID (w_d)", "LFED", "MPOIFD", "MFHD (mfpca)")
  load(paste("sim_", sim_setting, "_",outlier_type,".RData", sep = ""))
  cases <- c("no_sparse", "med_point", "high_point", "med_peak", "high_peak", "med_partial", "high_partial")
  summary_dataframe <- c()
  for (case_tp in 1:length(cases)) {
    simul_subset <- lapply(simul, function(k) {
      if (length(k) > 1) {
        k[[case_tp]]} 
    })
    median_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {
      k$median_measure[1:8]
    }}))
    central_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {
      k$central_measure[1:8]
    }}))
    outlier_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {k$outlier_measure[1:8]}}))
    
    rank_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {k$rank_measure[1:8]}}))
    
    runtime_measure <- unlist(lapply(simul_subset, function(k) {if (length(k) > 1) {k$running_time_measure[1:8]}}))
    
    subject <- rep(label, length(outlier_measure) / length(label))
    sparseness <- rep(cases[case_tp], length(subject))
    
    summary_subset <- data.frame(subject, sparseness, median_measure, central_measure, outlier_measure, rank_measure, runtime_measure)
    summary_dataframe <- rbind(summary_dataframe, summary_subset)
  }
  
  
  summary_dataframe$subject <- factor(summary_dataframe$subject,
                                      levels = c("GFID (w_t)", "GFID (w_d)", "GFED",
                                                 "LFID (w_t)", "LFID (w_d)", "LFED", "MPOIFD", "MFHD (mfpca)"))
  return (summary_dataframe)
} 


simplified_plot_simulation_boxplot <- function(simplified_summary_dataframe, sim_setting, outlier_type, 
                                               measure, measure_sparseness_type) {
  ### we only check the comparison between no sparse and high point sparseness as an instance
  measure_range <- c("median", "central", "outlier", "rank")
  ylab_range <- c("ASE median", "ASE central dispersion", 
                  "True outlier detection percentage", "Spearman correlation")
  color_palette <- c(brewer.pal(n = 6, name = "Paired"), "grey35", "grey80")
  color_palette <- color_palette[c(2, 4, 6, 1, 3, 5, 7, 8)]
  if (measure == "rank") {
    height <- 4.2
    mar_param <- c(5.5, 4.5, 0.5, 1)
    mgp_param <- c(2.1, 1, 0)
  } else if (measure == "median"){
    height <- 3.7
    mar_param <- c(0.5, 4.5, 3, 1)
    mgp_param <- c(2.1, 1, 0)
  } else {
    height <- 3.7
    mar_param <- c(0.5, 4.5, 0.5, 1)
    mgp_param <- c(2.1, 1, 0)
  }
  pdf(file = paste("model", sim_setting, "_", outlier_type, "_", measure,"_high_", measure_sparseness_type, ".pdf", sep = ""), 
      width = 10, height = height)
  par(mar = mar_param, mgp = mgp_param, mfrow = c(1, 1))
  
  
  if (measure_sparseness_type %in% c("point", "peak", "partial")) {
    data_for_plot <- subset(simplified_summary_dataframe, sparseness %in% c("no_sparse", 
                                                                 paste("high_", measure_sparseness_type, sep = "")))
    data_for_plot$sparseness <- factor(data_for_plot$sparseness,
                                       levels = unique(data_for_plot$sparseness))
    xlim_range <- c(1, 16)
    cut_distance <- seq(3.5, 13, length.out = 2)
    my_names <- c(expression(italic(p)['curve']), paste(seq(0, 60, length.out = 2),"%", sep = ""))
    grey_line_distance <- median(cut_distance) + 0.25
  }
  match_measure_index <- which(measure_range == measure)
  yvalue <- data_for_plot[, match_measure_index + 2]
  ylab <- ylab_range[match_measure_index]
  if (measure %in% c("central")) {
    yvalue <- log(yvalue + 0.05)
    ylab <- paste("Logarithm of ", ylab_range[match_measure_index], sep = "")
  }
  if (measure == "median") {
    main_title <- paste("Model ", 
          sim_setting, " (", outlier_type, " case)", sep = "")
  } else {
    main_title <- NULL
  }
  myplot <- boxplot(yvalue ~ subject * sparseness, data = data_for_plot, 
                    col = color_palette,
                    main = main_title,
                    varwidth = FALSE, xlim = xlim_range, 
                    ylim = c(min(yvalue, na.rm = TRUE), 
                             max(yvalue, na.rm = TRUE)), xaxt = "n", 
                    xlab = NULL, ylab = ylab, cex.names = 0.6, 
                    cex.lab = 1.1, cex = 0.8)
  
  # Add the grey vertical lines
  for (i in grey_line_distance) { 
    abline(v = i, lty = 1, col = "grey")
  }
  
  if (measure == "rank") {
    axis(1, at = c(0.5, cut_distance), 
         labels = my_names, 
         tick = FALSE, cex.axis = 0.8)
    
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    # Add a legend
    legend("bottom", title = "Depths", 
           legend = expression('GFID (w_t)','GFID (w_d)', 'GFED',
                               'LFID (w_t)','LFID (w_d)', 'LFED', 
                               'MPOIFD', 'MFHD (mfpca)'), 
           col = color_palette,
           pch = 15, bty = "o",  cex = 0.85, text.font = 1, 
           horiz = T, inset = c(0.01, 0.01))
  }
  dev.off()
}

# measure_range <- c("median", "central", "outlier", "rank")
# measure_sparseness_type <- "partial"
# sim_setting <- 4
# 
# outlier_types <- c("magnitude outlier I", "amplitude outlier I", "shape outlier I",
#                    "magnitude outlier II", "amplitude outlier II", "shape outlier II")
# for (outlier_type in outlier_types) {
#   simplified_summary_dataframe <- simplified_dataframe_summary(sim_setting, outlier_type) 
#   for (measure_criterion in measure_range) {
#     simplified_plot_simulation_boxplot(simplified_summary_dataframe, sim_setting, outlier_type, 
#                             measure = measure_criterion, 
#                             measure_sparseness_type)
#   }
# }
# 
# 
