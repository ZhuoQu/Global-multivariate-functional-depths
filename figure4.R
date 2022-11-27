
source("manuscript_simulation_data_analysis.R")


measure_range <- c("median", "central", "outlier", "rank")
measure_sparseness_type <- "point"
sim_setting <- 3

outlier_types <- c("magnitude outlier I", "shape outlier I")
for (outlier_type in outlier_types) {
  simplified_summary_dataframe <- simplified_dataframe_summary(sim_setting, outlier_type) 
  for (measure_criterion in measure_range) {
    simplified_plot_simulation_boxplot(simplified_summary_dataframe, sim_setting, outlier_type, 
                                       measure = measure_criterion, 
                                       measure_sparseness_type)
  }
}


