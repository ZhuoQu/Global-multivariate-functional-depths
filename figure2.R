source("normal_scenario_setting.R")
source("outlier_scenario_setting.R")
sim_setting <- 1:4
for (s in sim_setting) {
  normal_scenario <- normal_scenario_setting(sim_setting = s, n = 150,
                                           nbvar = 2, standard_grid = seq(0, 1, length.out = 50),
                                           visualize = FALSE)

  outlier_types <- c("magnitude outlier I", "amplitude outlier I", "shape outlier I")
  for (o in outlier_types) {
    outlier_scenario_setting(normal_scenario, n = 150, outlier_prop = 0.1, outlier_type = o,
                         nbvar, standard_grid, visualize = TRUE)
  }
}
