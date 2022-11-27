simulation_assessment_criteria <- function(n, nbvar, standard_grid, 
                                           simul_set, outlier_set, true_median,
                                           sim0_mfd, sim1_mfd, running_time, param_sim1_PD) {
  outlier_index <- outlier_set[[2]]
  estimate_median_index <- order(sim1_mfd, decreasing = TRUE)[1]
  estimate_median <- lapply(outlier_set[[1]], function(k){k[, estimate_median_index]})
  time_index <- which(standard_grid %in% param_sim1_PD[[estimate_median_index]]$argvals)

  central_index <- order(sim0_mfd, decreasing = TRUE)[1: (n / 2)]   
  contam_central_index <- order(sim1_mfd, decreasing = TRUE)[1: (n / 2)]

  central_dispersion <- matrix(NA, nrow = length(standard_grid), ncol = nbvar)
  contam_central_dispersion <- matrix(NA, nrow = length(standard_grid), ncol = nbvar)
  for (l in 1:nbvar) {
    central_dispersion[, l] <- apply(simul_set[[l]][, central_index], 1, max) - 
      apply(simul_set[[l]][, central_index], 1, min)
  
    contam_central_dispersion[, l] <- apply(outlier_set[[1]][[l]][, contam_central_index], 1, max) - 
      apply(outlier_set[[1]][[l]][, contam_central_index], 1, min)
  }

  central_measure <- mean(sapply(1:nbvar, function(l) {
    mean((log(contam_central_dispersion[, l] / central_dispersion[, l]))^2) }))

  
  median_measure <- mean(sapply(1:nbvar, function(l) { mean(((true_median[[l]][time_index] - 
                                                               estimate_median[[l]][time_index]) / central_dispersion[time_index, l])^2)}))
  
  outliers_measure <- length(intersect(order(sim1_mfd, decreasing = TRUE)[round(0.9 * n + 1): n], outlier_index)) / length(outlier_index)

  rank_measure <- cor(sim0_mfd[-outlier_index], sim1_mfd[-outlier_index], method = "spearman")
  return (list(median_measure = median_measure,
               central_measure = central_measure,
               outliers_measure = outliers_measure,
               rank_measure = rank_measure,
               running_time = running_time))
}
