sparsify_scenario <- function(sparse_type, ps, pc_band, standard_grid, simulation_data) {
  ## ps is a parameter specifying the proportion of sparseness among samples
  ## pc is a parameter specifying the proportion of sparseness among curves with missing values
  ## simulation_data is with length p, and each sublist is a nbtime * n matrix.
  n <- ncol(simulation_data[[1]])
  nbvar <- length(simulation_data)
  nbtime <- length(standard_grid)
  
  sample_curve <- sample(1:n, ps * n)
  PD <- lapply(1:n, function(i){
    if (i %in% setdiff(1:n, sample_curve) ) {
      observed_index <- 1:nbtime
    } else {
      pc <- runif(1, pc_band[1], pc_band[2])
      if (sparse_type == "point") {
        observed_index <- sort(sample(1:nbtime, size = round((1 - pc) * nbtime), replace = FALSE))
      } else if (sparse_type == "partial") {
         end_index <- round(rnorm(n = 1, mean = pc * nbtime / 2, sd = pc * nbtime / 6) + (1 - pc) * nbtime)
         end_index <- ifelse(end_index > nbtime - 5, nbtime - 5, end_index)
         end_index <- ifelse(end_index < (1 - pc) * nbtime, (1 - pc) * nbtime + 1, end_index)
         observed_index <- 1:end_index
      } else if (sparse_type == "peak") {
        sparse_st <- round(rnorm(n = 1, mean = (5 + (1 - pc) * nbtime) / 2, sd = (1 - pc) * nbtime / 6))
        sparse_st <- ifelse(sparse_st > (1 - pc)* nbtime - 2, (1 - pc)* nbtime - 2, sparse_st)
        sparse_st <- ifelse(sparse_st < 7, 7, sparse_st)
        observed_index <- c(1:(sparse_st - 1), (sparse_st + pc * nbtime + 1):nbtime)
      }
    }
    values <- simulation_data[[1]][, i]
    if (nbvar >= 2) {
      for (kk in 2:nbvar) {
        values <- cbind(values, simulation_data[[kk]][, i])
      }
    }
    return (list(argvals = standard_grid[observed_index],
                 subj = rep(i, length(observed_index)), 
                 y = values[observed_index, ]))
    
  })
  
  marginal_PD <- function(nbvar) {
    result <- PD
    for (l in 1:length(result)) {
      result[[l]]$y <- PD[[l]]$y[, nbvar]
    }
    return (result)
  }
  
  result <- vector(mode = "list", length = nbvar + 1)
  result[[1]] <- PD
  for (l in 2:(nbvar + 1)) {
    result[[l]] <- PD
    result[[l]] <- marginal_PD(l - 1)
  }
  return (result)
}
