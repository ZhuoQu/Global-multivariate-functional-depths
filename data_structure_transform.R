data_structure_transform <- function(nbvar, standard_grid, n, param) {
  #### the purpose of data_structure_transform is to transform data into
  ### a list of nbvar, and each sublist is a matrix of obs_nb * |grid|
  grid_size <- length(standard_grid)

  data_structure <- lapply(1:nbvar, function(variable) {
    mat <- matrix(NA, nrow = n, ncol = grid_size)
    rownames(mat) <- 1:n
    for(k in 1:n) {
      index <- sapply(param[[k]]$argvals, function(l) {which(abs(standard_grid - l) == min(abs(standard_grid - l)))})
      mat[k, index] <- param[[k]]$y[, variable]
    }
    return (mat)
  })
  
  
  return (data_structure)
}
