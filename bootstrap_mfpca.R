packages = c("funData", "MFPCA", "fdapace")

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

bootstrap_mfpca <- function(standard_grid, true_data, 
                            sparse_data, isbootstrap) {
  ## true_data and sparse data are list of nbvar, and each list is a n * |grid| matrix
  ### isbootstrap is a logical argument
  if ("matrix" %in% class(sparse_data)) {
    sparse_data <- list(sparse_data)
  } 
  grid_size <- ncol(sparse_data[[1]])
  nbvar <- length(sparse_data)
  n <- nrow(true_data[[1]])
  
  obj <- lapply(1:nbvar, function(i) {
    Ly <- sparse_data[[i]]
    res <- funData::funData(list(standard_grid), Ly)
    return(res) })
  expres <- list(type = "uFPCA")
  mFData <- multiFunData(obj)
  newmFData <- mFData
  
  sample_index <- 1:n
  estimated_observed_p <- sapply(1:nbvar, function(i) {
    apply(sparse_data[[i]], 2, function(j) {
      1 - sum(is.na(j)) / grid_size})})
  
  min_observed_points <- grid_size * min(estimated_observed_p)
  
  M <- ifelse(min_observed_points <= 3, 4, 9)
  if (length(true_data) == 0) {
    fit_mfpca <- MFPCA::MFPCA(newmFData, M, 
                              uniExpansions = lapply(1:nbvar, function(i) {expres}),
                              fit = TRUE)
    true_data <- lapply(fit_mfpca$fit, function(k) {k@X})
  }
  new_true_data <- true_data
  #new_observed_data <- observed_data
  newsparse_data <- sparse_data
  
  if (isbootstrap == TRUE) {
    sample_nb <- base::sample(1:n, n, replace = TRUE)
    
    sample_index <- sort(sample_nb)
    
    for (i in 1:nbvar) {
      newmFData[[i]]@X <- mFData[[i]]@X[sample_index, ]
      newsparse_data[[i]] <- sparse_data[[i]][sample_index, ]
      new_true_data[[i]] <- true_data[[i]][sample_index, ]
      #new_observed_data[[i]] <- observed_data[[i]][sample_index, ]
    }
  }
  
  fit_mfpca <- MFPCA::MFPCA(newmFData, M, 
                            uniExpansions = lapply(1:nbvar, function(i) {expres}),
                            fit = TRUE)

  fitY <- lapply(1:nbvar, function(i) {fit_mfpca$fit[[i]]@X})
  
  result <- list(
    fit = fitY, ### nbvar lists of n * length(sett) matrix
    sample_index = sample_index)
  ### fit = score %*% Phi + mean
  return (result)
}

