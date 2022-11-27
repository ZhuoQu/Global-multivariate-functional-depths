## 1. nbvar is the number of variables
## 2. param_PD is the output of the function parameterization_PD
## 2. param_PD is a list with the length n
## each sublist contains argvals, subj, y with |argvals| * nbvar, 
## time_percentage, arc_length and arc_length_percentage
#### 3. Dmat is a matrix of n*4
## where each column is time, time percentage, arc length and arc length dependent depth
source("boxplot_time_criterion.R")
# source("boxplot_timepcg_criterion.R")
# source("boxplot_arclen_criterion.R")
# source("boxplot_arclenpcg_criterion.R")
source("simplified_sparse_boxplot.R")
boxplot_criterion <- function(nbvar, param_PD, Dmat, outlier_candidate,
                              plotboxplot = 1, titles, ylabs) {
  ## 1. nbvar is the number of variables
  ## 2. param_PD is the output of the function parameterization_PD
  ## 3. parameterization is the type of parameterization c("time", "time percentage", "arc length", "arc length percentage")
  ## 3. type is an option of c("integrated depth", "extremal depth")
  ## 4. mt_depth is an option of multivariate depths: 
  ### c("halfspace depth", "projection depth", "skewness-adjusted projection depth", "directional projection depth",
  #"simplicial depth")")
  sample_size <- length(param_PD)
  descriptive_stat <- vector(mode = "list", length = 2)
  thres <- 0.75
  #  dd <- Dmat[, param]
  # # names(dd) <- 1:sample_size
  # median_index <- order(Dmat[, 2] + Dmat[, 4], decreasing = TRUE)[1]
  # centralRegion_index <- intersect(order(Dmat[, 2], decreasing = TRUE)[1: ceiling(sample_size / 2)], 
  #                                  order(Dmat[, 4], decreasing = TRUE)[1: ceiling(sample_size / 2)])
  time_interval <- sapply(param_PD, function(k) {
    range(k$argval)[2]
  })
  
    param = 1
    
      domain_test1 <- boxplot(time_interval, plot = FALSE)
      domain_test2 <- boxplot(log(time_interval), plot = FALSE)
      domain_outlier <- c(which(time_interval %in% domain_test1$out), 
                          which(log(time_interval) %in% domain_test2$out))
      outlier_candidate <- union(outlier_candidate, domain_outlier)
    
    candidate_index <- setdiff(1:sample_size, outlier_candidate)
    dd <- Dmat[candidate_index, param]
    names(dd) <- candidate_index
    order_index <- candidate_index[order(dd, decreasing = TRUE)]
    median_index <- order_index[1]
    centralRegion_index <- order_index[1: ceiling(length(candidate_index) / 2)]
    
      info <- boxplot_time_criterion(param_PD, median_index, centralRegion_index, outlier_candidate, thres)
    
    if (plotboxplot == 1) {
      simplified_sparse_boxplot(param_PD, info, outlier_candidate, 
                                param, titles, ylabs, 
                                showoutlier = TRUE)
    } else {
      simplified_sparse_intensity_boxplot(param_PD, info, outlier_candidate, 
                                          param, titles, ylabs, 
                                          colorsplit = 40, colorrange =  NULL,
                                          showoutlier = FALSE, showcontour = FALSE, 
                                          showlegend = TRUE)
    }
    info
  return (info)
}
