packages <- c("psychTools")

## Now load or install&load all
package_check <- lapply(
  packages,
  FUN <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

extremal_depth <- function(bin_est, subject_point_data) {
  sample_size <- max(subject_point_data$subject)
  gridlength <- length(bin_est$denseGrid)
  subject_point_data$weighted_point_depth <- subject_point_data$depth
  # * weight_bin[index]
  min_point_depth <- min(subject_point_data$weighted_point_depth)
  max_point_depth <- max(subject_point_data$weighted_point_depth)
  point_depth_index <- seq(min_point_depth, max_point_depth, length.out = gridlength)

  cdf_matrix <- t(sapply(1:sample_size, function(row_ind) {
    subject_depth <- subset(subject_point_data, subject == row_ind, select = weighted_point_depth)
    col_vec <- sapply(point_depth_index, function(w) {
      sum(subject_depth$weighted_point_depth <= w) / 
      length(subject_depth$weighted_point_depth)})
  }))
  rownames(cdf_matrix) <- 1:sample_size
  colnames(cdf_matrix) <- point_depth_index
  
  ordered_cdf_matrix <- dfOrder(cdf_matrix, -c(1:gridlength))
  # new_cdf_matrix <- cdf_matrix
  # extreme_to_center_order <-c()
  # for (k in 1:nrow(ordered_cdf_matrix)) {
  #   cat(k, "\n")
  #   subject <- which(apply(new_cdf_matrix, 1, function(x) identical(x, ordered_cdf_matrix[k, ])) == TRUE)
  #   new_cdf_matrix[subject, ] <- 0
  #   class <- rep(k, length(subject))
  #   extreme_to_center_order <- rbind(extreme_to_center_order, data.frame(subject = subject, class = class))
  # }
  
  d <- rep(0, sample_size)
  d[as.numeric(rownames(ordered_cdf_matrix))] <- 1:sample_size / sample_size
  return(d)
}

### The output of the function extremal_depth is a vector with length n (n is the number of subjects)
