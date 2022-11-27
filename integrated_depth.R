# source("multivariate_depth.R")
# source("updated_multivariate_depth.R")
####
### Subject_point_data is a dataframe with index, subject, y, and point_depth
packages <- c("geometry")

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
integrated_depth <- function(nbvar, bin_est, subject_point_data, weight) {
  sample_size <- max(subject_point_data$subject)
  time_index <- subject_point_data$bin_index
  #weight_subj <- table(subject_point_data$subject) / length(dense_grid_index)
  
  w <- bin_est$weight_time
  if (weight != "time") { 
    depth_thres <-  quantile(subject_point_data$depth, 0.25)
    weight_time <- sapply(1:length(w), function(l) {
      match_index <- which(time_index == l)
      if (length(match_index) <= 4) {
        return (0)
      } else {
        data <- bin_est$bin_observation[[l]]
        select_index <- which(subject_point_data$depth[match_index] > depth_thres)
        if (length(select_index) <= nbvar) {
          return (0)
        } else {
          match_point <- data[select_index, ]
          return (try(as.numeric(convhulln(match_point, "FA")$vol), silent = TRUE))
        }
      }
    })
  } else {
    weight_time <- w
  }
  
  d <- sapply(1:sample_size, function(samp_index) {
    #cat(samp_index, "\n")
    subject_time <- subset(subject_point_data, subject == samp_index, select = bin_index)[, 1]
    subject_depth <- subset(subject_point_data, subject == samp_index, select = depth)[, 1]
    weight_time <- weight_time[subject_time]
    value <- weight_time %*% subject_depth / sum(weight_time)
  })
  return (d)
}
