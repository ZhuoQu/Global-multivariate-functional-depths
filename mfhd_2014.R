packages <- c("mrfDepth", "ddalpha", "geometry")

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

options <- list(type = "Rotation", approx = FALSE)

hdepth <- mrfDepth::hdepth
marg_depth <- function(mat, depth_name) {
  if (depth_name == "MFHD") {
    if (ncol(mat) <= 3 || nrow(mat) < 120) {
      func <- hdepth(mat)$depthZ
    } else {
      func <- hdepth(mat, options = options)$depthZ
    }
  }
  if (depth_name == "MFPD") {
    func <- projdepth(mat)$depthZ
  }
  if (depth_name == "MFSPD") {
    func <- sprojdepth(mat)$depthZ
  }
  if (depth_name == "MFDPD") {
    func <- dprojdepth(mat)$depthZ
  } else if (depth_name == "MSBD") {
    func <- depth.simplicial(mat, mat, k = 0.05, exact = F)
  }
  return (func)
}

multi_depth <- function(x, depth_name, weight_type) { #c("MFHD","MFPD","MFDPD","MFSPD","MSBD")
  ## x is a list of length(sett). In each list, it is a matrix of obs_nb * var_nb
  dp <- t(sapply(x, function(x_list) {
    marg_depth(x_list, depth_name)})) ###dp <- matrix(NA, nrow = t, ncol = n)
  depth_thres <-  quantile(dp, 0.25)
  if (weight_type == "volume") {
  weight_bin <- sapply(1:length(standard_grid), function(l) {
    data <- x[[l]]
    match_point <- data[which(dp[l, ] > depth_thres), ]
    return (try(convhulln(match_point, "FA")$vol, silent = TRUE))
  })
  weight_bin <- weight_bin / sum(weight_bin)
  } else {
    weight_bin <- rep(1 / length(standard_grid), length(standard_grid))
  }
  result <- apply(dp, 2, function(dp_col) {
    weight_bin %*% dp_col
  })
  return (result)
}



