packages <- c("funData", "MASS")

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
normal_scenario_setting <- function(sim_setting, n, 
                               nbvar, standard_grid, visualize = TRUE) {
  group <- vector(mode = "list", length = nbvar) 
  
  fourier_union_grid <- standard_grid
  nbtime <- length(standard_grid)
  match_index <- 1:nbtime
  for (l in 2:nbvar) {
    fourier_union_grid <- c(fourier_union_grid, 
                            standard_grid + l * max(standard_grid) + 0.01)
  }
  random_sign <- ifelse(runif(nbvar, -1, 1) > 0, 1, -1)
  error_var <- c(runif(1, 0, 0.02), runif(nbvar -1 , 0, 0.1))
  if (sim_setting == 1) { ##### circle
    M = 8
    radius <- 5
    group[[1]] <- matrix(rep(radius * cos(2 * pi * standard_grid), n), ncol = n, byrow = FALSE)
    
    if (nbvar == 2) {
      group[[2]] <- matrix(rep(radius * sin(2 * pi * standard_grid), n), ncol = n, byrow = FALSE)
    } else if (nbvar == 3) {
      group[[2]] <- matrix(rep(radius * sin(2 * pi * standard_grid) * 
                                 sin(pi * standard_grid + pi / 2), n), ncol = n, byrow = FALSE)
        
      group[[3]] <- matrix(rep(radius * sin(2 * pi * standard_grid) * 
                                 cos(pi * standard_grid + pi / 2), n), ncol = n, byrow = FALSE)
    }
  } else if (sim_setting == 2) { ###### hurricane
      M = 8
      group[[1]] <- matrix(rep(-4 * standard_grid, n), ncol = n, byrow = FALSE)
      group[[2]] <- matrix(rep(5 * standard_grid, n), ncol = n, byrow = FALSE)
      if (nbvar == 3) {
        group[[3]] <- matrix(rep(6 * standard_grid, n), ncol = n, byrow = FALSE)
      }
    } else if (sim_setting == 3) {
      M = 2
      group[[2]] <- matrix(rep(4 * standard_grid, n), ncol = n, byrow = FALSE)
      
      group[[1]] <- matrix(NA, ncol = n, nrow = nbtime)
      for (l in 1:n) {
        group[[1]][, l] <- 6 * (standard_grid - 1/2 )^2 
      }
      if (nbvar == 3) {
        group[[3]] <- matrix(rep(4 * log(standard_grid + 0.2), n), ncol = n, byrow = FALSE)
      }
  } else if (sim_setting == 4) {
      M <- 0
      group[[1]] <- matrix(NA, ncol = n, nrow = nbtime)
      group[[2]] <- matrix(NA, ncol = n, nrow = nbtime)
      for (l in 1:n) {
        group[[1]][, l] <- standard_grid
        num <- runif(1, -7, 7)
        group[[2]][, l] <- num * (standard_grid - 1/2)^2
        group[[2]][, l] <- group[[2]][, l] - num / 4
      }
      
      if (nbvar == 3) {
        group[[3]] <- matrix(rep(5 * standard_grid, n), ncol = n, byrow = FALSE)
        for (l in 1:n) {
          group[[3]][, l] <- group[[3]][, l] + rnorm(1, 0, 0.5) * cos(pi * standard_grid)
        }
      }
  }
  for (l in 1:n) {
    if (M >= 1) {
      m <- 1:M
      rho <- mvrnorm(1, rep(0, M), diag(sqrt(exp(-(m + 1 / 2)))))
      basis_fun <- eFun(fourier_union_grid, M, type = "Fourier")
      group[[1]][, l] <- group[[1]][, l] + 
        t(random_sign[1] * basis_fun@X[, match_index]) %*% rho
      group[[2]][, l] <- group[[2]][, l] + 
        t(random_sign[2] * basis_fun@X[, (nbtime + match_index)]) %*% rho 
      if (nbvar == 3) {
        group[[3]][, l] <- group[[3]][, l] + 
          t(random_sign[3] * basis_fun@X[, (2 * nbtime + match_index)]) %*% rho
      }
    } 
    group[[1]][, l] <- group[[1]][, l] + rnorm(nbtime, 0, sqrt(error_var[1]))
    group[[2]][, l] <- group[[2]][, l] + rnorm(nbtime, 0, sqrt(error_var[2]))
    
    if (nbvar == 3) {
      group[[3]][, l] <- group[[3]][, l] + rnorm(nbtime, 0, sqrt(error_var[3]))
    }
  }
  
    
  if (visualize == TRUE) {
    if (nbvar == 2) {
      plot(group[[1]][, 1], group[[2]][, 1], 
          ylim = range(group[[2]]), 
          xlim = range(group[[1]]), type = "n",
          xlab = "Variable 1", 
          ylab = "Variable 2", 
          main = "Simulations")
      for (l in 1:n) {
        lines(group[[1]][, l], group[[2]][, l])
      }
    } else if (nbvar == 3) {
      s3d <- scatterplot3d::scatterplot3d(group[[1]][, 1], 
                                          group[[2]][, 1], 
                                          group[[3]][, 1], 
                                          xlim = range(group[[1]]), 
                                          ylim = range(group[[2]]),
                                          zlim = range(group[[3]]), 
                                          type = "n",
                                          xlab = "Variable 1", 
                                          ylab = "Variable 2", 
                                          zlab = "Variable 3", 
                                          main = "Simulations")
      for (l in 1:n) {
        s3d$points3d(group[[1]][, l], 
                   group[[2]][, l], 
                   group[[3]][, l], type = "l")
      }
    }
  }
  return (group)
}

# normal_scenario <- normal_scenario_setting(sim_setting = 2, n = 150, 
#                                     nbvar = 2, nbtime = 50, visualize = TRUE)
