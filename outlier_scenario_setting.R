outlier_scenario_setting <- function(normal_scenario, n, outlier_prop, outlier_type,
                                    nbvar, standard_grid, visualize = TRUE) {
  if (outlier_type != "no outlier") {
    outlier_index <- sample(1:n, round(outlier_prop * n))
  } else {
    outlier_index <- NULL
  }
  nbtime <- length(standard_grid)
  #### introduce outliers 
  radius <- sapply(normal_scenario, function(kk) {max(abs(range(kk)))})
    if (outlier_type == "magnitude outlier I") {
       ### Magnitude outlier 1
      for (s in outlier_index) {
        rand_nb <- ifelse(runif(1, -0.5, 0.5) >= 0, 1, -1)
        for (l in 1:nbvar) {
          rand_number <- runif(1, 0.8, 1)
          normal_scenario[[l]][, s] <- normal_scenario[[l]][, s] + 
            rand_nb * rand_number * radius[l]
        }
      } 
    }
    
    if (outlier_type == "magnitude outlier II") {
      #### Magnitude outlier 2
      for (s in outlier_index) {
        terminal_st <- sample(3:round(0.85 * nbtime), 1)
        term <- terminal_st:(terminal_st + round(0.1 * nbtime))
        rand_nb <-ifelse(runif(1, -0.5, 0.5) >= 0, 1, -1)
        for (l in 1:nbvar) {
          rand_number <- runif(1, 0.8, 1)
          normal_scenario[[l]][term, s] <- normal_scenario[[l]][term, s] + 
            rand_nb * rand_number * radius[l]
      }
     }
    }
    if (outlier_type == "amplitude outlier I") {
      #### Magnitude outlier 2
      for (l in 1:nbvar) {
        for (s in outlier_index) {
          rand_number <- runif(1, 0.8, 1)
          normal_scenario[[l]][, s] <- (1 + rand_number) * normal_scenario[[l]][, s] 
        }
      }
    }
    if (outlier_type == "amplitude outlier II") {
      #### Magnitude outlier 2
      for (l in 1:nbvar) {
        for (s in outlier_index) {
          rand_number <- runif(1, 0.8, 0.9)
          normal_scenario[[l]][, s] <- (1 - rand_number) * normal_scenario[[l]][, s]
        }
      }
    }
    if (sim_setting == 1) {
    if (outlier_type == "shape outlier I") {
      #### Magnitude outlier 2
        for (s in outlier_index) {
          rand_number <- runif(1, 0.3, 0.5)
        normal_scenario[[1]][, s] <- rand_number * normal_scenario[[1]][, s]
        normal_scenario[[2]][, s] <- 2 * rand_number * normal_scenario[[2]][, s]
      }
    }
    if (outlier_type == "shape outlier II") {
      #### Magnitude outlier 2
        for (s in outlier_index) {
          rand_number <- runif(1, 1.6, 1.8)
          normal_scenario[[1]][, s] <- rand_number * normal_scenario[[1]][, s]
      }
    }
    if (outlier_type == "shape outlier III") {
      lambda <- seq(0, 1, length.out = nbtime)
      #### Magnitude outlier 2
      for (s in outlier_index) {
        r <- runif(1, 3, 4) * (1 + standard_grid)
        if (nbvar == 2) {
          normal_scenario[[1]][, s] <- r * cos(2 * pi * standard_grid)
          normal_scenario[[2]][, s] <- r * sin(2 * pi * standard_grid)
        }
        if (nbvar == 3) {
          normal_scenario[[1]][, s] <- r * cos(2 * pi * standard_grid)
          normal_scenario[[2]][, s] <- r * sin(2 * pi * standard_grid) * cos(pi * lambda)
          normal_scenario[[3]][, s] <- r * sin(2 * pi * standard_grid) * sin(pi * lambda)
        }
      }
    } 
    if (outlier_type == "shape outlier IV") {
      #### Magnitude outlier 2
      for (s in outlier_index) {
        lambda <- seq(0, 2, length.out = nbtime)
        r <- runif(1, 4, 5) * sin(4 * pi * standard_grid)
        if (nbvar == 2) {
          normal_scenario[[1]][, s] <- r * cos(2 * pi * standard_grid)
          normal_scenario[[2]][, s] <- r * sin(2 * pi * standard_grid)
        }
        if (nbvar == 3) {
          normal_scenario[[1]][, s] <- r * cos(2 * pi * standard_grid)
          normal_scenario[[2]][, s] <- r * sin(2 * pi * standard_grid) * cos(2 * pi * lambda)
          normal_scenario[[3]][, s] <- r * cos(2 * pi * standard_grid) * sin(2 * pi * lambda)
        }
      }
    }
    if (outlier_type == "shape outlier V") {
      
        #### Magnitude outlier 2
        for (s in outlier_index) {
          quard_len <- ceiling(nbtime/4)
          r <- runif(1, 4, 5)
          normal_scenario[[1]][, s] <- c(rep(r, quard_len), 
                                           seq(r, -r, length.out =  quard_len),
                                           rep(-r, quard_len),
                                           seq(-r, r, length.out =  nbtime - 3 * quard_len))
          normal_scenario[[2]][, s] <- c(seq(-r, r, length.out = quard_len),
                                           rep(r, quard_len), 
                                           seq(r, -r, length.out =  quard_len),
                                           rep(-r, nbtime - 3 * quard_len))
        
          if (nbvar == 3) {
            first_seq <- 2 * r * standard_grid[1:quard_len] + runif(1, -1.5, 1.5)
            second_seq <- rep(max(first_seq), quard_len)
            third_seq <- rev(first_seq)
            forth_seq <- rep(min(first_seq), nbtime - 3 * quard_len)
            normal_scenario[[3]][, s] <- c(first_seq, second_seq, third_seq, forth_seq)
          }
        }
    }  
  }
   if (sim_setting == 2 || sim_setting == 3 || sim_setting == 4) {
     if (outlier_type == "shape outlier I") {
       #### Magnitude outlier 2
       
         for (s in outlier_index) {
           rand_number <- runif(1, 0.8, 1)
           normal_scenario[[1]][, s] <- 1 / 5 * max(normal_scenario[[1]]) * rand_number * sin(2 * pi * standard_grid) * 
             normal_scenario[[1]][, s]
       }
     }
     if (outlier_type == "shape outlier II") {
       #### Magnitude outlier 2
         for (s in outlier_index) {
           rand_number <- runif(1, 0.8, 1)
           normal_scenario[[1]][, s] <- rand_number * normal_scenario[[1]][, s] + 
             rand_number * cos(pi * standard_grid)
           normal_scenario[[2]][, s] <- rand_number * normal_scenario[[2]][, s] + 
             2 * rand_number * sin(pi * standard_grid)
         }
     }
     if (outlier_type == "shape outlier III") {
       #### Magnitude outlier 2
      for (s in outlier_index) {
        rand_number <- runif(1, 8, 10)
        if (nbvar == 3) {
          normal_scenario[[2]][, s] <- rand_number * (standard_grid - 1/2)^2 + 
            normal_scenario[[2]][, s]
        }
        normal_scenario[[nbvar]][, s] <- rand_number * (standard_grid - 1/4)^2 + 
          normal_scenario[[nbvar]][, s]
      }
     }
   }

  if (visualize == TRUE) {
    pdf(file = paste("m", sim_setting,"_", outlier_type, ".pdf", sep = ""), 
        width = 5, height = 5)
    par(mar = c(3.5, 4, 3, 1), mgp = c(2.2, 1, 0))
    if (nbvar == 2) {
      plot(normal_scenario[[1]][, 1], normal_scenario[[2]][, 1], 
           ylim = range(normal_scenario[[2]]), 
           xlim = range(normal_scenario[[1]]), type = "n",
           xlab = "Variable 1", 
           ylab = "Variable 2", 
           main = paste("Model ", sim_setting," (", outlier_type, ")", sep = ""))
      for (l in setdiff(1:n, outlier_index)) {
        lines(normal_scenario[[1]][, l], normal_scenario[[2]][, l])
      }
      for (l in outlier_index) {
        lines(normal_scenario[[1]][, l], normal_scenario[[2]][, l], col = 2)
      }
    } else if (nbvar == 3) {
      s3d <- scatterplot3d::scatterplot3d(normal_scenario[[1]][, 1], 
                                          normal_scenario[[2]][, 1], 
                                          normal_scenario[[3]][, 1], 
                                          xlim = range(normal_scenario[[1]]), 
                                          ylim = range(normal_scenario[[2]]),
                                          zlim = range(normal_scenario[[3]]), 
                                          type = "n",
                                          xlab = "Variable 1", 
                                          ylab = "Variable 2", 
                                          zlab = "Variable 3", 
                                          main = "Simulations")
      for (l in setdiff(1:n, outlier_index)) {
        s3d$points3d(normal_scenario[[1]][, l], 
                     normal_scenario[[2]][, l], 
                     normal_scenario[[3]][, l], type = "l")
      }
      for (l in outlier_index) {
        s3d$points3d(normal_scenario[[1]][, l], 
                     normal_scenario[[2]][, l], 
                     normal_scenario[[3]][, l], type = "l", col = 2)
      }
    }
    dev.off()
  }
  return (list(normal_scenario, outlier_index))
}

