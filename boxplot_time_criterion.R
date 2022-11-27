source("subsetlist.R")
boxplot_time_criterion <- function(param_PD, median_index, centralRegion_index, outlier_candidate, thres) {
  centraldata <- subsetlist(param_PD, centralRegion_index)
  centralRegion_length <- floor(quantile(sapply(centraldata, function(k) {length(k$argvals)}), thres))
  sample_size <- length(param_PD)
  nbvar <- ncol(param_PD[[1]]$y)
  available_grid <- unlist(lapply(centraldata, function(k) {k$argvals}))
  centralRegion_grid <- seq(min(available_grid), max(available_grid), length.out = centralRegion_length)
  central_obs <- lapply(1:(length(centralRegion_grid) - 1), function(l) {
    lapply(1:nbvar, function(varnm) {
      unlist(lapply(centraldata, function(subl) {
        match_ind <- intersect(which(subl$argvals >= centralRegion_grid[l]),
                               which(subl$argvals < centralRegion_grid[l + 1]))
        if (length(match_ind) > 0) {
          return (subl$y[match_ind, varnm])
        }
      }))
    })
  })
  
  observe_prop <- sapply(central_obs, function(l) {length(l[[1]])/ length(centralRegion_index)})
  observe_prop <- ifelse(observe_prop > 1, 1, observe_prop)
  ################################### remove the grid with no missing values
  no_observation_index <- which(sapply(central_obs, function(k) {length(k[[1]]) == 0}) == TRUE)
  if (sum(no_observation_index) > 0) {
    centralRegion_grid <- centralRegion_grid[-no_observation_index]
    central_obs <- subsetlist(central_obs, setdiff(1:(centralRegion_length - 1), no_observation_index))
  }

  ####################################
  centralRegion_min <- lapply(1:nbvar, function(varnm) {
    sapply(central_obs, function(k) {min(k[[varnm]])})})
  centralRegion_max <- lapply(1:nbvar, function(varnm) {
    sapply(central_obs, function(k) {max(k[[varnm]])})})
  centralRegion_range <- lapply(1:nbvar, function(varnm) {
    centralRegion_max[[varnm]] - centralRegion_min[[varnm]]})
  
  centralRegion_separation <- lapply(1:nbvar, function(varnm) {
    centralRegion_min[[varnm]] + observe_prop * centralRegion_range[[varnm]] 
  })
  
  outlier_upper_thres <- lapply(1:nbvar, function(varnm) {
    centralRegion_max[[varnm]] + 1.5 * centralRegion_range[[varnm]]})
  outlier_lower_thres <- lapply(1:nbvar, function(varnm) {
    centralRegion_min[[varnm]] - 1.5 * centralRegion_range[[varnm]]})
  
  
  remove_comp_index <- sort(unique(unlist(lapply(centralRegion_range, function(k) {
    l <- boxplot(k, plot = FALSE)
    if (length(l$out < l$stats[1, 1]) > 0) {
      return (which (k %in% l$out[l$out < l$stats[1, 1]]))}
  }))))
  
  if (length(remove_comp_index) == 0) {
    remove_comp_index2 <- sort(unique(unlist(lapply(centralRegion_range, function(k) {
      l <- boxplot(log(k + 0.1), plot = FALSE)
      if (length(l$out < l$stats[1, 1]) > 0) {
        return (which (log(k + 0.1) %in% l$out[l$out < l$stats[1, 1]]))}
    }))))
    
    remove_comp_index <- sort(unique(c(remove_comp_index, remove_comp_index2)))
  }
  
  ##### remove the index with the smaller observation times
  observe_count <- sapply(central_obs, function(k) {length(k[[1]])})
  boxplot_observe_count <- boxplot(observe_count, plot = FALSE)
  if (length(boxplot_observe_count$out) == 0) {
    boxplot_observe_count <- boxplot(log(observe_count), plot = FALSE)
    added_index <- which(log(observe_count) %in% boxplot_observe_count$out)
    if (length(added_index) > 0) {
      remove_comp_index <- unique(c(remove_comp_index, added_index))
    }
  }
  
  samp <- setdiff(1:sample_size, union(centralRegion_index, outlier_candidate))
  candidate <- subsetlist(param_PD, samp)
  outlier_index <- lapply(1:nbvar, function(varnm) {
    out <- c()
    for (v in 1:length(samp)) {
      for (l in setdiff(1:(length(centralRegion_grid) - 1), remove_comp_index) ) {
        ind <- intersect(which(candidate[[v]]$argvals < centralRegion_grid[l + 1]),
                         which(candidate[[v]]$argvals >= centralRegion_grid[l]))
        yvalue <- candidate[[v]]$y[ind, varnm]
        if (length(yvalue) > 0 && (max(yvalue) > outlier_upper_thres[[varnm]][l] || min(yvalue) < outlier_lower_thres[[varnm]][l])) {
          
          cat(v, "-", samp[v],"-", l, "\n")
          out <- c(out, samp[v])
          break
        }
      }
    }
   return (sort(out))
  })
  
  Region_length <- max(centralRegion_length, floor(quantile(sapply(param_PD, function(k) {length(k$argvals)}), thres)))
  if (Region_length == centralRegion_length) {
    Region_grid <- centralRegion_grid
  } else {
    all_grid <- unlist(lapply(param_PD, function(k) {k$argvals}))
    Region_grid <- c(centralRegion_grid, 
                   seq(max(centralRegion_grid) + 
                   (range(all_grid)[2] - range(all_grid)[1]) / Region_length, 
                   max(all_grid), 
                   length.out = max(Region_length - centralRegion_length, 3)))
  }
  
  non_outlying_obs <- lapply(1:(length(Region_grid) - 1), function(l) {
    lapply(1:nbvar, function(varnm) {
      non_outlying_index <- setdiff(1:sample_size, union(outlier_index[[varnm]], outlier_candidate))
      non_outlying_data <- subsetlist(param_PD, non_outlying_index)
      unlist(lapply(non_outlying_data, function(subl) {
        match_ind <- intersect(which(subl$argvals >= Region_grid[l]),
                               which(subl$argvals < Region_grid[l + 1]))
        if (length(match_ind) > 0) {
          return (subl$y[match_ind, varnm])
        }
      }))
    })
  })
  
  ################################### remove the grid with no missing values
  no_observation_index <- which(sapply(non_outlying_obs, function(k) {length(k[[1]]) == 0}) == TRUE)
  if (sum(no_observation_index) > 0) {
    non_outlying_obs <- subsetlist(non_outlying_obs, setdiff(1:(length(Region_grid) - 1), no_observation_index))
    Region_grid <- Region_grid[-no_observation_index]
  }
  
  non_outlying_Region_min <- lapply(1:nbvar, function(varnm) {
    sapply(non_outlying_obs, function(k) {min(k[[varnm]])})})
  non_outlying_Region_max <- lapply(1:nbvar, function(varnm) {
    sapply(non_outlying_obs, function(k) {max(k[[varnm]])})})
  
  plot_inf <- list(centralgrid = centralRegion_grid, 
                   nonoutlyinggrid = Region_grid,
                   central_min = centralRegion_min, 
                   central_max = centralRegion_max, 
                   central_separation = centralRegion_separation,
                   nonoutlying_min = non_outlying_Region_min,
                   nonoutlying_max = non_outlying_Region_max)
  descriptive_stat <- list(med = median_index, 
                           central = centralRegion_index, 
                           functional_outlier = outlier_index,
                           domain_outlier = outlier_candidate,
                           observe_prop = observe_prop,
                           extra_inf = plot_inf)
  return (descriptive_stat)
}
