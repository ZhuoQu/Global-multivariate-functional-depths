source("subsetlist.R")
simplified_sparse_boxplot <- function(param_PD, info, outlier_candidate, 
                                      param, titles, ylabs, 
                                      showoutlier = TRUE) {
  sample_size <- length(param_PD)
  nbvar <- ncol(param_PD[[1]]$y)
  centralgrid <- info$extra_inf$centralgrid[-length(info$extra_inf$centralgrid)]
  nonoutlyinggrid <- info$extra_inf$nonoutlyinggrid[-length(info$extra_inf$nonoutlyinggrid)]
  median_info <- subsetlist(param_PD, info$med)
  y <- median_info[[1]]$y
  if (param == 1) {
    x <- median_info[[1]]$argvals
    xlim <- range(sapply(param_PD, function(k) {k$argvals}))
    xlab <- "Time"
  } else if (param == 2) {
    x <- median_info[[1]]$time_percentage
    xlim <- range(sapply(param_PD, function(k) {k$time_percentage}))
    xlab <- "Time Percentage"
  } 
  
  sep <- ifelse(nbvar <= 3, nbvar, 3)
  par(mfrow = c(1, sep), mai = c(0.5, 0.55, 0.5, 0.07), 
      mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))
  
  for (varnm in 1:nbvar) {
    outlier_info <- subsetlist(param_PD, info$functional_outlier[[varnm]])
    barval <- (centralgrid[1] + centralgrid[length(centralgrid)])/2
    bar <- which(sort(c(centralgrid, barval)) == barval)[1]
    maxcurve <- smooth(info$extra_inf$nonoutlying_max[[varnm]], kind = "3RS3R")
    mincurve <- smooth(info$extra_inf$nonoutlying_min[[varnm]], kind = "3RS3R")
    
    xx <- c(centralgrid, centralgrid[order(centralgrid, decreasing = TRUE)])
    sup <- smooth(info$extra_inf$central_max[[varnm]], kind = "3RS3R")
    inf <- smooth(info$extra_inf$central_min[[varnm]], kind = "3RS3R")
    supinv = sup[order(centralgrid, decreasing = TRUE)]
    yy <- c(inf, supinv)
    sep <- smooth(info$extra_inf$central_separation[[varnm]], kind = "3RS3R")
    yy_separation <- c(inf, sep[order(centralgrid, decreasing = TRUE)])
    reference_line <- (inf + sup) / 2 
    #### Plot the median
    plot(x, y[, varnm], lty = 1, lwd = 2, col = 1, type = "n", 
       xlim = xlim, 
       ylim = range(lapply(param_PD, function(k) {k$y[, varnm]})),
       xlab = xlab, 
       ylab = ylabs[varnm], 
       main = titles[varnm])

    lines(c(centralgrid[bar], centralgrid[bar]), c(maxcurve[bar], sup[bar]), 
          col = 4, lwd = 2)
    lines(c(centralgrid[bar], centralgrid[bar]), c(mincurve[bar], inf[bar]), 
          col = 4, lwd = 2)
    
    if (showoutlier == TRUE) {
      polygon(xx[is.finite(yy)], yy[is.finite(yy)], col = "grey", border = 4, lwd = 2)
      polygon(xx[is.finite(yy)], yy_separation[is.finite(yy_separation)], col = 6, border = 4, lwd = 2)
      lines(nonoutlyinggrid, maxcurve, col = 4, lwd = 2)
      lines(nonoutlyinggrid, mincurve, col = 4, lwd = 2)
    }
    
    if (param == 1) {
      lapply(outlier_info, function(k) {
        lines(k$argvals, k$y[, varnm], lwd = 2, col = "grey", type = "l")
        lines(k$argvals, k$y[, varnm], lwd = 2, lty = 2, col = "red")
      })
      
      lapply(outlier_candidate, function(k) {
        lines(param_PD[[k]]$argvals, 
              param_PD[[k]]$y[1:length(param_PD[[k]]$argvals), varnm], 
              lwd = 2, col = "grey", type = "l")
        lines(param_PD[[k]]$argvals, 
               param_PD[[k]]$y[1:length(param_PD[[k]]$argvals), varnm], 
               lwd = 2, lty = 2, col = "green")
      })
    } else if (param == 2) {
      lapply(outlier_info, function(k) {
        lines(k$time_percentage, k$y[, varnm], lwd = 2, col = "grey", type = "l")
        lines(k$time_percentage, k$y[, varnm], lty = 2, lwd = 2, col = "red")
      })
      lapply(outlier_candidate, function(k) {
        lines(param_PD[[k]]$time_percentage, 
              param_PD[[k]]$y[1:length(param_PD[[k]]$time_percentage), varnm], 
              lwd = 2, col = "grey", type = "l")
        lines(param_PD[[k]]$time_percentage, 
               param_PD[[k]]$y[1:length(param_PD[[k]]$time_percentage), varnm], 
               lwd = 2, lty = 2, col = "green")
      })
    }
    
    if (showoutlier == FALSE) {
      polygon(xx[is.finite(yy)], yy[is.finite(yy)], col = "grey", border = 4, lwd = 2)
      polygon(xx[is.finite(yy)], yy_separation[is.finite(yy_separation)], col = 6, border = 4, lwd = 2)
      lines(nonoutlyinggrid, maxcurve, col = 4, lwd = 2)
      lines(nonoutlyinggrid, mincurve, col = 4, lwd = 2)
    }
    
    lines(x[-length(x)], y[-length(x), varnm], lwd = 2, col = "white", type = "l")
    lines(x[-length(x)], y[-length(x), varnm], lty = 2, lwd = 2)
    lines(centralgrid, reference_line, lty = 2, lwd = 1.5, col = "cyan")
  }
}
