packages <- c( "spatstat.geom", "spatstat", "plotfunctions")

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
source("subsetlist.R")
colorsplit = 40
simplified_sparse_intensity_boxplot <- function(param_PD, info, outlier_candidate, 
                                      param, titles, ylabs, 
                                      colorsplit = 40, colorrange =  NULL,
                                      showoutlier = FALSE, showcontour = FALSE, 
                                      showlegend = TRUE) {
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
  if (showlegend == TRUE && length(info$domain_outlier) > 0) {
    colnum <- nbvar + 1
    if (nbvar >= 2) {
      major_width <- (0.94 + 0.01 * nbvar)
    } 
    if (nbvar == 1) {
      major_width <- (0.91 + 0.01 * nbvar)
    }
    m <- matrix(1:colnum, nrow = 1, ncol = colnum, byrow = TRUE)
    layout(mat = m, 
           widths = c(rep(major_width / (colnum - 1), colnum - 1), 1 - major_width))
  } else {
    colnum <- min(nbvar, 3)
    m <- matrix(1:colnum, nrow = 1, ncol = colnum, byrow = TRUE)
    layout(mat = m, 
           widths = rep(1 / colnum, colnum))
  }
  par(mai = c(0.7, 0.8, 0.4, 0.1), mar = c(4.5, 4.2, 2, 0.5),
      mgp = c(3, 1.5, 0))
  
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
    ######################
    xpoint <- unlist(lapply(subsetlist(param_PD, info$central), function(k) {k$argvals}))
    ypoint <- unlist(lapply(subsetlist(param_PD, info$central), function(k) {k$y[, varnm]}))
    pp_obs <- ppp(xpoint, ypoint, poly = list(x = xx, y = yy))
    Q.d <- density.ppp(pp_obs, adjust = 1, dimyx = c(200, 200), at = "pixels")
    norm_Q.d <- Q.d
    
    norm_Q.d <- Q.d
    if (length(colorrange) == 0) {
      rg <- range(Q.d[['v']], na.rm = TRUE)
    } else {
      rg <- range(unlist(colorrange))
    }
    norm_Q.d$v <- 1 - Q.d$v / (max(rg, na.rm=TRUE))
    select_col <- c("magenta", "tomato", "gold", "yellow", "white")
    grRd <- colorRampPalette(select_col, space = "rgb")  ####################################################################
    CO <- colourmap(grRd(colorsplit), range = c(0, 1))
  
    #######################
    #### Plot the median
    plot(x, y[, varnm], lty = 1, lwd = 2, col = 1, type = "n", 
         xlim = xlim, 
         ylim = range(lapply(param_PD, function(k) {k$y[, varnm]})),
         xlab = xlab, 
         ylab = ylabs[varnm], 
         main = titles[varnm], cex.main = 1.5, cex.lab = 1.5)
    
    lines(c(centralgrid[bar], centralgrid[bar]), c(maxcurve[bar], sup[bar]), 
          col = 4, lwd = 2)
    lines(c(centralgrid[bar], centralgrid[bar]), c(mincurve[bar], inf[bar]), 
          col = 4, lwd = 2)
    
    if (showoutlier == TRUE) {
      polygon(xx[is.finite(yy)], yy[is.finite(yy)], col = "magenta", border = 4, lwd = 2)
      plot(norm_Q.d, las = 1, add = TRUE, col = CO)  # Plot density raster
      if (showcontour == TRUE) {
        contour(norm_Q.d, add = TRUE, drawlabels = FALSE, cex = 0.7, levels = c(0, 0.2, 0.4, 0.6, 0.8, 1))
      }
      lines(nonoutlyinggrid, maxcurve, col = 4, lwd = 2)
      lines(nonoutlyinggrid, mincurve, col = 4, lwd = 2)
    }
    
    if (param == 1) {
      lapply(outlier_info, function(k) {
        lines(k$argvals, k$y[, varnm], col = "grey", lwd = 2, type = "l")
        lines(k$argvals, k$y[, varnm], lty = 2, lwd = 2, col = "red")
      })
      
      lapply(outlier_candidate, function(k) {
        lines(param_PD[[k]]$argvals, 
              param_PD[[k]]$y[1:length(param_PD[[k]]$argvals), varnm], 
              col = "grey", type = "l", lwd = 2)
        lines(param_PD[[k]]$argvals, 
               param_PD[[k]]$y[1:length(param_PD[[k]]$argvals), varnm], 
               lty = 2, col = "green",lwd = 2)
      })
    } else if (param == 2) {
      lapply(outlier_info, function(k) {
        lines(k$time_percentage, k$y[, varnm], col = "grey", type = "l")
        lines(k$time_percentage, k$y[, varnm], lty = 2, lwd = 2, col = "red")
      })
      lapply(outlier_candidate, function(k) {
        lines(param_PD[[k]]$time_percentage, 
              param_PD[[k]]$y[1:length(param_PD[[k]]$time_percentage), varnm], 
              col = "grey", type = "l", lwd = 2)
        lines(param_PD[[k]]$time_percentage, 
               param_PD[[k]]$y[1:length(param_PD[[k]]$time_percentage), varnm], 
              lty = 2, col = "green", lwd = 2)
      })
    }
    
    if (showoutlier == FALSE) {
      polygon(xx[is.finite(yy)], yy[is.finite(yy)], col = "magenta", border = 4, lwd = 2)
      plot(norm_Q.d, las = 1, add = TRUE, col = CO)
      if (showcontour == TRUE) {
        contour(norm_Q.d, add = TRUE, drawlabels = FALSE, cex = 0.7, levels = c(0, 0.2, 0.4, 0.6, 0.8, 1))
      }
      lines(nonoutlyinggrid, maxcurve, col = 4, lwd = 2)
      lines(nonoutlyinggrid, mincurve, col = 4, lwd = 2)
    }
    
    lines(x[-length(x)], y[-length(x), varnm], lwd = 2, col = "grey", type = "l")
    lines(x[-length(x)], y[-length(x), varnm], lty = 2, lwd = 2)
  }
  
  if (showlegend == TRUE && length(info$domain_outlier) > 0) {
    par(mai = c(0.6, 0.22, 0.95, 0.05))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "%")
    gradientLegend(valRange = c(0, 100), color = select_col,
                   length = 1, depth = 0.45, side = 4, dec = 0,
                   inside = TRUE, n.seg = 4, cex = 0.7,
                   pos = c(0.3, 0, 0.7, 1.07), coords = FALSE)
  }
  
}
