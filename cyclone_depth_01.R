### This file is used for the depth paper
### final_cyclone_00.R needs to be run first

packages <- c("mapdata", "ggplot2", "readr","dplyr", "maps", 
              "rnaturalearth", "rnaturalearthdata", "rgeos",
              "ggspatial", "plotrix")

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

source("subsetlist.R")
# source("01_algorithm_distance.R")
# source("02_algorithm_clustering.R")
# source("03_algorithm_find_outlier.R")
# source("find_optimal_theta.R")
#source("final_cyclone_00.R")

###################################
load("neighbours_westpacific.RData")
load("pacific_index.RData")    #### westpacific
whole_track_df <- read_csv('whole_track_cyclone.csv')
whole_track_df$Days <- whole_track_df$time * whole_track_df$duration
load("clust_westpacific.RData") ## RData should be under the application directory
outlier_index <- clust_westpacific$isolate

#############################################
worldmap <- map_data ("world", wrap = c(0, 360))
############################################# figures of clustering result
clustl <- 2
  #loc <- ifelse(clustl == 1, "(a)", "(b)")
  index <- unlist(lapply(subsetlist(clust_westpacific$clust, clustl), 
                         function(l) {l$element}))
  # neighbour_number <- apply(neighbours_westpacific[index, index], 2, sum)
  # median <- index[which(neighbour_number == max(neighbour_number))][1]
  # firstquarter <- index[order(neighbour_number, decreasing = T)[1:round(0.25 * (length(neighbour_number)))]]
  # secondquarter <- index[order(neighbour_number, decreasing = T)[round(0.25 * (length(neighbour_number))+1):round(0.5 * (length(neighbour_number)))] ]
  # thirdquarter <- index[order(neighbour_number, decreasing = T)[round(0.5 * (length(neighbour_number)) + 1):round(0.75 * (length(neighbour_number)))] ]

## Functions versus days
par(mfrow = c(1, 1), mai = c(0.5, 0.55, 0.3, 0.07), 
    mar = c(3.5, 3.5, 2, 1), mgp = c(2, 1, 0))

  clustl <- 2
  #loc <- ifelse(clustl == 1, "(c)", "(d)")
  index <- unlist(lapply(subsetlist(clust_westpacific$clust, clustl), 
                         function(l) {l$element}))
  data <- subset(whole_track_df, id %in% westpacific[index])
  maximum_day <- max(data$duration)
  # neighbour_number <- apply(neighbours_westpacific[index, index], 2, sum)
  # median <- index[which(neighbour_number == max(neighbour_number))][1]
  # firstquarter <- index[order(neighbour_number, decreasing = T)[1:round(0.25 * (length(neighbour_number)))]]
  # secondquarter <- index[order(neighbour_number, decreasing = T)[round(0.25 * (length(neighbour_number))+1):round(0.5 * (length(neighbour_number)))] ]
  # thirdquarter <- index[order(neighbour_number, decreasing = T)[round(0.5 * (length(neighbour_number)) + 1):round(0.75 * (length(neighbour_number)))] ]
  # shift coordinates to recenter worldmap
  pdf(file = paste("clust_", clustl, "_depth_days.pdf", sep = ""), width = 6, height = 4)
  ggplot(aes(x = long, y = lat), data = worldmap) + 
    geom_path(aes(group = group), 
              #fill = "#f9f9f9", 
              colour = "grey65") + 
    scale_y_continuous(limits = c(0, 65)) +
    scale_x_continuous(limits = c(78, 198)) +
    coord_equal() +  theme_bw() +
    # geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time), 
    #           data = subset(whole_track_df, id %in% westpacific[outlier_index]), color = "red") +
    # geom_path(aes(x = cal_lon, y = lat,group = id, alpha = 1 - time),
    #           data = subset(whole_track_df, id %in% westpacific[thirdquarter]), color = "pink") +
    # geom_path(aes(x = cal_lon, y = lat, group = id, alpha = 1 - time),
    #           data = subset(whole_track_df, id %in% westpacific[secondquarter]), color = "magenta") +
    # geom_path(aes(x = cal_lon, y = lat,group = id, alpha = 1 - time),
    #           data = subset(whole_track_df, id %in% westpacific[firstquarter]), color = "purple") +
    # geom_path(aes(x = cal_lon, y = lat,group = id, alpha = 1 - time),
    #           data = subset(whole_track_df, id %in% westpacific[median], col = grey(time)), color = "black") + 
    geom_path(aes(x = cal_lon, y = lat, group = id, colour = Days),
              data = subset(whole_track_df, id %in% westpacific[index])) +
    guides(fill = guide_legend(title = "Time"))+
    #labs(fill = "Time") +
    #theme(legend.position = "none") +
    
    labs(title = paste(" Northwest Pacific Cyclone Tracks", sep = ""),
         x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 13),
          plot.margin = margin(1, 1.2, 1, 1.2)) + 
    scale_colour_gradient(low = "red", high = "yellow", na.value = NA,
                          breaks = round(seq(0, maximum_day, length.out = 5), 1), 
                          labels = round(seq(0.0, maximum_day, length.out = 5), 0),
                          limits = c(0, round(maximum_day)))
  dev.off()


#