source("boxplot_criterion.R")
source("boxplot_time_criterion.R")
source("simplified_sparse_boxplot.R")
source("simplified_sparse_intensity_boxplot.R")
load("param_hurricane.RData")
load("hurricane_global_extremal.RData")
load("hurricane_global_integrated.RData")

extremal_boxplot <- boxplot_criterion(nbvar = 2, 
                                      param_PD = param_hurricane, 
                                      Dmat = hurricane_global_extremal, 
                                      plotboxplot = 1, 
                                      outlier_candidate = NULL,
                                      titles = c("Simplified Sparse Functional Boxplot: Longitude",
                                                 "Simplified Sparse Functional Boxplot: Latitude"), 
                                      ylabs = c("Longitude", "Latitude"))

domain_outlier <- extremal_boxplot$domain_outlier
select_index[domain_outlier]
function_outlier <- unique(unlist(extremal_boxplot$functional_outlier))
extremal_intensity_boxplot <- boxplot_criterion(nbvar = 2, 
                                                param_PD = param_hurricane, 
                                                Dmat = hurricane_global_extremal, 
                                                plotboxplot = 2, 
                                                outlier_candidate = NULL,
                                                titles = c("Simplified Intensity Sparse Functional Boxplot: Longitude",
                                                           "Simplified Intensity Sparse Functional Boxplot: Latitude"), 
                                                ylabs = c("Longitude", "Latitude"))



################################################################################
loc <- ifelse(clustl == 1, "(a)", "(b)")
param_hurricane_dataframe <- data.frame(Days = unlist(lapply(param_hurricane, function(k) {k$argvals})), 
                                        id = unlist(lapply(param_hurricane, function(k) {k$subj})), 
                                        longitude = unlist(lapply(param_hurricane, function(k) {k$y[, 1]})), 
                                        latitude = unlist(lapply(param_hurricane, function(k) {k$y[, 2]})))

################################################################################
integrated_boxplot <- boxplot_criterion(nbvar = 2, 
                                        param_PD = param_hurricane, 
                                        Dmat = hurricane_global_integrated, 
                                        plotboxplot = 1, 
                                        outlier_candidate = NULL,
                                        titles = c("Simplified Sparse Functional Boxplot: Longitude",
                                                   "Simplified Sparse Functional Boxplot: Latitude"), 
                                        ylabs = c("Longitude", "Latitude"))
domain_outlier <- integrated_boxplot$domain_outlier
select_index[domain_outlier]
function_outlier <- unique(unlist(integrated_boxplot$functional_outlier))
integrated_intensity_boxplot <- boxplot_criterion(nbvar = 2, 
                                                  param_PD = param_hurricane, 
                                                  Dmat = hurricane_global_integrated, 
                                                  plotboxplot = 2, 
                                                  outlier_candidate = NULL,
                                                  titles = c("Simplified Intensity Sparse Functional Boxplot: Longitude",
                                                             "Simplified Intensity Sparse Functional Boxplot: Latitude"), 
                                                  ylabs = c("Longitude", "Latitude"))

#################################################

unique(subset(data, id %in% unique(data$id)[domain_outlier[1]])$year)
unique(subset(data, id %in% unique(data$id)[domain_outlier[2]])$year)
unique(subset(data, id %in% unique(data$id)[domain_outlier[3]])$year)

#################################################
#loc <- ifelse(clustl == 1, "(a)", "(b)")
par(mfrow = c(1, 1), mar = c(4.2, 4, 2, 1), mgp = c(2.2, 1, 0))
plot(rank(hurricane_global_integrated[, 1]), 
     rank(hurricane_global_extremal[, 1]),
     xlab = "Rank based on GMFID", ylab = "Rank based on GMFED",
     main = "Rank-Rank Plot")

extreme_index_GMFID <- order(hurricane_global_integrated, decreasing = FALSE)[1:round(0.1 * length(param_hurricane))]
extreme_index_GMFED <- order(hurricane_global_extremal, decreasing = FALSE)[1:round(0.1 * length(param_hurricane))]
cross_index <- intersect(extreme_index_GMFID, extreme_index_GMFED)

points(rank(hurricane_global_integrated[, 1])[extreme_index_GMFID],
       rank(hurricane_global_extremal[, 1])[extreme_index_GMFID], col = "red")

points(rank(hurricane_global_integrated[, 1])[extreme_index_GMFED],
       rank(hurricane_global_extremal[, 1])[extreme_index_GMFED], col = "blue")

points(rank(hurricane_global_integrated[, 1])[cross_index],
       rank(hurricane_global_extremal[, 1])[cross_index], col = "purple")

most_potential_outlier <- cross_index
second_potential_outlier <- setdiff(union(extreme_index_GMFED, 
                                          extreme_index_GMFID), cross_index)
# neighbour_number <- apply(neighbours_westpacific[index, index], 2, sum)




#### Visualization before the potential outlier detection
median <- which(hurricane_global_integrated[, 1] == max(hurricane_global_integrated[, 1]))
firstquarter <- order(hurricane_global_integrated[, 1], decreasing = T)[1:round(0.25 * (length(index)))]
secondquarter <- order(hurricane_global_integrated[, 1], decreasing = T)[round(0.25 * (length(index))+1):round(0.5 * (length(index)))]
thirdquarter <- order(hurricane_global_integrated[, 1], decreasing = T)[round(0.5 * (length(index)) + 1):round(0.75 * (length(index)))]
# shift coordinates to recenter worldmap
pdf(file = paste("Cyclone_GMFID_before_outlier.pdf", sep = ""), width = 6, height = 4)
par(mfrow = c(1, 1), mar = c(4.2, 4, 2, 1), mgp = c(2.2, 1, 0))
ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_path(aes(group = group), 
            #fill = "#f9f9f9", 
            colour = "grey65") + 
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(78, 198)) +
  coord_equal() +  theme_bw()  +
  geom_path(aes(x = longitude, y = latitude, group = id, alpha = (22 - Days) / 22),
            data = subset(param_hurricane_dataframe, id %in% thirdquarter), color = "pink") +
  geom_path(aes(x = longitude, y = latitude, group = id, alpha = (22 - Days) / 22),
            data = subset(param_hurricane_dataframe, id %in% secondquarter), color = "magenta") +
  geom_path(aes(x = longitude, y = latitude, group = id, alpha = (22 - Days) / 22),
            data = subset(param_hurricane_dataframe, id %in% firstquarter), color = "purple") +
  ##geom_path(aes(x = longitude, y = latitude, group = id),
  ##           data = subset(param_hurricane_dataframe, id %in% most_potential_outlier), color = "blue", linetype = "dashed", size = 0.9) +
  ## geom_path(aes(x = longitude, y = latitude, group = id), 
  ##           data = subset(param_hurricane_dataframe, id %in% second_potential_outlier), color = "yellow", linetype = "dashed", size = 0.9) +
  geom_path(aes(x = longitude, y = latitude, group = id),
            data = subset(param_hurricane_dataframe, id %in% domain_outlier), color = "green", size = 0.7) +
  geom_path(aes(x = longitude, y = latitude, group = id), 
            data = subset(param_hurricane_dataframe, id %in% function_outlier), color = "red", size = 0.7) +
  geom_path(aes(x = longitude, y = latitude, group = id),
            data = subset(param_hurricane_dataframe, id %in% median), color = "black", size = 0.7) +
  #guides(fill= guide_legend(title = "Time")) +
  #labs(fill = "Time") +
  theme(legend.position = "none") +
  
  labs(title = paste("Northwest Pacific Cyclone Tracks (Before Potential Outlier Detection)", sep = ""),
       x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 13),
        plot.margin = margin(1, 1.2, 1, 1.2)) + 
  scale_colour_gradient(low = "red", high = "yellow", na.value = NA,
                        breaks = round(seq(0, max(data$Days), length.out = 6), 1), 
                        labels = round(seq(0, max(data$Days), length.out = 6), 0),
                        limits = c(0, round(max(data$Days))))
dev.off()

################# Visualization after the potential outlier detection
union_outlier <- unique(c(domain_outlier, function_outlier,
                          most_potential_outlier, second_potential_outlier))
### Update the first, second and third quartile
update_size <- length(param_hurricane) - length(union_outlier)
update_median <- which(hurricane_global_integrated[, 1] == max(hurricane_global_integrated[-union_outlier, 1]))

update_firstquarter <- setdiff(order(hurricane_global_integrated[, 1], decreasing = T)[1:round(0.25 * update_size)], union_outlier)
update_secondquarter <- setdiff(order(hurricane_global_integrated[, 1], decreasing = T)[round(0.25 * update_size+1):round(0.5 * (update_size))], union_outlier)
update_thirdquarter <- setdiff(order(hurricane_global_integrated[, 1], decreasing = T)[round(0.5 * update_size + 1):round(0.75 * update_size)], union_outlier)

pdf(file = paste("Cyclone_GMFID_after_outlier.pdf", sep = ""), width = 6, height = 4)
ggplot(aes(x = long, y = lat), data = worldmap) + 
  geom_path(aes(group = group), 
            #fill = "#f9f9f9", 
            colour = "grey65") + 
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(78, 198)) +
  coord_equal() +  theme_bw()  +
  geom_path(aes(x = longitude, y = latitude, group = id, alpha = (22 - Days) / 22),
            data = subset(param_hurricane_dataframe, id %in% update_thirdquarter), color = "pink") +
  geom_path(aes(x = longitude, y = latitude, group = id, alpha = (22 - Days) / 22),
            data = subset(param_hurricane_dataframe, id %in% update_secondquarter), color = "magenta") +
  geom_path(aes(x = longitude, y = latitude, group = id, alpha = (22 - Days) / 22),
            data = subset(param_hurricane_dataframe, id %in% update_firstquarter), color = "purple") +
  geom_path(aes(x = longitude, y = latitude, group = id), 
            data = subset(param_hurricane_dataframe, id %in% second_potential_outlier), color = "yellow", size = 0.7) +
  geom_path(aes(x = longitude, y = latitude, group = id),
            data = subset(param_hurricane_dataframe, id %in% most_potential_outlier), color = "blue", size = 0.7) +
  geom_path(aes(x = longitude, y = latitude, group = id),
            data = subset(param_hurricane_dataframe, id %in% domain_outlier), color = "green", size = 0.7) +
  geom_path(aes(x = longitude, y = latitude, group = id), 
            data = subset(param_hurricane_dataframe, id %in% function_outlier), color = "red", size = 0.7) +
  geom_path(aes(x = longitude, y = latitude, group = id),
            data = subset(param_hurricane_dataframe, id %in% update_median), color = "black", size = 0.7) +
  #guides(fill= guide_legend(title = "Time")) +
  #labs(fill = "Time") +
  theme(legend.position = "none") +
  
  labs(title = paste("Northwest Pacific Cyclone Tracks (After Potential Outlier Detection)", sep = ""),
       x = "Longitude", y = "Latitude") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 13),
        plot.margin = margin(1, 1.2, 1, 1.2)) + 
  scale_colour_gradient(low = "red", high = "yellow", na.value = NA,
                        breaks = round(seq(0, max(data$Days), length.out = 6), 1), 
                        labels = round(seq(0, max(data$Days), length.out = 6), 0),
                        limits = c(0, round(max(data$Days))))
dev.off()



# par(mfrow = c(1, 1), mar = c(4.2, 4, 2, 1), mgp = c(2.2, 1, 0))
# dx <- sapply(param_hurricane, function(k) {max(k$argvals)})
# hist(dx, xlim = c(0, 23),
#      xlab = "Days", ylab = "Density", breaks = 20, probability = T,
#      main = "Histogram of the Northwest Pacific Cyclones", cex.lab = 1.3)
# lines(density(dx, kernel = "gaussian"), col = 4, lwd = 2)
# 
#abline(v = 3, col = "red", lty = 2)
#abline(v = 21.5, col = "red", lty = 2)