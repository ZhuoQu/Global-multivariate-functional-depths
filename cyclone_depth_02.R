##setwd("~/Dropbox/Mac/Desktop/KAUST/project/Depthnotion/new_code")
source("parameterization_PD.R")
source("multivariate_functional_depth.R")


clustl <- 2

index <- unlist(lapply(subsetlist(clust_westpacific$clust, clustl), 
                                            function(l) {l$element}))
data <- subset(whole_track_df, id %in% westpacific[index])

data_clust <- list()

select_index <- c()
st_index <- 1
for (l in 1:length(index)) {
  match_index <- which(data$id == unique(data$id)[l])
  if (length(match_index) >= 4) {
    select_index <- c(select_index, l)
    data_clust[[st_index]] <- list(argvals = data$Days[match_index], 
                            subj = rep(l, length(match_index)), 
                            y = cbind(data$cal_lon[match_index], data$lat[match_index]))
    st_index <- st_index + 1
  }
}


param_hurricane <- parameterization_PD(data_clust)
save(param_hurricane, file = "param_hurricane.RData")

hurricane_global_extremal <- mulitvariate_functional_depth(nbvar = 2, 
                                                      param_PD = param_hurricane,
                                                      point_depth_comp = "global",
                                                      type = "extremal depth", 
                                                      mt_depth = "halfspace depth", weight = "time")
save(hurricane_global_extremal, file = "hurricane_global_extremal.RData")

hurricane_global_integrated <- mulitvariate_functional_depth(nbvar = 2, 
                                                             param_PD = param_hurricane,
                                                            point_depth_comp = "global",
                                                            type = "integrated depth", 
                                                            mt_depth = "halfspace depth", weight = "time")
save(hurricane_global_integrated, file = "hurricane_global_integrated.RData")

