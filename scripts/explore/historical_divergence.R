
# Compute divergence between future predictions across different calibrations (in terms of Sorensen index)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

calibrations <- c(paste0(paste0("subset", 1:10),paste0("_rep", rep(1:10, each = 10))))

fit_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/fitted/fagus_sylvatica"
thresholds <- sapply(calibrations, function(c) readRDS(paste0(fit_dir, "/", "cmaes_fit_", c, ".rds"))$best_threshold)

# Load simulations
simulations <- as.data.frame(do.call(rbind, lapply(1:length(calibrations), function(i){
  sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"
  output <- read_mean_outputvalue(file.path(sim_dir, calibrations[i]), 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
  names(output) <- c("lat", "lon", "fitness")
  output$fitness <- ifelse(output$fitness < thresholds[i], 0, 1)
  output$cal <- calibrations[i]
  return(output)
})))

# Compute pairwise Sorensen index - by GCM
combs <- t(combn(x = unique(simulations$cal), m = 2))
sor <- apply(X = combs, MARGIN = 1, function(i){
  sim1 <- simulations[simulations$cal == i[1],]
  sim2 <- simulations[simulations$cal == i[2],]
  sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
  sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
    (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
  return(c(sorensen, paste0(i[1],".",i[2])))
})
sorensen_divergence_historical <- as.data.frame(t(sor))
sorensen_divergence_historical$V1 <- as.numeric(sorensen_divergence_historical$V1)
names(sorensen_divergence_historical) <- c("sorensen", "comb")
saveRDS(sorensen_divergence_historical, file = file.path(wd, "data", "metrics", "sorensen_divergence_historical.rds"))








