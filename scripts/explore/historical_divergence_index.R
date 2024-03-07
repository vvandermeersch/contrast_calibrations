
# Compute divergence between paleo predictions across different calibrations (in terms of Sorensen index)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
library(future.apply)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

calibrations <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))
fit_dir <- file.path(wd, "data", "fit")

species <- "fagus_sylvatica"

# Load simulations
plan(multisession, workers = 20)
simulations <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
  
  sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
  output <- read_mean_outputvalue(file.path(sim_dir, species, c), 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
  names(output) <- c("lat", "lon", "fitness")
  
  threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
  
  output$fitness <- ifelse(output$fitness < threshold, 0, 1)
  output$cal <- c
  return(output)
  
})))
plan(sequential); gc()

# Compute pairwise Sorensen index
combs <- t(combn(x = unique(simulations$cal), m = 2))
sor <- apply(X = combs, MARGIN = 1, function(i){
  sim1 <- simulations[simulations$cal == i[1],]
  sim2 <- simulations[simulations$cal == i[2],]
  sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
  sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
    (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
  return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", "only.inverse")))
})
sorensen_divergence_historical <- data.frame(gcm = "ERA5-LAND", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])

saveRDS(sorensen_divergence_historical, file = file.path(wd, "data", "metrics", "sorensen_divergence_historical.rds"))

ggplot() +
  geom_boxplot(data = sorensen_divergence_paleo[sorensen_divergence_paleo$cat == "with.expert",], 
               aes(x = 1, y = 1-sorensen), width = 1, fill = "#40b8ab", color = "#40b8ab", alpha = 0.5) +
  geom_boxplot(data = sorensen_divergence_paleo[sorensen_divergence_paleo$cat == "only.inverse",], 
               aes(x = 2, y = 1-sorensen), width = 1, fill = "#0c86c8", color = "#0c86c8", alpha = 0.5) +
  geom_boxplot(data = sorensen_divergence_historical[sorensen_divergence_historical$cat == "with.expert",], 
               aes(x = 4, y = 1-sorensen), width = 1, fill = "#40b8ab", color = "#40b8ab", alpha = 0.5) +
  geom_boxplot(data = sorensen_divergence_historical[sorensen_divergence_historical$cat == "only.inverse",], 
               aes(x = 5, y = 1-sorensen), width = 1, fill = "#0c86c8", color = "#0c86c8", alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1.5, 4.5), labels = c("Distant past\n(11500 BP)", "Historical\n(1970-2000)")) +
  theme(panel.grid.minor.x = element_blank(), axis.title.x = element_blank()) +  
  ylab("Divergence between predictions")
  
  







