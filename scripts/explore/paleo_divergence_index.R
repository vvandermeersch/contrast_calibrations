
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

# Load simulations at 11500BP
plan(multisession, workers = 20)
simulations11500BP <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
  
  sim_dir <- file.path(wd, "data", "simulations", "paleo", "11500BP")
  output <- read_mean_outputvalue(file.path(sim_dir, species, c), 
                                  years = c(-11514:-11485), model = "PHENOFIT", output_var = "Fitness")
  names(output) <- c("lat", "lon", "fitness")
  
  threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
  
  output$fitness <- ifelse(output$fitness < threshold, 0, 1)
  output$cal <- c
  return(output)
    
})))
plan(sequential); gc()

# Compute pairwise Sorensen index at 11500BP
combs <- t(combn(x = unique(simulations11500BP$cal), m = 2))
sor <- apply(X = combs, MARGIN = 1, function(i){
  sim1 <- simulations11500BP[simulations11500BP$cal == i[1],]
  sim2 <- simulations11500BP[simulations11500BP$cal == i[2],]
  sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
  sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
    (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
  return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", "only.inverse")))
})
sorensen_divergence_paleo_11500BP <- data.frame(gcm = "HadCM3B_11500BP", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])

# Load simulations at 7000BP
plan(multisession, workers = 20)
simulations7000BP <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
  
  sim_dir <- file.path(wd, "data", "simulations", "paleo", "7000BP")
  output <- read_mean_outputvalue(file.path(sim_dir, species, c), 
                                  years = c(-7014:-6985), model = "PHENOFIT", output_var = "Fitness")
  names(output) <- c("lat", "lon", "fitness")
  
  threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
  
  output$fitness <- ifelse(output$fitness < threshold, 0, 1)
  output$cal <- c
  return(output)
  
})))
plan(sequential); gc()

# Compute pairwise Sorensen index at 7000BP
combs <- t(combn(x = unique(simulations7000BP$cal), m = 2))
sor <- apply(X = combs, MARGIN = 1, function(i){
  sim1 <- simulations7000BP[simulations7000BP$cal == i[1],]
  sim2 <- simulations7000BP[simulations7000BP$cal == i[2],]
  sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
  sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
    (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
  return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", "only.inverse")))
})
sorensen_divergence_paleo_7000BP <- data.frame(gcm = "HadCM3B_7000BP", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])


sorensen_divergence_paleo <- rbind(sorensen_divergence_paleo_7000BP, sorensen_divergence_paleo_11500BP)
saveRDS(sorensen_divergence_paleo, file = file.path(wd, "data", "metrics", "sorensen_divergence_paleo_11500BP.rds"))


ggplot() +
  geom_boxplot(data = sorensen_divergence_paleo[sorensen_divergence_paleo$cat == "with.expert",], 
               aes(x = gcm, y = 1-sorensen), width = 5, fill = "#40b8ab", color = "#40b8ab", alpha = 0.5) +
  geom_boxplot(data = sorensen_divergence_paleo[sorensen_divergence_paleo$cat == "only.inverse",], 
               aes(x = gcm, y = 1-sorensen), width = 5, fill = "#0c86c8", color = "#0c86c8", alpha = 0.5)







