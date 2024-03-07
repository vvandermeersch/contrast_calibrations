
# Compute divergence between future predictions across different calibrations (in terms of Sorensen index)
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

gcms <- c("GFDL-ESM4", "MPI-ESM1-2-HR", "UKESM1-0-LL")
calibrations <- c("expert", "subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7")
fit_dir <- file.path(wd, "data", "fit")

# Load simulations
plan(multisession, workers = 5)
simulations <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
  sim <- as.data.frame(do.call(rbind, future_lapply(1:length(calibrations), function(i){
    
    sim_dir <- file.path(wd, "data", "simulations", "historical", m)
    output <- read_mean_outputvalue(file.path(sim_dir, calibrations[i]), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
    names(output) <- c("lat", "lon", "fitness")
    
    threshold <- readRDS(file.path(fit_dir, m, paste0(calibrations[i], ".rds")))$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- calibrations[i]
    return(output)
  })))
  sim$gcm <- m
  return(sim)
})))
plan(sequential); gc()

# Compute pairwise Sorensen index
sorensen_divergence_historical <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
  sim <- simulations[simulations$gcm == m, ]
  combs <- t(combn(x = unique(sim$cal), m = 2))
  sor <- apply(X = combs, MARGIN = 1, function(i){
    sim1 <- sim[sim$cal == i[1],]
    sim2 <- sim[sim$cal == i[2],]
    sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
    sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
      (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
    return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", "only.inverse")))
  })
  return(data.frame(gcm = m, sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,]))
})))

saveRDS(sorensen_divergence_historical, file = file.path(wd, "data", "metrics", "sorensen_divergence_historical.rds"))

ggplot() +
  geom_boxplot(data = sorensen_divergence_historical[sorensen_divergence_historical$cat == "with.expert",], 
               aes(x = 2017, y = 1-sorensen), width = 5, fill = "#40b8ab", color = "#40b8ab", alpha = 0.5) +
  geom_boxplot(data = sorensen_divergence_historical[sorensen_divergence_historical$cat == "only.inverse",], 
               aes(x = 2020, y = 1-sorensen), width = 5, fill = "#0c86c8", color = "#0c86c8", alpha = 0.5)







