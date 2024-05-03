
# Similarity index between model predictions
calibrations <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))
fit_dir <- file.path(wd, "data", "fit")
species <- "fagus_sylvatica"

source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

# process-based predictions
if(reload_data_fig1){
  
  # Load simulations at 11500BP ----
  plan(multisession, workers = 10)
  simulations11500BP <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
    
    sim_dir <- file.path(wd, "data", "simulations", "paleo", "11500BP")
    output <- read_mean_outputvalue(file.path(sim_dir, species, "pbm", c), 
                                    years = c(-11514:-11485), model = "PHENOFIT", output_var = "Fitness")
    names(output) <- c("lat", "lon", "fitness")
    
    threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- c
    return(output)
    
  })))
  
  simulations11500BP_csdm <- as.data.frame(do.call(rbind, lapply(csdm, function(c){
    
    sim_dir <- file.path(wd, "data", "simulations", "paleo", "11500BP")
    output <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
    names(output) <- c("lat", "lon", "fitness")
    
    sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
    fitfile <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
    threshold <- fitfile$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- c
    return(output)
    
  })))
  simulations11500BP <- rbind(simulations11500BP, simulations11500BP_csdm)
  
  combs <- t(combn(x = unique(simulations11500BP$cal), m = 2))
  sor <- future_apply(X = combs, MARGIN = 1, function(i){
    sim1 <- simulations11500BP[simulations11500BP$cal == i[1],]
    sim2 <- simulations11500BP[simulations11500BP$cal == i[2],]
    sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
    sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
      (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
    return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", 
                                                     ifelse(xor(i[1] %in% csdm, i[2] %in% csdm), "with.csdm",
                                                            ifelse(i[1] %in% csdm & i[2] %in% csdm, "only.csdm",
                                                                   "only.inverse")))))
  })
  plan(sequential); gc()
  sorensen_divergence_paleo_11500BP <- data.frame(gcm = "HadCM3B_11500BP", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])
  
  # Load simulations at 5000BP ----
  plan(multisession, workers = 10)
  simulations5000BP <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
    
    sim_dir <- file.path(wd, "data", "simulations", "paleo", "5000BP")
    output <- read_mean_outputvalue(file.path(sim_dir, species, "pbm", c), 
                                    years = c(-5014:-4985), model = "PHENOFIT", output_var = "Fitness")
    names(output) <- c("lat", "lon", "fitness")
    
    threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- c
    return(output)
    
  })))
  simulations5000BP_csdm <- as.data.frame(do.call(rbind, lapply(csdm, function(c){
    
    sim_dir <- file.path(wd, "data", "simulations", "paleo", "5000BP")
    output <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
    names(output) <- c("lat", "lon", "fitness")
    
    sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
    fitfile <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
    threshold <- fitfile$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- c
    return(output)
    
  })))
  simulations5000BP <- rbind(simulations5000BP, simulations5000BP_csdm)

  combs <- t(combn(x = unique(simulations5000BP$cal), m = 2))
  sor <- future_apply(X = combs, MARGIN = 1, function(i){
    sim1 <- simulations5000BP[simulations5000BP$cal == i[1],]
    sim2 <- simulations5000BP[simulations5000BP$cal == i[2],]
    sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
    sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
      (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
    return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", 
                                                     ifelse(xor(i[1] %in% csdm, i[2] %in% csdm), "with.csdm",
                                                            ifelse(i[1] %in% csdm & i[2] %in% csdm, "only.csdm",
                                                                   "only.inverse")))))
  })
  plan(sequential); gc()
  sorensen_divergence_paleo_5000BP <- data.frame(gcm = "HadCM3B_5000BP", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])
  sorensen_divergence_paleo <- rbind(sorensen_divergence_paleo_5000BP, sorensen_divergence_paleo_11500BP)
  saveRDS(sorensen_divergence_paleo, file = file.path(wd, "figures", "data", "fig1", "sorensen_divergence_paleo.rds"))
  
  # Load historical simulations ----
  plan(multisession, workers = 10)
  simulations <- as.data.frame(do.call(rbind, future_lapply(calibrations, function(c){
    
    sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
    output <- read_mean_outputvalue(file.path(sim_dir, species,  "pbm", c), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
    names(output) <- c("lat", "lon", "fitness")
    
    threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- c
    return(output)
    
  })))
  simulations_csdm <- as.data.frame(do.call(rbind, lapply(csdm, function(c){
    
    sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
    fitfile <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
    output <- fitfile$europe_pred
    names(output) <- c("lat", "lon", "fitness")
    
    threshold <- fitfile$best_threshold
    
    output$fitness <- ifelse(output$fitness < threshold, 0, 1)
    output$cal <- c
    return(output)
  })))
  simulations <- rbind(simulations, simulations_csdm)
  
  combs <- t(combn(x = unique(simulations$cal), m = 2))
  sor <- future_apply(X = combs, MARGIN = 1, function(i){
    sim1 <- simulations[simulations$cal == i[1],]
    sim2 <- simulations[simulations$cal == i[2],]
    sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
    sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
      (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
    return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", 
                                                     ifelse(xor(i[1] %in% csdm, i[2] %in% csdm), "with.csdm",
                                                            ifelse(i[1] %in% csdm & i[2] %in% csdm, "only.csdm",
                                                                   "only.inverse")))))
  })
  plan(sequential); gc()
  sorensen_divergence_historical <- data.frame(gcm = "ERA5-LAND", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])
  saveRDS(sorensen_divergence_historical, file = file.path(wd, "figures", "data", "fig1", "sorensen_divergence_historical.rds"))
  
}else{
  sorensen_divergence_paleo <- readRDS(file.path(wd, "figures", "data", "fig1", "sorensen_divergence_paleo.rds"))
  sorensen_divergence_historical <- readRDS(file = file.path(wd, "figures", "data", "fig1", "sorensen_divergence_historical.rds"))
}

# correlative model predictions
# if(reload_data_fig1){
#   
#   csdm <- c("brt", "gam", "lasso_glm", "maxent", "random_forest")
#   
#   # historical
#   simulations <- as.data.frame(do.call(rbind, lapply(csdm, function(c){
#     
#     sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
#     fitfile <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
#     output <- fitfile$europe_pred
#     names(output) <- c("lat", "lon", "fitness")
#     
#     threshold <- fitfile$best_threshold
#     
#     output$fitness <- ifelse(output$fitness < threshold, 0, 1)
#     output$cal <- c
#     return(output)
#   })))
#   combs <- t(combn(x = unique(simulations$cal), m = 2))
#   sor <- apply(X = combs, MARGIN = 1, function(i){
#     sim1 <- simulations[simulations$cal == i[1],]
#     sim2 <- simulations[simulations$cal == i[2],]
#     sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
#     sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
#       (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
#     return(c(sorensen, paste0(i[1],".",i[2]), "only.csdm"))
#   })
#   sorensen_divergence_historical_csdm <- data.frame(gcm = "ERA5-LAND", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])
#   saveRDS(sorensen_divergence_historical_csdm, file = file.path(wd, "figures", "data", "fig1", "sorensen_divergence_historical_csdm.rds"))
#   
#   # 11500BP
#   simulations11500BP <- as.data.frame(do.call(rbind, lapply(csdm, function(c){
#     
#     sim_dir <- file.path(wd, "data", "simulations", "paleo", "11500BP")
#     output <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
#     names(output) <- c("lat", "lon", "fitness")
#     
#     sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
#     fitfile <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
#     threshold <- fitfile$best_threshold
#     
#     output$fitness <- ifelse(output$fitness < threshold, 0, 1)
#     output$cal <- c
#     return(output)
#     
#   })))
#   combs <- t(combn(x = unique(simulations11500BP$cal), m = 2))
#   sor <- apply(X = combs, MARGIN = 1, function(i){
#     sim1 <- simulations11500BP[simulations11500BP$cal == i[1],]
#     sim2 <- simulations11500BP[simulations11500BP$cal == i[2],]
#     sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
#     sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
#       (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
#     return(c(sorensen, paste0(i[1],".",i[2]), "only.csdm"))
#   })
#   sorensen_divergence_paleo_11500BP <- data.frame(gcm = "HadCM3B_11500BP", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])
#   
#   # 5000BP
#   simulations5000BP <- as.data.frame(do.call(rbind, lapply(csdm, function(c){
#     
#     sim_dir <- file.path(wd, "data", "simulations", "paleo", "5000BP")
#     output <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
#     names(output) <- c("lat", "lon", "fitness")
#     
#     sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
#     fitfile <- readRDS(file.path(sim_dir, species,  "csdm", paste0(c, ".rds")))
#     threshold <- fitfile$best_threshold
#     
#     output$fitness <- ifelse(output$fitness < threshold, 0, 1)
#     output$cal <- c
#     return(output)
#     
#   })))
#   combs <- t(combn(x = unique(simulations5000BP$cal), m = 2))
#   sor <- apply(X = combs, MARGIN = 1, function(i){
#     sim1 <- simulations5000BP[simulations5000BP$cal == i[1],]
#     sim2 <- simulations5000BP[simulations5000BP$cal == i[2],]
#     sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
#     sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
#       (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
#     return(c(sorensen, paste0(i[1],".",i[2]), "only.csdm"))
#   })
#   sorensen_divergence_paleo_5000BP <- data.frame(gcm = "HadCM3B_5000BP", sorensen = as.numeric(sor[1,]), comb = sor[2,], cat = sor[3,])
#   sorensen_divergence_paleo_csdm <- rbind(sorensen_divergence_paleo_5000BP, sorensen_divergence_paleo_11500BP)
#   saveRDS(sorensen_divergence_paleo_csdm, file = file.path(wd, "figures", "data", "fig1", "sorensen_divergence_paleo_csdm.rds"))
#   
# }else{
#   sorensen_divergence_paleo_csdm <- readRDS(file.path(wd, "figures", "data", "fig1", "sorensen_divergence_paleo_csdm.rds"))
#   sorensen_divergence_historical_csdm <- readRDS(file = file.path(wd, "figures", "data", "fig1", "sorensen_divergence_historical_csdm.rds"))
# }

boxplot_similarity <- ggplot() +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "with.expert" & gcm == "HadCM3B_11500BP"), 
               aes(x = 1, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.inverse" & gcm == "HadCM3B_11500BP"), 
               aes(x = 2, y = 1-sorensen, fill = cat, color = cat), width = 0.8,
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.csdm" & gcm == "HadCM3B_11500BP"), 
               aes(x = 3, y = 1-sorensen, fill = cat, color = cat), width = 0.8,
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "with.expert" & gcm == "HadCM3B_5000BP"), 
               aes(x = 5, y = 1-sorensen, fill = cat, color = cat), width = 0.8,  
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.inverse" & gcm == "HadCM3B_5000BP"), 
               aes(x = 6, y = 1-sorensen, fill = cat, color = cat), width = 0.8,  
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.csdm" & gcm == "HadCM3B_5000BP"), 
               aes(x = 7, y = 1-sorensen, fill = cat, color = cat), width = 0.8,
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_historical %>% dplyr::filter(cat == "with.expert"), 
               aes(x = 9, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_historical %>% dplyr::filter(cat == "only.inverse"), 
               aes(x = 10, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_historical %>% dplyr::filter(cat == "only.csdm"), 
               aes(x = 11, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2, 6, 10), 
                     labels = c("Distant past\n(11500 BP)", "Distant past\n(5000 BP)", "Historical\n(1970-2000)")) +
  scale_y_continuous(limits = c(0, 0.6), expand = c(0,0), breaks = c(0,0.2,0.4,0.6), minor_breaks= c(0.1, 0.3, 0.5), guide = "axis_minor") +
  theme(panel.grid.minor.x = element_blank(), axis.title.x = element_blank()) +  
  ylab("Divergence between predictions") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "top", plot.margin = margin(b = 5.5, r = 2.5, l = 4.5, t = 2),
        legend.box.spacing = unit(0, "pt"),
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2, linewidth = 0.2), nrow = 3, byrow=TRUE),
         color = guide_legend(override.aes = list(alpha = 0.2, linewidth = 0.2), nrow = 3, byrow=TRUE)) + 
  scale_fill_manual(
    values = c("#6d90be", "#90be6d", "#be6d90"),
    breaks = c("only.inverse", "with.expert", "only.csdm"),
    labels = c("Within inverse calibrations", "Between expert and inverse calib.", "Within correlative models"), aesthetics = c("colour", "fill")
  )


boxplot_similarity <- ggplot() +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "with.expert" & gcm == "HadCM3B_11500BP"), 
               aes(x = 1, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.inverse" & gcm == "HadCM3B_11500BP"), 
               aes(x = 2, y = 1-sorensen, fill = cat, color = cat), width = 0.8,
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.csdm" & gcm == "HadCM3B_11500BP"), 
               aes(x = 3, y = 1-sorensen, fill = cat, color = cat), width = 0.8,
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "with.expert" & gcm == "HadCM3B_5000BP"), 
               aes(x = 5, y = 1-sorensen, fill = cat, color = cat), width = 0.8,  
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.inverse" & gcm == "HadCM3B_5000BP"), 
               aes(x = 6, y = 1-sorensen, fill = cat, color = cat), width = 0.8,  
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_paleo %>% dplyr::filter(cat == "only.csdm" & gcm == "HadCM3B_5000BP"), 
               aes(x = 7, y = 1-sorensen, fill = cat, color = cat), width = 0.8,
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_historical %>% dplyr::filter(cat == "with.expert"), 
               aes(x = 9, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_historical %>% dplyr::filter(cat == "only.inverse"), 
               aes(x = 10, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  geom_boxplot(data = sorensen_divergence_historical %>% dplyr::filter(cat == "only.csdm"), 
               aes(x = 11, y = 1-sorensen, fill = cat, color = cat), width = 0.8, 
               alpha = 0.6,outlier.size = 0.1, linewidth = 0.4) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2, 6, 10), 
                     labels = c("Distant past\n(11500 BP)", "Distant past\n(5000 BP)", "Historical\n(1970-2000)")) +
  scale_y_continuous(limits = c(0, 0.6), expand = c(0,0), breaks = c(0,0.2,0.4,0.6), minor_breaks= c(0.1, 0.3, 0.5), guide = "axis_minor") +
  theme(panel.grid.minor.x = element_blank(), axis.title.x = element_blank()) +  
  ylab("Divergence between predictions") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "top", plot.margin = margin(b = 5.5, r = 2.5, l = 4.5, t = 2),
        legend.box.spacing = unit(0, "pt"),
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2, linewidth = 0.2), nrow = 3, byrow=TRUE),
         color = guide_legend(override.aes = list(alpha = 0.2, linewidth = 0.2), nrow = 3, byrow=TRUE)) + 
  scale_fill_manual(
    values = c("#6d90be", "#90be6d", "#be6d90"),
    breaks = c("only.inverse", "with.expert", "only.csdm"),
    labels = c("Within inverse calibrations", "Between expert and inverse calib.", "Within correlative models"), aesthetics = c("colour", "fill")
  )

