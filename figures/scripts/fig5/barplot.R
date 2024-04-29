
# Fig 5.: partial calibrations
source(file.path(wd, "scripts", "functions", "read_species_file.R"))
species <- "fagus_sylvatica"

# Load limiting factors for each partial calibration
occurrences <- "D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds"
fit_dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations/data/fit/ERA5-LAND"

if(reload_data_fig5){
  ctrb_species <- c()
  ## Expert ----
  sim_exp <- "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch" 
  fit_exp <- file.path(fit_dir, "fagus_sylvatica", "expert.rds")
  surv <- rast(read_mean_outputvalue(sim_exp, 
                                     years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
  fruitind <- rast(read_mean_outputvalue(sim_exp, 
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
  matind <- rast(read_mean_outputvalue(sim_exp,
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
  fitness <- rast(read_mean_outputvalue(sim_exp, 
                                        years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
  indices <- c(surv, fruitind, matind)
  presence <- rast(readRDS(occurrences))
  presence[presence == 0] <- NA
  fitnessmask <- fitness
  threshold <- readRDS(fit_exp)$best_threshold
  fitnessmask[fitnessmask >= threshold] <- NA
  presence <- crop(presence, fitnessmask)
  fitnessmask <- crop(fitnessmask, presence)
  presencemask <- mask(presence, fitnessmask)
  indices <- mask(crop(indices, presencemask), presencemask)
  indices_df <- as.data.frame(indices)
  names(indices_df) <- c("surv", "fruit", "mat") 
  ctrb_species <- rbind(ctrb_species,
                        indices_df %>%
                          mutate(
                            surv = surv + 1e-8, fruit = fruit + 1e-8, mat = mat + 1e-8,
                            prop_unpred = ncell(presencemask[!is.na(presencemask)])/ncell(presence[!is.na(presence)]),
                            ctrb_all = surv*fruit+surv*mat+fruit*mat,
                            ctrb_surv = fruit*mat/ctrb_all,
                            ctrb_fruit = surv*mat/ctrb_all,
                            ctrb_mat = fruit*surv/ctrb_all) %>%
                          select(-c(ctrb_all)) %>%
                          tidyr::pivot_longer(cols = starts_with("ctrb"), names_to = "index", values_to = "ctrb") %>%
                          mutate(index = factor(index, levels = c("ctrb_mat", "ctrb_fruit", "ctrb_surv"))) %>%
                          group_by(index, prop_unpred) %>%
                          summarise(mean_ctrb = mean(ctrb), .groups = "drop") %>%
                          mutate(species = species, type = "0expert", rep = "expert",
                                 auc = round(readRDS(fit_exp)$auc_all,3)))
  
  
  
  
  ## Partial: FHmin and Maturation ----
  sim_part <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/partial"
  fit_part <- file.path(fit_dir, "fagus_sylvatica", "partial")
  cal <- paste0("subset",rep(1:2, each = 5),"_rep", 1:5)
  for(c in cal){
    cat(".")
    surv <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
    fruitind <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                           years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
    matind <- rast(read_mean_outputvalue(file.path(sim_part, c),
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
    fitness <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                          years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
    indices <- c(surv, fruitind, matind)
    presence <- rast(readRDS(occurrences))
    presence[presence == 0] <- NA
    fitnessmask <- fitness
    threshold <- readRDS(file.path(fit_part, paste0(c, ".rds")))$best_threshold
    fitnessmask[fitnessmask >= threshold] <- NA
    presence <- crop(presence, fitnessmask)
    fitnessmask <- crop(fitnessmask, presence)
    presencemask <- mask(presence, fitnessmask)
    indices <- mask(crop(indices, presencemask), presencemask)
    indices_df <- as.data.frame(indices)
    names(indices_df) <- c("surv", "fruit", "mat") 
    
    ctrb_species <- rbind(ctrb_species,
                          indices_df %>%
                            mutate(
                              surv = surv + 1e-8, fruit = fruit + 1e-8, mat = mat + 1e-8,
                              prop_unpred = ncell(presencemask[!is.na(presencemask)])/ncell(presence[!is.na(presence)]),
                              ctrb_all = surv*fruit+surv*mat+fruit*mat,
                              ctrb_surv = fruit*mat/ctrb_all,
                              ctrb_fruit = surv*mat/ctrb_all,
                              ctrb_mat = fruit*surv/ctrb_all) %>%
                            select(-c(ctrb_all)) %>%
                            tidyr::pivot_longer(cols = starts_with("ctrb"), names_to = "index", values_to = "ctrb") %>%
                            mutate(index = factor(index, levels = c("ctrb_mat", "ctrb_fruit", "ctrb_surv"))) %>%
                            group_by(index, prop_unpred) %>%
                            summarise(mean_ctrb = mean(ctrb), .groups = "drop") %>%
                            mutate(species = species, type = "1fhmin_mat", rep = c,
                                   auc = round(readRDS(file.path(fit_part, paste0(c, ".rds")))$auc_all,3)))
    
  }
  ## Partial: FHmin/max and Maturation ----
  sim_part <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/partial/relmax"
  fit_part <- file.path(fit_dir, "fagus_sylvatica", "partial", "relmax")
  cal <- paste0("subset",rep(1:1, each = 5),"_rep", 1:5)
  for(c in cal){
    cat(".")
    surv <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
    fruitind <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                           years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
    matind <- rast(read_mean_outputvalue(file.path(sim_part, c),
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
    fitness <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                          years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
    indices <- c(surv, fruitind, matind)
    presence <- rast(readRDS(occurrences))
    presence[presence == 0] <- NA
    fitnessmask <- fitness
    threshold <- readRDS(file.path(fit_part, paste0(c, ".rds")))$best_threshold
    fitnessmask[fitnessmask >= threshold] <- NA
    presence <- crop(presence, fitnessmask)
    fitnessmask <- crop(fitnessmask, presence)
    presencemask <- mask(presence, fitnessmask)
    indices <- mask(crop(indices, presencemask), presencemask)
    indices_df <- as.data.frame(indices)
    names(indices_df) <- c("surv", "fruit", "mat") 
    
    ctrb_species <- rbind(ctrb_species,
                          indices_df %>%
                            mutate(
                              surv = surv + 1e-8, fruit = fruit + 1e-8, mat = mat + 1e-8,
                              prop_unpred = ncell(presencemask[!is.na(presencemask)])/ncell(presence[!is.na(presence)]),
                              ctrb_all = surv*fruit+surv*mat+fruit*mat,
                              ctrb_surv = fruit*mat/ctrb_all,
                              ctrb_fruit = surv*mat/ctrb_all,
                              ctrb_mat = fruit*surv/ctrb_all) %>%
                            select(-c(ctrb_all)) %>%
                            tidyr::pivot_longer(cols = starts_with("ctrb"), names_to = "index", values_to = "ctrb") %>%
                            mutate(index = factor(index, levels = c("ctrb_mat", "ctrb_fruit", "ctrb_surv"))) %>%
                            group_by(index, prop_unpred) %>%
                            summarise(mean_ctrb = mean(ctrb), .groups = "drop") %>%
                            mutate(species = species, type = "2fh_mat", rep = c,
                                   auc = round(readRDS(file.path(fit_part, paste0(c, ".rds")))$auc_all,3)))
    
  }
  ## Partial: FHmin/max and leaf phenology ----
  sim_part <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/partial/pheno_fhmaxmin"
  fit_part <- file.path(fit_dir, "fagus_sylvatica", "partial", "pheno_fhmaxmin")
  cal <- paste0("subset",rep(1:1, each = 5),"_rep", 1:5)
  for(c in cal){
    cat(".")
    surv <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
    fruitind <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                           years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
    matind <- rast(read_mean_outputvalue(file.path(sim_part, c),
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
    fitness <- rast(read_mean_outputvalue(file.path(sim_part, c), 
                                          years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
    indices <- c(surv, fruitind, matind)
    presence <- rast(readRDS(occurrences))
    presence[presence == 0] <- NA
    fitnessmask <- fitness
    threshold <- readRDS(file.path(fit_part, paste0(c, ".rds")))$best_threshold
    fitnessmask[fitnessmask >= threshold] <- NA
    presence <- crop(presence, fitnessmask)
    fitnessmask <- crop(fitnessmask, presence)
    presencemask <- mask(presence, fitnessmask)
    indices <- mask(crop(indices, presencemask), presencemask)
    indices_df <- as.data.frame(indices)
    names(indices_df) <- c("surv", "fruit", "mat") 
    
    ctrb_species <- rbind(ctrb_species,
                          indices_df %>%
                            mutate(
                              surv = surv + 1e-8, fruit = fruit + 1e-8, mat = mat + 1e-8,
                              prop_unpred = ncell(presencemask[!is.na(presencemask)])/ncell(presence[!is.na(presence)]),
                              ctrb_all = surv*fruit+surv*mat+fruit*mat,
                              ctrb_surv = fruit*mat/ctrb_all,
                              ctrb_fruit = surv*mat/ctrb_all,
                              ctrb_mat = fruit*surv/ctrb_all) %>%
                            select(-c(ctrb_all)) %>%
                            tidyr::pivot_longer(cols = starts_with("ctrb"), names_to = "index", values_to = "ctrb") %>%
                            mutate(index = factor(index, levels = c("ctrb_mat", "ctrb_fruit", "ctrb_surv"))) %>%
                            group_by(index, prop_unpred) %>%
                            summarise(mean_ctrb = mean(ctrb), .groups = "drop") %>%
                            mutate(species = species, type = "3fh_pheno", rep = c,
                                   auc = round(readRDS(file.path(fit_part, paste0(c, ".rds")))$auc_all,3)))
    
  }
  
  saveRDS(ctrb_species, file = file.path(wd, "figures", "data", "fig5", "limiting_variables_fagus.rds"))
}else{
  ctrb_species <- readRDS(file.path(wd, "figures", "data", "fig5", "limiting_variables_fagus.rds"))
}

data_barplot <- ctrb_species %>%
  group_by(type, index) %>%
  summarise(mean_ctrb = mean(mean_ctrb), 
            sd_auc = sd(auc),
            auc = mean(auc), 
            sd_prop = sd(prop_unpred),
            prop_unpred = mean(prop_unpred)) %>%
  mutate(type = factor(type)) %>%
  mutate(type = recode(type, '0expert' = 'Expert\ncalibration', 
                       '1fhmin_mat' = 'Frost min.,\nMaturation',
                       '2fh_mat' = 'Frost,\nMaturation',
                       '3fh_pheno' = 'Frost,\nLeaf pheno.'))

xlabels <- unique(data_barplot[,c("type", "auc", "sd_auc", "prop_unpred", "sd_prop")])

barplot <- 
  ggplot(data = data_barplot, aes(x = type, 
             y = mean_ctrb*100, fill = type, pattern = index)) +
  geom_bar_pattern(stat="identity", color="black", 
                   width = 0.5, linewidth = 0.2, alpha = 0.7,
                   pattern_fill = "black",
                   pattern_colour = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.025,
                   pattern_size = 0.2,
                   pattern_key_scale_factor = 0.4) +
  scale_pattern_manual(values = c("stripe", "circle", "none"),
                       labels = c("Maturation ind.", "Fruit ind.", "Survival")) +
  guides(pattern = guide_legend(title = NULL, override.aes = list(fill = "white")),
         fill = "none") +
  scale_x_discrete(labels = ifelse(xlabels$type == "Expert\ncalibration", paste0(xlabels$type,"\n\n", round(xlabels$auc,2), "\n", round(xlabels$prop_unpred*100,1),"%"),
                                   paste0(xlabels$type,"\n\n", round(xlabels$auc,2), " (", round(xlabels$sd_auc,2), ")\n", round(xlabels$prop_unpred*100,1),"% (", round(xlabels$sd_prop,2), ")"))) +
  scale_fill_manual(values = c("grey", "#f9c74f", "#90be6d", "#577590")) +
  scale_color_manual(values = c("grey", "#f9c74f", "#90be6d", "#577590")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution (%)", fill = NULL) +
  coord_cartesian(clip = "off", ylim = c(0,100), xlim = c(0.8, 4.2)) +
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", label = "AUC = ", y = -23, x = 0.2, size = 2.5,
           color = "grey30") +
  annotate(geom = "text", label = "% unpred. = ", y = -29, x = 0.2, size = 2.5,
           color = "grey30") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_text(size = 7),
        legend.text = element_text(size = 7),
        legend.key.height = unit(0.3,"cm"), legend.key.width = unit(0.3,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "bottom", plot.margin = margin(b = 5.5, r = 5, l = 2.5, t = 23),
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        legend.box.spacing = unit(7.5, "pt"),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))
