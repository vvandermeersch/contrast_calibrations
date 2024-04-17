# Partial calibrations
species <- "fagus_sylvatica"

# Limiting factors
occurrences <- "D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds"
fit_dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations/data/fit/ERA5-LAND"
ctrb_species <- c()
{
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
}

var_contrib <- 
  ctrb_species %>%
  group_by(type, index) %>%
  summarise(mean_ctrb = mean(mean_ctrb), 
            sd_auc = sd(auc),
            auc = mean(auc), 
            sd_prop = sd(prop_unpred),
            prop_unpred = mean(prop_unpred)) %>%
  ggplot(aes(x = ifelse(type == "0expert", paste0(type,"\n\n", round(auc,2), "\n", round(prop_unpred*100,1),"%"),
                        paste0(type,"\n\n", round(auc,2), " (", round(sd_auc,2), ")\n", round(prop_unpred*100,1),"% (", round(sd_prop,2), ")")), 
             y = mean_ctrb*100, fill = index)) +
  geom_bar(stat="identity", color="black", width = 0.5, linewidth = 0.3, alpha = 0.8) +
  scale_fill_manual(values = c("#577590", "#90be6d", "#f9c74f"),
                    labels = c("Maturation ind.", "Fruit ind.", "Survival")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution (%)", fill = NULL) +
  coord_cartesian(clip = "off", ylim = c(0,100)) +
  annotate(geom = "text", label = "AUC = ", y = -21, x = 0, size = 2.5,
           color = "grey30") +
  annotate(geom = "text", label = "% unpred. = ", y = -26, x = 0, size = 2.5,
           color = "grey30") +
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8),
        legend.key.height = unit(0.4,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank())

var_contrib <- 
  ctrb_species %>%
  group_by(type, index) %>%
  summarise(mean_ctrb = mean(mean_ctrb), 
            sd_auc = sd(auc),
            auc = mean(auc), 
            sd_prop = sd(prop_unpred),
            prop_unpred = mean(prop_unpred)) %>%
  ggplot(aes(x = ifelse(type == "0expert", paste0(type,"\n\n", round(auc,2), "\n", round(prop_unpred*100,1),"%"),
                        paste0(type,"\n\n", round(auc,2), " (", round(sd_auc,2), ")\n", round(prop_unpred*100,1),"% (", round(sd_prop,2), ")")), 
             y = mean_ctrb*100, fill = type, pattern = index)) +
  geom_bar_pattern(stat="identity", color="black", 
                   width = 0.5, linewidth = 0.2, alpha = 0.8,
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
  
  scale_fill_manual(values = c("#ff595e", "#ffca3a", "#8ac926", "#1982c4")) +
  scale_color_manual(values = c("#ff595e", "#ffca3a", "#8ac926", "#1982c4")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution (%)", fill = NULL) +
  coord_cartesian(clip = "off", ylim = c(0,100)) +
  scale_y_continuous(expand = c(0,0)) +
  annotate(geom = "text", label = "AUC = ", y = -17, x = 0.5, size = 2.5,
           color = "grey30") +
  annotate(geom = "text", label = "% unpred. = ", y = -23, x = 0.5, size = 2.5,
           color = "grey30") +
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8),
        legend.key.height = unit(0.4,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title = element_text(size = 10), panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey", linewidth = 0.3),
        legend.position = "bottom", plot.margin = margin(b = 5.5, r = 10, l = 5.5, t = 20),
        panel.background = element_rect(color = "grey", fill = NA, linewidth = 0.3))
