
species <- c("A. alba", "F. sylv.", "Q. rob.", "B. pend.", "P. abies", "Q. pub.")
fit_dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations/data/fit/ERA5-LAND"
occurrences <- c("D:/species/processed/abies_alba/abies_alba_presabs.rds",
                 "D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds",
                 "D:/species/processed/quercus_robur/quercus_robur_presabs.rds",
                 "D:/species/processed/betula_pendula/betula_pendula_presabs.rds",
                 "D:/species/processed/picea_abies/picea_abies_presabs.rds",
                 "D:/species/processed/quercus_pubescens/quercus_pubescens_presabs.rds")

sim_exp <- c("D:/simulations/phenofit/present/expert/abies_alba/VVanderMeersch2",
             "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch",
             "D:/simulations/phenofit/present/expert/quercus_robur/ADuputie_Chuine",
             "D:/simulations/phenofit/present/expert/betula_pendula/DAsse",
             "D:/simulations/phenofit/present/expert/picea_abies/Chuine2",
             "D:/simulations/phenofit/present/expert/quercus_pubescens/BLeys")


fit_exp <- c(file.path(fit_dir, "abies_alba", "expert.rds"),
             file.path(fit_dir, "fagus_sylvatica", "expert.rds"),
             file.path(fit_dir, "quercus_robur", "expert.rds"),
             file.path(fit_dir, "betula_pendula", "expert.rds"),
             file.path(fit_dir, "picea_abies", "expert.rds"),
             file.path(fit_dir, "quercus_pubescens", "expert.rds"))

sim_part <- c("D:/simulations/phenofit/present/fitted/abies_alba/partial/drought_and_frost",
              "D:/simulations/phenofit/present/fitted/fagus_sylvatica/partial/relmax",
             "D:/simulations/phenofit/present/fitted/quercus_robur/partial",
             "D:/simulations/phenofit/present/fitted/betula_pendula/partial/frost_mat",
             "D:/simulations/phenofit/present/fitted/picea_abies/partial/dgt_flower",
             "D:/simulations/phenofit/present/fitted/quercus_pubescens/partial")

fit_part <- c(file.path(fit_dir, "abies_alba", "partial/drought_and_frost"),
              file.path(fit_dir, "fagus_sylvatica", "partial/relmax"),
              file.path(fit_dir, "quercus_robur", "partial"),
              file.path(fit_dir, "betula_pendula", "partial/frost_mat"),
              file.path(fit_dir, "picea_abies", "partial/dgt_flower"),
              file.path(fit_dir, "quercus_pubescens", "partial"))




ctrb_species <- c()
for(s in 1:6){
  cat(paste0("\nProcessing ",  species[s], " ...\n"))
  
  # Expert
  cat("   - expert calibration\n")
  surv <- rast(read_mean_outputvalue(file.path(sim_exp[s]), 
                                     years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
  fruitind <- rast(read_mean_outputvalue(file.path(sim_exp[s]), 
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
  matind <- rast(read_mean_outputvalue(file.path(sim_exp[s]),
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
  fitness <- rast(read_mean_outputvalue(file.path(sim_exp[s]), 
                                        years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
  indices <- c(surv, fruitind, matind)
  presence <- rast(readRDS(occurrences[s]))
  presence[presence == 0] <- NA
  fitnessmask <- fitness
  threshold <- readRDS(file.path(fit_exp[s]))$best_threshold
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
                          mutate(species = species[s], type = "expert", rep = "expert",
                                 auc = round(readRDS(file.path(fit_exp[s]))$auc_all,3)))
  
  # Partial
  cat("   - partial calibration")
  cal <- paste0("subset",rep(1:1, each = 5),"_rep", 1:5)
  for(c in cal){
    cat(".")
    surv <- rast(read_mean_outputvalue(file.path(sim_part[s], c), 
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
    fruitind <- rast(read_mean_outputvalue(file.path(sim_part[s], c), 
                                           years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
    matind <- rast(read_mean_outputvalue(file.path(sim_part[s], c),
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
    fitness <- rast(read_mean_outputvalue(file.path(sim_part[s], c), 
                                          years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
    indices <- c(surv, fruitind, matind)
    presence <- rast(readRDS(occurrences[s]))
    presence[presence == 0] <- NA
    fitnessmask <- fitness
    threshold <- readRDS(file.path(fit_part[s], paste0(c, ".rds")))$best_threshold
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
                            mutate(species = species[s], type = "partial", rep = c,
                                   auc = round(readRDS(file.path(fit_part[s], paste0(c, ".rds")))$auc_all,3)))
    
  }
  
  # Partial relmax
  # cat("   - partial calibration")
  # cal <- paste0("subset",rep(1:1, each = 5),"_rep", 1:5)
  # for(c in cal){
  #   cat(".")
  #   surv <- rast(read_mean_outputvalue(file.path(sim_part[s], "relmax", c), 
  #                                      years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
  #   
  #   fruitind <- rast(read_mean_outputvalue(file.path(sim_part[s], "relmax", c), 
  #                                          years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
  #   matind <- rast(read_mean_outputvalue(file.path(sim_part[s], "relmax", c),
  #                                        years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
  #   fitness <- rast(read_mean_outputvalue(file.path(sim_part[s], "relmax", c), 
  #                                         years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
  #   indices <- c(surv, fruitind, matind)
  #   presence <- rast(readRDS(occurrences[s]))
  #   presence[presence == 0] <- NA
  #   fitnessmask <- fitness
  #   threshold <- readRDS(file.path(fit_part[s], "relmax", paste0(c, ".rds")))$best_threshold
  #   fitnessmask[fitnessmask >= threshold] <- NA
  #   presence <- crop(presence, fitnessmask)
  #   fitnessmask <- crop(fitnessmask, presence)
  #   presencemask <- mask(presence, fitnessmask)
  #   indices <- mask(crop(indices, presencemask), presencemask)
  #   indices_df <- as.data.frame(indices)
  #   names(indices_df) <- c("surv", "fruit", "mat") 
  #   
  #   ctrb_species <- rbind(ctrb_species,
  #                         indices_df %>%
  #                           mutate(
  #                             surv = surv + 1e-8, fruit = fruit + 1e-8, mat = mat + 1e-8,
  #                             prop_unpred = ncell(presencemask[!is.na(presencemask)])/ncell(presence[!is.na(presence)]),
  #                             ctrb_all = surv*fruit+surv*mat+fruit*mat,
  #                             ctrb_surv = fruit*mat/ctrb_all,
  #                             ctrb_fruit = surv*mat/ctrb_all,
  #                             ctrb_mat = fruit*surv/ctrb_all) %>%
  #                           select(-c(ctrb_all)) %>%
  #                           tidyr::pivot_longer(cols = starts_with("ctrb"), names_to = "index", values_to = "ctrb") %>%
  #                           mutate(index = factor(index, levels = c("ctrb_mat", "ctrb_fruit", "ctrb_surv"))) %>%
  #                           group_by(index, prop_unpred) %>%
  #                           summarise(mean_ctrb = mean(ctrb), .groups = "drop") %>%
  #                           mutate(species = species[s], type = "partial\n(rel. FHmax)", rep = c,
  #                                  auc = round(readRDS(file.path(fit_part[s], "relmax", paste0(c, ".rds")))$auc_all,3)))
    
  # }
  
}

var_contrib <- 
  ctrb_species %>%
  group_by(type, index, species) %>%
  summarise(mean_ctrb = mean(mean_ctrb), 
            sd_auc = sd(auc),
            auc = mean(auc), 
            sd_prop = sd(prop_unpred),
            prop_unpred = mean(prop_unpred)) %>%
  ggplot(aes(x = ifelse(type == "expert", paste0(type,"\n\n", round(auc,2), "\n", round(prop_unpred*100,1),"%"),
                        paste0(type,"\n\n", round(auc,2), " (", round(sd_auc,2), ")\n", round(prop_unpred*100,1),"% (", round(sd_prop,2), ")")), 
             y = mean_ctrb*100, fill = index)) +
  facet_wrap(~ species, scales = "free_x") +
  geom_bar(stat="identity", color="black", width = 0.5, linewidth = 0.3, alpha = 0.8) +
  scale_fill_manual(values = c("#577590", "#90be6d", "#f9c74f"),
                    labels = c("Maturation ind.", "Fruit ind.", "Survival")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution (%)", fill = NULL) +
  coord_cartesian(clip = "off", ylim = c(0,100),  expand = TRUE) +
  # annotate(geom = "text", label = "AUC = ", y = -21, x = 0, size = 2.5,
  #          color = "grey30") +
  # annotate(geom = "text", label = "% unpred. = ", y = -26, x = 0, size = 2.5,
  #          color = "grey30") +
  geom_text(data = data.frame(species = "A. alba", label = c("AUC =", "% unpred. ="), y = c(-21, -26)),
            aes(x = 0.4, y = y, label = label), inherit.aes = FALSE, size = 2.5, color = "grey30") +
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8),
        legend.key.height = unit(0.4,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank())


  




