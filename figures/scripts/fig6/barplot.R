
species <- c("Abies\nalba", "Fagus\nsylvatica", "Quercus\nrobur", "Betula\npendula", 
             "Picea\nabies", "Quercus\npubescens", "Fraxinus\nexcelsior", "Larix\ndecidua")
fit_dir <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations/data/fit/ERA5-LAND"
occurrences <- c("D:/species/processed/abies_alba/abies_alba_presabs.rds",
                 "D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds",
                 "D:/species/processed/quercus_robur/quercus_robur_presabs.rds",
                 "D:/species/processed/betula_pendula/betula_pendula_presabs.rds",
                 "D:/species/processed/picea_abies/picea_abies_presabs.rds",
                 "D:/species/processed/quercus_pubescens/quercus_pubescens_presabs.rds",
                 "D:/species/processed/fraxinus_excelsior/fraxinus_excelsior_presabs.rds",
                 "D:/species/processed/larix_decidua/larix_decidua_presabs.rds")

sim_exp <- c("D:/simulations/phenofit/present/expert/abies_alba/VVanderMeersch2",
             "D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch",
             "D:/simulations/phenofit/present/expert/quercus_robur/ADuputie_Chuine",
             "D:/simulations/phenofit/present/expert/betula_pendula/DAsse",
             "D:/simulations/phenofit/present/expert/picea_abies/Chuine2",
             "D:/simulations/phenofit/present/expert/quercus_pubescens/BLeys",
             "D:/simulations/phenofit/present/expert/fraxinus_excelsior/DAsse",
             "D:/simulations/phenofit/present/expert/larix_decidua/DAsse" )


fit_exp <- c(file.path(fit_dir, "abies_alba", "expert.rds"),
             file.path(fit_dir, "fagus_sylvatica", "expert.rds"),
             file.path(fit_dir, "quercus_robur", "expert.rds"),
             file.path(fit_dir, "betula_pendula", "expert.rds"),
             file.path(fit_dir, "picea_abies", "expert.rds"),
             file.path(fit_dir, "quercus_pubescens", "expert.rds"),
             file.path(fit_dir, "fraxinus_excelsior", "expert.rds"),
             file.path(fit_dir, "larix_decidua", "expert.rds"))

sim_part <- c("D:/simulations/phenofit/present/fitted/abies_alba/partial/drought_frost",
              "D:/simulations/phenofit/present/fitted/fagus_sylvatica/partial/relmax",
              "D:/simulations/phenofit/present/fitted/quercus_robur/partial",
              "D:/simulations/phenofit/present/fitted/betula_pendula/partial/expfrost_flo_mat",
              "D:/simulations/phenofit/present/fitted/picea_abies/partial/dgt_flo_mat",
              "D:/simulations/phenofit/present/fitted/quercus_pubescens/partial",
              "D:/simulations/phenofit/present/fitted/fraxinus_excelsior/partial/frost_flo_mat",
              "D:/simulations/phenofit/present/fitted/larix_decidua/partial/dgt_flo_mat")

fit_part <- c(file.path(fit_dir, "abies_alba", "partial/drought_frost"),
              file.path(fit_dir, "fagus_sylvatica", "partial/relmax"),
              file.path(fit_dir, "quercus_robur", "partial"),
              file.path(fit_dir, "betula_pendula", "partial/expfrost_flo_mat"),
              file.path(fit_dir, "picea_abies", "partial/dgt_flo_mat"),
              file.path(fit_dir, "quercus_pubescens", "partial"),
              file.path(fit_dir, "fraxinus_excelsior", "partial/frost_flo_mat"),
              file.path(fit_dir, "larix_decidua", "partial/dgt_flo_mat"))

if(reload_data_fig6){
  
  ctrb_species <- c()
  for(s in 1:8){
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
    
    if(species[s] %in% c("")){
      cal <- paste0("subset",rep(2, each = 5),"_rep", 1:5)
    }else if(species[s] %in% c("")){
      cal <- NULL
    }else{
      cal <- paste0("subset",rep(1:2, each = 5),"_rep", 1:5)
    }
    
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
    
  }
  
  saveRDS(ctrb_species, file = file.path(wd, "figures", "data", "fig6", "limiting_variables_partial.rds"))
}else{
  ctrb_species <- readRDS(file.path(wd, "figures", "data", "fig6", "limiting_variables_partial.rds"))
}

barplot_partial <- 
  ctrb_species %>%
  # dplyr::filter(species != "Fraxinus\nexcelsior") %>%
  group_by(type, index, species) %>%
  summarise(mean_ctrb = mean(mean_ctrb), 
            sd_auc = sd(auc),
            auc = mean(auc), 
            sd_prop = sd(prop_unpred),
            prop_unpred = mean(prop_unpred)) %>%
  # ggplot(aes(x = ifelse(type == "expert", paste0("\n", round(auc,2), "\n", round(prop_unpred*100,0),"%"),
  #                       paste0("\n", round(auc,2), " (", round(sd_auc,2), ")\n", round(prop_unpred*100,0),"% (", round(sd_prop,2), ")")), 
  #            y = mean_ctrb*100, pattern = index, fill = type)) +
  ggplot(aes(x = ifelse(type == "expert", paste0("\n", round(auc,2)), paste0("\n", round(auc,2), "\n(", round(sd_auc,2), ")")),
             y = mean_ctrb*100*prop_unpred, pattern = index, fill = type)) +
  facet_wrap(~ species, scales = "free_x", nrow = 1) +
  geom_bar_pattern(stat="identity", color="grey30", 
                   width = 0.5, linewidth = 0.2, alpha = 0.7,
                   pattern_fill = "grey30",
                   pattern_colour = "grey30",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.025,
                   pattern_size = 0.2,
                   pattern_key_scale_factor = 0.4) +
  scale_pattern_manual(
    name = "Main limiting driver: ",
    values = c("stripe", "circle", "none"),
    labels = c("Maturation ind.", "Fruit ind.", "Survival")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(title = NULL, override.aes = list(pattern = "none"))) +
  scale_fill_manual(values = c("#577590", "#f9c74f"), labels = c("Expert", "Partial")) + 
  theme_minimal() +
  labs(x = NULL, y = "Presences unpredicted (%)", fill = NULL) +
  coord_cartesian(clip = "off", ylim = c(0,100), xlim = c(0.9, 2.1)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_text(data = data.frame(species = "Abies\nalba", label = c("AUC ="),
                              y = c(-19), x = -0.05), 
            aes(x = x, y = y, label = label),
            size = 2.5, color = "grey30",
            inherit.aes = FALSE) +
  theme(axis.text.y = element_text(size = 8), axis.text.x = element_text(size = 7),
        legend.text = element_text(size = 8), legend.title = element_text(size = 8),
        legend.key.height = unit(0.3,"cm"), legend.key.width = unit(0.3,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "top", plot.margin = margin(b = 5.5, r = 5, l = 2.5, t = 23),
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        legend.box.spacing = unit(7.5, "pt"),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        strip.text = element_text(size = 8, color = "grey30"))
