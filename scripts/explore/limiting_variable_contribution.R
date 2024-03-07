
species <- c("F. sylv.", "A. alba", "Q. rob.", "Q. ilex", "Q. pet.", 
             "Q. pubes.", "B. pend.", "P. sylv.", "L. dec.", "P. abies")
simulations <- c("D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch",
                 "D:/simulations/phenofit/present/expert/abies_alba/VVanderMeersch2",
                 "D:/simulations/phenofit/present/expert/quercus_robur/ADuputie_Chuine",
                 "D:/simulations/phenofit/present/expert/quercus_ilex/FTauc/ptype3",
                 "D:/simulations/phenofit/present/expert/quercus_petraea/VanderMeersch2023_Chuine",
                 "D:/simulations/phenofit/present/expert/quercus_pubescens/BLeys",
                 "D:/simulations/phenofit/present/expert/betula_pendula/DAsse",
                 "D:/simulations/phenofit/present/expert/pinus_sylvestris/VVanderMeersch",
                 "D:/simulations/phenofit/present/expert/larix_decidua/DAsse",
                 "D:/simulations/phenofit/present/expert/picea_abies/Chuine2")

fit_dir <- "C:/Users/vandermeersch/Documents/CEFE/phd/phenofit/fit/expert"
fit_settings <- c(file.path(fit_dir, "fagus_sylvatica", "Fagus_sylvatica_VVanderMeersch.rds"),
                  file.path(fit_dir, "abies_alba", "Abies_alba_VVanderMeersch2.rds"),
                  file.path(fit_dir, "quercus_robur", "Quercus_robur_ADuputie_Chuine.rds"),
                  file.path(fit_dir, "quercus_ilex", "Quercus_ilex_FTauc_ptype3.rds"),
                  file.path(fit_dir, "quercus_petraea", "Quercus_petraea_VanderMeersch2023_Chuine.rds"),
                  file.path(fit_dir, "quercus_pubescens", "Quercus_pubescens_BLeys.rds"),
                  file.path(fit_dir, "betula_pendula", "Betula_pendula_DAsse.rds"),
                  file.path(fit_dir, "pinus_sylvestris", "VVanderMeersch.rds"),
                  file.path(fit_dir, "larix_decidua", "Larix_decidua_DAsse.rds"),
                  file.path(fit_dir, "picea_abies", "Chuine2.rds"))
  
occurrences <- c("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds",
                 "D:/species/processed/abies_alba/abies_alba_presabs.rds",
                 "D:/species/processed/quercus_robur/quercus_robur_presabs.rds",
                 "D:/species/processed/quercus_ilex/quercus_ilex_presabs.rds",
                 "D:/species/processed/quercus_petraea/quercus_petraea_presabs.rds",
                 "D:/species/processed/quercus_pubescens/quercus_pubescens_presabs.rds",
                 "D:/species/processed/betula_pendula/betula_pendula_presabs.rds",
                 "D:/species/processed/pinus_sylvestris/pinus_sylvestris_presabs.rds",
                 "D:/species/processed/larix_decidua/larix_decidua_presabs.rds",
                 "D:/species/processed/picea_abies/picea_abies_presabs.rds")

ctrb_species <- c()
for(s in 1:10){
  print(s)
  surv <- rast(read_mean_outputvalue(simulations[s], 
                                     years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
  fruitind <- rast(read_mean_outputvalue(simulations[s], 
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
  matind <- rast(read_mean_outputvalue(simulations[s],
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
  fitness <- rast(read_mean_outputvalue(simulations[s], 
                                        years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
  indices <- c(surv, fruitind, matind)
  presence <- rast(readRDS(occurrences[s]))
  presence[presence == 0] <- NA
  fitnessmask <- fitness
  threshold <- readRDS(fit_settings[s])$best_threshold
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
    summarise(mean_ctrb = mean(ctrb)) %>%
    mutate(species = species[s]))

}

aucs <- data.frame(species = species,
                   auc = round(sapply(1:10, function(i){readRDS(fit_settings[i])$auc_all}),2))
 

var_contrib <- ggplot(data = ctrb_species %>% left_join(aucs), 
       aes(x = paste0(species, "\n\n", auc, "\n", round(prop_unpred*100,1),"%"), 
                      y = mean_ctrb*100, fill = index)) +
  geom_bar(stat="identity", color="black", width = 0.5, linewidth = 0.3, alpha = 0.8) +
  scale_fill_manual(values = c("#577590", "#90be6d", "#f9c74f"),
                    labels = c("Maturation ind.", "Fruit ind.", "Survival")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution (%)", fill = NULL) +
  coord_cartesian(clip = "off", ylim = c(0,100)) +
  annotate(geom = "text", label = "AUC = ", y = -28, x = 0, size = 2.5,
           color = "grey30") +
  annotate(geom = "text", label = "% unpred. = ", y = -36, x = 0, size = 2.5,
           color = "grey30") +
  theme(axis.text = element_text(size = 8), legend.text = element_text(size = 8),
        legend.key.height = unit(0.4,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title = element_text(size = 8), panel.grid.major.x = element_blank())

ggsave(var_contrib, filename = file.path(wd, "scripts", "explore", "graphs", "last", "var_contrib.pdf"),
       width = 190, height = 60, unit = "mm")



