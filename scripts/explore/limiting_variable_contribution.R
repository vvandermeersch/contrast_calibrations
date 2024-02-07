
species <- c("F. sylvatica", "A. alba", "Q. robur", "Q. ilex")
simulations <- c("D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch",
                 "D:/simulations/phenofit/present/expert/abies_alba/VVanderMeersch2",
                 "D:/simulations/phenofit/present/expert/quercus_robur/ADuputie_Chuine",
                 "D:/simulations/phenofit/present/expert/quercus_ilex/FTauc/ptype3")
thresholds <- c(0.162, 0.100, 0.701, 0.172)
occurrences <- c("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds",
                 "D:/species/processed/abies_alba/abies_alba_presabs.rds",
                 "D:/species/processed/quercus_robur/quercus_robur_presabs.rds",
                 "D:/species/processed/quercus_ilex/quercus_ilex_presabs.rds")

ctrb_species <- c()
for(s in 1:4){
  surv <- rast(read_mean_outputvalue(simulations[s], 
                                     years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
  fruitind <- rast(read_mean_outputvalue(simulations[c], 
                                         years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
  matind <- rast(read_mean_outputvalue(simulations[c],
                                       years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
  fitness <- rast(read_mean_outputvalue(simulations[c], 
                                        years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
  indices <- c(surv, fruitind, matind)
  presence <- rast(readRDS(occurrences[s]))
  presence[presence == 0] <- NA
  fitnessmask <- fitness
  fitnessmask[fitnessmask >= thresholds[s]] <- NA
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
      ctrb_all = surv*fruit+surv*mat+fruit*mat,
      ctrb_surv = fruit*mat/ctrb_all,
      ctrb_fruit = surv*mat/ctrb_all,
      ctrb_mat = fruit*surv/ctrb_all) %>%
    select(-c(ctrb_all)) %>%
    tidyr::pivot_longer(cols = starts_with("ctrb"), names_to = "index", values_to = "ctrb") %>%
    mutate(index = factor(index, levels = c("ctrb_mat", "ctrb_fruit", "ctrb_surv"))) %>%
    group_by(index) %>%
    summarise(mean_ctrb = mean(ctrb)) %>%
    mutate(species = species[s]))

}

ggplot(data = ctrb_species, aes(x = species, y = mean_ctrb, fill = index)) +
  geom_bar(stat="identity", color="black", width = 0.5, linewidth = 0.3) +
  scale_fill_manual(values = c("#56b4e9", "#56e98b", "#e98b56"),
                    labels = c("Maturation index", "Fruit index", "Survival")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution", fill = NULL)

