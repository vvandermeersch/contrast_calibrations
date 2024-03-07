
occurrences <- readRDS("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds")

plan(multisession, workers = 20)
contributions <- as.data.frame(do.call(rbind, future_lapply(yr_windows, function(yr){
  cat(paste0((yr[1]+yr[2])/2, "...\n"))
  
  ctrb <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    ctrb <- as.data.frame(do.call(rbind, lapply(scenarios, function(s){
      ctrb <- as.data.frame(do.call(rbind, lapply(1:length(calibrations), function(i){
        sim_dir <- file.path(wd, "data", "simulations", "future", m, s)
        output <- read_mean_outputvalue(file.path(sim_dir, calibrations[i]), 
                                        years = c(yr[1]:yr[2]), model = "PHENOFIT", output_var = "Fitness")
        
        surv <- rast(read_mean_outputvalue(file.path(sim_dir, calibrations[i]), years = c(yr[1]:yr[2]), 
                                           model = "PHENOFIT", output_var = "Survival")[,c(2,1,3)])
        fruitind <- rast(read_mean_outputvalue(file.path(sim_dir, calibrations[i]), years = c(yr[1]:yr[2]), 
                                               model = "PHENOFIT", output_var = "FruitIndex")[,c(2,1,3)])
        matind <- rast(read_mean_outputvalue(file.path(sim_dir, calibrations[i]), years = c(yr[1]:yr[2]), 
                                             model = "PHENOFIT", output_var = "MaturationIndex")[,c(2,1,3)])
        fitness <- rast(read_mean_outputvalue(file.path(sim_dir, calibrations[i]), years = c(yr[1]:yr[2]), 
                                              model = "PHENOFIT", output_var = "Fitness")[,c(2,1,3)])
        indices <- c(surv, fruitind, matind)
        presence <- rast(occurrences)
        presence[presence == 0] <- NA
        fitnessmask <- fitness
        fitnessmask[fitnessmask >= thresholds[i]] <- NA
        presence <- crop(presence, fitnessmask)
        fitnessmask <- crop(fitnessmask, presence)
        presencemask <- mask(presence, fitnessmask)
        indices <- mask(crop(indices, presencemask), presencemask)
        indices_df <- as.data.frame(indices)
        names(indices_df) <- c("surv", "fruit", "mat")
        
        ctrb <- indices_df %>%
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
          summarise(mean_ctrb = mean(ctrb), ncells = n()) %>%
          mutate(cal = calibrations[i], gcm = m, ssp = s, year = (yr[1]+yr[2])/2)

        return(ctrb)
      })))
      return(ctrb)
    })))
    return(ctrb)
  })))
  return(ctrb)
})))
plan(sequential);gc()

contributions %>%
  group_by(cal, ssp, index) %>%
  summarise(mean_ctrb = mean(mean_ctrb)) %>%
  ggplot(aes(x = cal, y = mean_ctrb, fill = index)) +
  facet_wrap(~ssp) + 
  geom_bar(stat="identity", color="black", width = 0.6, linewidth = 0.3) +
  scale_fill_manual(values = c("#56b4e9", "#56e98b", "#e98b56"),
                    labels = c("Maturation index", "Fruit index", "Survival")) +
  theme_minimal() +
  labs(x = NULL, y = "Mean contribution", fill = NULL)



