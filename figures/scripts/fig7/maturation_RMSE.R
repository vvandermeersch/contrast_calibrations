
records <- all_records %>%
  dplyr::filter(stade %in% stades)

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")
#sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/partial/relmax"
#species <- "fagus_sylvatica"
#models <- c("expert",paste0("partial/relmaxsubset",rep(1:2, each = 5),"_rep", 1:5))

if(reload_data_fig7){
  
  plan(multisession, workers = 11)
  maturation_simulations <- future_lapply(models, function(m){
    
    maturation_m <- data.frame()
    sim_path <- ifelse(species == "fagus_sylvatica", file.path(sim_dir, species, "pbm", m), file.path(sim_dir, species, m))
    for(yr in 1970:2000){
      
      maturation <- read_mean_outputvalue(sim_path, years = yr, model = "PHENOFIT",
                                          output_var = "FruitMaturationDate", correct_date = TRUE)
      names(maturation) <- c("lat", "lon", as.character(yr))
      
      maturation <- maturation %>%
        inner_join(unique(records[records$year == yr,c("lat", "lon")]), by = join_by(lat, lon))
      
      if(yr == 1970){
        maturation_m <- maturation
      }else{
        maturation_m <- maturation_m %>%
          full_join(maturation, by = join_by(lat, lon))
      }
      
    }
    gc()
    
    maturation_m <- maturation_m %>%
      pivot_longer(cols = -c(lat, lon), names_to = "year", values_to = "sim_doy", values_drop_na = TRUE) %>%
      mutate(year = as.numeric(year), mod = m) %>% 
      left_join(records[c("lat", "lon", "year", "stade", "mean_doy")], by = join_by(lat, lon, year))
    
    # leafout_rmse <- leafout_m %>%
    #   group_by(lat, lon, year, stade, sim_doy, mean_doy) %>%
    #   reframe(rmse = rmse(sim_doy, mean_doy),  mod = m)
    
    return(maturation_m)
    
  })
  plan(sequential); gc()
  maturation_simulations <- as.data.frame(do.call(rbind, maturation_simulations))
  saveRDS(maturation_simulations, file = file.path(wd, "figures", "data", "fig7", paste0(species,"_maturation.rds")))
  
}else{
  
  maturation_simulations <- readRDS(file.path(wd, "figures", "data", "fig7", paste0(species,"_maturation.rds")))
}

# clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds"))

data_boxplot <- maturation_simulations %>%
  dplyr::filter(mean_doy > 212) %>% # discard strange observed date (before 1st August...)
  # left_join(clusters, join_by(mod)) %>%
  group_by(lat, lon, mod, year) %>%
  reframe(rmse = ifelse(sim_doy != 366, rmse(sim_doy, mean_doy), NA), sim_doy = sim_doy) %>%
  mutate(mod = reorder(mod, rmse, median, na.rm = TRUE, decreasing = FALSE),
         type = ifelse(mod=="expert", "expert", "partial"))
# data_boxplot$clust <- ifelse(data_boxplot$mod == 'expert', 0, data_boxplot$clust)

# median_rmse <- data_boxplot %>%
#   group_by(clust) %>%
#   summarise(median_rmse = median(rmse, na.rm = TRUE)) %>%
#   dplyr::filter(clust != "3_1")

prop_mat <- data_boxplot %>% group_by(mod) %>%
  summarise(prop = sum(is.na(rmse))/n()*100)

data_mat <- data_boxplot
data_boxplot <- data_boxplot %>%
  left_join(prop_mat, by = join_by(mod)) %>%
  mutate(rmse = ifelse(prop > 60, NA, rmse)) %>%
  mutate(mod = reorder(mod, rmse, median, na.rm = TRUE, decreasing = FALSE))
data_mat$mod = factor(data_mat$mod, levels(data_boxplot$mod))

maturation_rmse_boxplots <- data_boxplot %>%
  mutate(mod = reorder(mod, rmse, median, decreasing = FALSE, na.rm = TRUE)) %>% 
  ggplot() +
  geom_boxplot(aes(x = mod, y = rmse, fill = type, col = type),
               outlier.shape = NA, alpha = 0.7, linewidth = 0.5) +
  geom_text(data = unique(data_boxplot[, "mod"]), aes(x = mod, y = 60, label = mod), stat = 'identity', angle = 90, size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#577590", "#f9c74f"), labels = c("Expert", "Partial")) +
  scale_color_manual(values = c("#577590", "#f9c74f"), labels = c("Expert", "Partial")) +
  scale_y_continuous(breaks = seq(0, 200, 15), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,75), xlim = c(1,11), clip = "off") + 
  # geom_hline(data = median_rmse, aes(yintercept = median_rmse), 
  #            linewidth = 0.8, color = "white", alpha = 0.6) +
  # geom_hline(data = median_rmse, aes(yintercept = median_rmse, color = clust), 
  #            lty = "dotted", linewidth = 0.6) +
  # ggstar::geom_star(data = median_rmse, aes(x = 104.7, y = median_rmse, fill = clust, col = clust), 
  #                   alpha = 0.7, angle = 90, starshape = 26, size = 2) +
  ylab("Fruit maturation date") + 
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 7.5), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))

without_maturation_boxplot <- data_mat %>%
  ggplot() +
  geom_bar(aes(x = mod, alpha = is.na(rmse), fill = type),
           position="fill") +
  scale_alpha_manual(values = c("FALSE" = 0, "TRUE" = 0.7)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#577590", "#f9c74f"), labels = c("Expert", "Partial")) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25), expand = c(0,0),
    labels = c("0%" , "25", "50", "75", "100%"), position = "left") +
  coord_cartesian(ylim = c(0,1), xlim = c(1,11), clip = "off") + 
  ylab("No maturation") + 
  theme(axis.text.y = element_text(size = 6), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 6.5), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3))
