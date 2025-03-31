
records <- all_records %>%
  dplyr::filter(stade %in% c(10:15))

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")
species <- "fagus_sylvatica"

models <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

if(reload_data_fig4){
  
  plan(multisession, workers = 20)
  leafout_simulations <- future_lapply(models, function(m){
    
    leafout_m <- data.frame()
    sim_path <- file.path(sim_dir, species, m)
    for(yr in 1970:2000){
      
      leafout <- read_mean_outputvalue(sim_path, years = yr, model = "PHENOFIT",
                                       output_var = "LeafUnfoldingDate", correct_date = TRUE)
      names(leafout) <- c("lat", "lon", as.character(yr))
      
      leafout <- leafout %>%
        inner_join(unique(records[records$year == yr,c("lat", "lon")]), by = join_by(lat, lon))
      
      if(yr == 1970){
        leafout_m <- leafout
      }else{
        leafout_m <- leafout_m %>%
          full_join(leafout, by = join_by(lat, lon))
      }
      
    }
    gc()
    
    leafout_m <- leafout_m %>%
      pivot_longer(cols = -c(lat, lon), names_to = "year", values_to = "sim_doy", values_drop_na = TRUE) %>%
      mutate(year = as.numeric(year), mod = m) %>% 
      left_join(records[c("lat", "lon", "year", "stade", "mean_doy")], by = join_by(lat, lon, year))
    
    # leafout_rmse <- leafout_m %>%
    #   group_by(lat, lon, year, stade, sim_doy, mean_doy) %>%
    #   reframe(rmse = rmse(sim_doy, mean_doy),  mod = m)
    
    return(leafout_m)
    
  })
  plan(sequential); gc()
  leafout_simulations <- as.data.frame(do.call(rbind, leafout_simulations))
  saveRDS(leafout_simulations, file = file.path(wd, "figures", "data", "fig4", "leafout_simulations.rds"))
  
}else{
  
  leafout_simulations <- readRDS(file.path(wd, "figures", "data", "fig4", "leafout_simulations.rds"))
}

clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds"))

data_boxplot <- leafout_simulations %>%
  left_join(clusters, join_by(mod)) %>%
  dplyr::filter(stade != 10) %>%
  group_by(lat, lon, clust, mod, year) %>%
  reframe(rmse = rmse(sim_doy, mean_doy)) %>%
  mutate(mod = reorder(mod, rmse, median))
data_boxplot$clust <- ifelse(data_boxplot$mod == 'expert', 0, data_boxplot$clust)

median_rmse <- data_boxplot %>%
  group_by(clust) %>%
  summarise(median_rmse = median(rmse)) %>%
  dplyr::filter(clust != "3_1")


leafout_rmse_boxplots <- data_boxplot %>%
  mutate(mod = reorder(mod, rmse, median, decreasing = FALSE)) %>% 
  ggplot() +
  geom_boxplot(aes(x = mod, y = rmse, color = clust, fill = clust),
               outlier.shape = NA, alpha = 0.3, linewidth = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_color_manual(values = c("grey", "#577590", "#43AA8B", "#ac92eb", '#f9c74f', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_fill_manual(values = c("grey", "#577590", "#43AA8B", "#ac92eb", '#f9c74f', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_y_continuous(breaks = seq(0, 200, 15), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,105), xlim = c(1,101), clip = "off") + 
  # geom_hline(data = median_rmse, aes(yintercept = median_rmse), 
  #            linewidth = 0.8, color = "white", alpha = 0.6) +
  # geom_hline(data = median_rmse, aes(yintercept = median_rmse, color = clust), 
  #            lty = "dotted", linewidth = 0.6) +
  ggstar::geom_star(data = median_rmse, aes(x = 104.7, y = median_rmse, fill = clust, col = clust), 
                    alpha = 0.7, angle = 90, starshape = 26, size = 2) +
  
  ylab("Leafout date") + 
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 7.5), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3)) +
  ggimage::geom_image(
    data = data.frame(x = 14, y = 85,image=file.path(wd, 'figures', 'files','img', 'leafout.png')),
    aes(x = x, y = y , image = image), size=0.4)

cat("Leafout RMSE\n")
data_boxplot %>%
  group_by(substr(clust, 0, 1)) %>% 
  summarise(mean = mean(rmse, na.rm = TRUE), sd = sd(rmse, na.rm = TRUE), 
            median = median(rmse, na.rm = TRUE),
            quantile(rmse, 0.25), quantile(rmse, 0.75))

# data_boxplot %>%
#   mutate(group = substr(clust, 0, 1)) %>%
#   dplyr::filter(group %in% c("0", "1", "2")) %>%
#   dunn_test(rmse ~ group, p.adjust.method = "holm")


data_estimate <- leafout_simulations %>%
  left_join(clusters, join_by(mod)) %>%
  dplyr::filter(stade != 10) %>%
  group_by(lat, lon, clust, mod, year) %>%
  reframe(difdoy = sim_doy-mean_doy) %>%
  mutate(mod = reorder(mod, difdoy, median))
data_estimate$clust <- ifelse(data_estimate$mod == 'expert', 0, data_estimate$clust)

data_estimate %>%
  dplyr::filter(clust != '0') %>%
  summarise(mean = mean(difdoy, na.rm = TRUE), sd = sd(difdoy, na.rm = TRUE))

