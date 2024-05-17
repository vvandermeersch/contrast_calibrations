
records <- all_records %>%
  dplyr::filter(stade %in% c(94, 95))

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")
species <- "fagus_sylvatica"

models <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

if(reload_data_fig4){
  
  plan(multisession, workers = 20)
  senescence_simulations <- future_lapply(models, function(m){
    
    senescence_m <- data.frame()
    sim_path <- file.path(sim_dir, species, "pbm", m)
    for(yr in 1970:2000){
      
      senescence <- read_mean_outputvalue(sim_path, years = yr, model = "PHENOFIT",
                                          output_var = "LeafSenescenceDate", correct_date = TRUE)
      names(senescence) <- c("lat", "lon", as.character(yr))
      
      senescence <- senescence %>%
        inner_join(unique(records[records$year == yr,c("lat", "lon")]), by = join_by(lat, lon))
      
      if(yr == 1970){
        senescence_m <- senescence
      }else{
        senescence_m <- senescence_m %>%
          full_join(senescence, by = join_by(lat, lon))
      }
      
    }
    gc()
    
    senescence_m <- senescence_m %>%
      pivot_longer(cols = -c(lat, lon), names_to = "year", values_to = "sim_doy", values_drop_na = TRUE) %>%
      mutate(year = as.numeric(year), mod = m) %>% 
      left_join(records[c("lat", "lon", "year", "stade", "mean_doy")], by = join_by(lat, lon, year))
    
    # leafout_rmse <- leafout_m %>%
    #   group_by(lat, lon, year, stade, sim_doy, mean_doy) %>%
    #   reframe(rmse = rmse(sim_doy, mean_doy),  mod = m)
    
    return(senescence_m)
    
  })
  plan(sequential); gc()
  senescence_simulations <- as.data.frame(do.call(rbind, senescence_simulations))
  saveRDS(senescence_simulations, file = file.path(wd, "figures", "data", "fig4", "senescence_simulations.rds"))
  
}else{
  
  senescence_simulations <- readRDS(file.path(wd, "figures", "data", "fig4", "senescence_simulations.rds"))
}

clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds"))

data_boxplot <- senescence_simulations %>%
  left_join(clusters, join_by(mod)) %>%
  group_by(lat, lon, clust, mod, year) %>%
  reframe(rmse = ifelse(sim_doy != 366, rmse(sim_doy, mean_doy), NA), sim_doy = sim_doy) %>%
  mutate(mod = reorder(mod, rmse, median, na.rm = TRUE, decreasing = FALSE))
data_boxplot$clust <- ifelse(data_boxplot$mod == 'expert', 0, data_boxplot$clust)

median_rmse <- data_boxplot %>%
  group_by(clust) %>%
  summarise(median_rmse = median(rmse, na.rm = TRUE)) %>%
  dplyr::filter(clust != "3_1" & clust != "2_1" & clust != "1_2")

prop_sen <- data_boxplot %>% group_by(mod) %>%
  summarise(prop = sum(is.na(rmse))/n()*100)

data_sen <- data_boxplot
data_boxplot <- data_boxplot %>%
  left_join(prop_sen, by = join_by(mod)) %>%
  mutate(rmse = ifelse(prop > 60, NA, rmse)) %>%
  mutate(mod = reorder(mod, rmse, median, na.rm = TRUE, decreasing = FALSE))
data_sen$mod = factor(data_sen$mod, levels(data_boxplot$mod))

senescence_rmse_boxplots <- data_boxplot %>%
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
  scale_y_continuous(
    breaks = seq(0, 200, 15), expand = c(0,0), position = "right") +
  coord_cartesian(ylim = c(0,120), xlim = c(1,101), clip = "off") + 
  # geom_hline(data = median_rmse, aes(yintercept = median_rmse), 
  #            linewidth = 0.8, color = "white", alpha = 0.6) +
  # geom_hline(data = median_rmse, aes(yintercept = median_rmse, color = clust), 
  #            lty = "dotted", linewidth = 0.6) +
  ggstar::geom_star(data = median_rmse, aes(x = -2.8, y = median_rmse, fill = clust, col = clust), 
                    alpha = 0.7, angle = -90, starshape = 26, size = 2) +
  ylab("Leaf senescence date") + 
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
    data = data.frame(x = 14, y = 97,image="C:/Users/vandermeersch/Documents/CEFE/phd/notebook/phenofit_schema/senescence.png"),
    aes(x = x, y = y , image = image), size=0.4)


without_senescence_boxplot <- data_sen %>%
  ggplot() +
  geom_bar(aes(x = mod, alpha = is.na(rmse), fill = clust),
           position="fill") +
  scale_alpha_manual(values = c("FALSE" = 0, "TRUE" = 0.7)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_color_manual(values = c("grey", "#577590", "#43AA8B", "#ac92eb", '#f9c74f', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_fill_manual(values = c("grey", "#577590", "#43AA8B", "#ac92eb", '#f9c74f', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.25), expand = c(0,0),
    labels = c("0%" , "25", "50", "75", "100%"), position = "right") +
  coord_cartesian(ylim = c(0,1), clip = "off") + 
  ylab("Not senescent") + 
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




