
# Boxplot of predicted phenological dates

species <- data.frame(
  name = c("fagus_sylvatica"),
  occ_path = c("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds")
)

models <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))
sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")

s <- 1
if(reload_data_fig2){
  plan("multisession", workers = 6)
  dates_df <- foreach(m = models, .combine = rbind) %dofuture% {
    cat(paste0(m,"\n"))
    
    sim_path <- file.path(sim_dir, species[s, "name"], m)
    
    dormancy <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                           output_var = "LeafDormancyBreakDate", correct_date = TRUE)[,c(2,1,3)])
    ecodormancy <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                              output_var = "EcodormancyCustom", correct_date = TRUE)[,c(2,1,3)])
    leafout <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                          output_var = "LeafUnfoldingDate", correct_date = TRUE)[,c(2,1,3)])
    flowering <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                            output_var = "FloweringDate", correct_date = TRUE)[,c(2,1,3)])
    maturation <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                             output_var = "FruitMaturationDate", correct_date = TRUE)[,c(2,1,3)])
    senescence <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                             output_var = "LeafSenescenceDate", correct_date = TRUE)[,c(2,1,3)])
    
    dates <- c(dormancy, ecodormancy, leafout, flowering, maturation, senescence)
    names(dates) <- c("dormancy_date", "ecodormancy_length", "leafout_date", "flowering_date", "maturation_date", "senescence_date")
    
    presence <- rast(readRDS(species[s, "occ_path"]))
    absence <- presence
    presence[presence == 0] <- NA
    absence[absence == 1] <- NA
    
    date_pres <- mask(crop(dates, presence), presence) %>%
      as.data.frame()
    date_abs <- mask(crop(dates, absence), absence) %>%
      as.data.frame()
    
    rbind(
      data.frame(mod = m, species = species[s, "name"], obs = c("presence"), date_pres),
      data.frame(mod = m, species = species[s, "name"], obs = c("absence"), date_abs))
    
  }
  plan(sequential);gc()
  
  
  clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds"))
  
  dates_df <- dates_df %>%
    left_join(clusters, join_by(mod))
  
  dates_df$clust <- ifelse(dates_df$mod == "expert", 0, dates_df$clust)

  dates_df$sub <- stringr::str_split(dates_df$mod, "_", simplify = T)[, 1]
  
  saveRDS(dates_df, file = file.path(wd, "figures", "data", "fig2", "simulated_dates.rds"))
}else{
  dates_df <- readRDS(file.path(wd, "figures", "data", "fig2", "simulated_dates.rds"))
}


dates_df2 <- dates_df %>%
  mutate(mod = reorder(mod, dormancy_date, median)) %>% 
  tidyr::pivot_longer(cols = c("dormancy_date", "ecodormancy_length", "leafout_date", "flowering_date", "maturation_date", "senescence_date"),
                      names_to = "var", values_to = "index") %>% dplyr::filter(mod != "expert" & var != "flowering_date" & var != "leafout_date")

endodormancy <- ggplot(data = dates_df2 %>% dplyr::filter(var == "dormancy_date")) +
  geom_boxplot(aes(x = mod, y = index, fill = as.factor(clust), color = as.factor(clust)), 
               outlier.shape = NA, alpha = 0.3, linewidth = 0.3) +
  theme_minimal() +
  scale_fill_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                    breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                     breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  coord_cartesian(clip = 'off', ylim = c(-120, 170)) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-100, 150, 50)) +
  ylab("Endodormancy break (DOY)") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 8), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        plot.margin = margin(t = 20, b = 10, r = 5.5, l = 5.5))

ecodormancy <- ggplot(data = dates_df2 %>% dplyr::filter(var == "ecodormancy_length")) +
  geom_boxplot(aes(x = mod, y = index, fill = as.factor(clust), color = as.factor(clust)), 
               outlier.shape = NA, alpha = 0.3, linewidth = 0.3) +
  theme_minimal() +
  scale_fill_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                    breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                     breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  coord_cartesian(clip = 'off', ylim = c(0, 400)) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab("Encodormancy length (no. days)") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 8), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        plot.margin = margin(t = 20, b = 10, r = 5.5, l = 5.5))

maturation <- ggplot(data = dates_df2 %>% dplyr::filter(var == "maturation_date")) +
  geom_boxplot(aes(x = mod, y = index, fill = as.factor(clust), color = as.factor(clust)), 
               outlier.shape = NA, alpha = 0.3, linewidth = 0.3) +
  theme_minimal() +
  scale_fill_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                    breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                     breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  coord_cartesian(clip = 'off', ylim = c(130, 370)) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(150, 370, 30)) +
  ylab("Fruit maturation (DOY)") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 8), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        plot.margin = margin(t = 10, b = 20, r = 5.5, l = 5.5))

senescence <- ggplot(data = dates_df2 %>% dplyr::filter(var == "senescence_date")) +
  geom_boxplot(aes(x = mod, y = index, fill = as.factor(clust), color = as.factor(clust)), 
               outlier.shape = NA, alpha = 0.3, linewidth = 0.3) +
  theme_minimal() +
  scale_fill_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                    breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("#577590", "#43AA8B", "#957fc6", '#f9c74f', "#F9844A"),
                     breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  coord_cartesian(clip = 'off', ylim = c(210, 370)) +
  scale_x_discrete(expand = c(0.01,0.01)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(210, 370, 30)) +
  ylab("Leaf senescence (DOY)") +
  theme(axis.text.y = element_text(size = 7), axis.text.x = element_blank(),
        legend.text = element_text(size = 7), legend.title = element_blank(),
        legend.key.height = unit(0.5,"cm"), legend.key.width = unit(0.4,"cm"),
        axis.title.y = element_text(size = 8), axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        legend.position = "none",
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        panel.grid.minor.y = element_blank(), ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        plot.margin = margin(t = 10, b = 20, r = 5.5, l = 5.5))
