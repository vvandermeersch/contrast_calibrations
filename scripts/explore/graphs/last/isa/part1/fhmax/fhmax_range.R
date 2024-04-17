
measured <- data.frame(rep = "meas", clust = 0, FHmax = c(-21, -22, -30, -29.5, -30.4, -31.6, -32.18, -33, -31, -36))

frost_parameters <- species_parameters %>%
  dplyr::filter(grepl("frost", species_parameters$var))
frost_parameters$var <- factor(frost_parameters$var, levels = paste0('frost',1:12))

clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds"))

frost_par_sum <- frost_parameters %>%
  group_by(rep) %>%
  summarize(
    FHleafmin = value[var == "frost3"],
    FHflowermin = value[var == "frost4"],
    FHtfemax = value[var == "frost7"],
    FHtflmax = value[var == "frost8"],
    FHpfemax = value[var == "frost9"],
    FHpflmax = value[var == "frost10"],
    FHflmax = FHtflmax + FHpflmax,
    FHfemax = FHtfemax + FHpfemax,
    FHmax = (FHflmax+FHfemax)/2,
    FHmin = (FHleafmin+FHflowermin)/2
  ) %>%
  ungroup() %>% 
  left_join(clusters, by = c("rep" = "mod"))
  
data_plot <- rbind(measured, frost_par_sum[, c("rep", "clust", "FHmax")])
fhmax_range <- ggplot() +
  geom_boxplot(data = data_plot %>% dplyr::filter(rep == "meas"), 
               aes(x = 1, y = FHmax, col = clust, fill = clust),
               alpha = 0.3, width = 0.2) +
  geom_boxplot(data = data_plot %>% dplyr::filter(rep != "meas"), 
               aes(x = 2, y = FHmax, col = clust, fill = clust),
               alpha = 0.3, width = 1) +
  scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  geom_point(aes(y = -20, x = 0.5),  fill = 'darkred', size = 3, alpha = 0.9, shape = 23, colour="white") +
  geom_text(aes(y = -20.8, x = 0.5, label = "Expert\ncalibration"), size = 3, color = "darkred") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.position = 'none',
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Measurements", "Calibrations"), lim = c(0.5,2.5)) +
  coord_cartesian(clip = "off")

ggsave(plot = fhmax_range, file = file.path(wd, "scripts", "explore", "graphs", "last", "isa", "part1", "fhmax_range.pdf"),
       width = 100, height = 150, unit = "mm")


fhmin_range <- ggplot() +
  geom_boxplot(data = frost_par_sum, 
               aes(x = 2, y = FHmin, col = clust, fill = clust),
               alpha = 0.3, width = 1) +
  scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  geom_point(aes(y = -5.3, x = 0.5),  fill = 'darkred', size = 3, alpha = 0.9, shape = 23, colour="white") +
  geom_text(aes(y = -4.9, x = 0.5, label = "Expert\ncalibration"), size = 3, color = "darkred") + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.position = 'none',
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Measurements", "Calibrations"), lim = c(0.5,2.5)) +
  coord_cartesian(clip = "off")

ggsave(plot = fhmin_range, file = file.path(wd, "scripts", "explore", "graphs", "last", "isa", "part1", "fhmin_range.pdf"),
       width = 100, height = 150, unit = "mm")


