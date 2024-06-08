
maturation_rmse_boxplots <- data_boxplot %>%
  # group_by(species) %>%
  # mutate(mod = reorder(mod, rmse, median, decreasing = FALSE, na.rm = TRUE)) %>% 
  # ungroup() %>%
  ggplot() +
  facet_wrap(~ species, scales = "free_x", labeller = as_labeller(labels)) +
  geom_boxplot(aes(x = mod, y = rmse, fill = type, col = type),
               outlier.shape = NA, alpha = 0.5, linewidth = 0.3) +
  # geom_text(data = unique(data_boxplot[, "mod"]), aes(x = mod, y = 60, label = mod), stat = 'identity', angle = 90, size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c("#577590", "#f9c74f"), labels = c("Expert", "Partial")) +
  scale_color_manual(values = c("#577590", "#f9c74f"), labels = c("Expert", "Partial")) +
  scale_y_continuous(breaks = seq(0, 200, 15), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,90), xlim = c(1,11), clip = "off") + 
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
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        strip.text = element_text(size = 7, color = "grey30"))

without_maturation_boxplot <- data_mat %>%
  ggplot() +
  facet_wrap(~ species, scales = "free_x", labeller = as_labeller(labels)) +
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
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        strip.text = element_blank())
