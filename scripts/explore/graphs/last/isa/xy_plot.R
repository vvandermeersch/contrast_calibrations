
plot <- dates_df2 %>%
  mutate(id = rep(1:4741849, each = 6)) %>%
  dplyr::filter(var %in% c("flowering_date", "maturation_date")) %>%
  dplyr::select(mod, clust, id, var, index, obs) %>%
  tidyr::pivot_wider(names_from = var, values_from = index) %>%
  dplyr::filter(maturation_date < 366) %>%
  ggplot(aes(x = flowering_date, y = maturation_date-flowering_date, col = clust, fill = clust)) +
  facet_grid(obs ~ clust) +
  geom_point(size = 0.1, alpha = 0.3) +
  scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  geom_abline(slope=-1, intercept = 366, color = "black", size = 0.5) +
  theme(legend.position = 'none')

ggsave(plot = plot, file = file.path(wd, "scripts", "explore", "graphs", "last", "isa", "part1", 
                                     "xyplot_fruitdiffmat_wpresabs.png"),
       width = 297, height = 210, unit = "mm")

plot <- dates_df2 %>%
  mutate(id = rep(1:4741849, each = 6)) %>%
  dplyr::filter(var %in% c("maturation_date", "senescence_date")) %>%
  # dplyr::filter(mod %in% c("expert", "subset5_rep3", "subset1_rep3")) %>%
  dplyr::select(mod, clust, id, var, index, obs) %>%
  tidyr::pivot_wider(names_from = var, values_from = index) %>%
  dplyr::filter(maturation_date < 366) %>%
  ggplot(aes(x = maturation_date, y = senescence_date, col = clust, fill = clust)) +
  facet_grid(obs ~ clust) +
  geom_point(size = 0.1, alpha = 0.3) +
  scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  geom_abline(slope=1, intercept = 0, color = "black", size = 0.5) +
  theme(legend.position = 'none')

ggsave(plot = plot, file = file.path(wd, "scripts", "explore", "graphs", "last", "isa", "part1", 
                                     "xyplot_matsen_wpresabs.png"),
       width = 297, height = 210, unit = "mm")
