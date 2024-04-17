library(data.table)
library(future.apply)
library(ggpplot2)
library(ModelMetrics)

# Simulations against data
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")
species <- "fagus_sylvatica"

models <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

plan(multisession, workers = 34)
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
leafout_simulations2 <- as.data.frame(do.call(rbind, leafout_simulations))

clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds"))

leafout_simulations2 %>%
  left_join(clusters, join_by(mod)) %>%
  group_by(clust, mod, stade, year) %>%
  reframe(rmse = rmse(sim_doy, mean_doy)) %>%
  mutate(mod = reorder(mod, rmse, median)) %>% 
  ggplot() +
  facet_wrap(~ stade, scales = "free_y") +
  geom_boxplot(aes(x = mod, y = rmse, color = ifelse(mod == 'expert', 0, clust)),
               outlier.size = 0.2) +
  theme_minimal() +
  labs(x = "", y = "RMSE (day)") +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_color_manual(values = c("black", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  labs(title = "Yearly RMSE on leafout day (order by median RMSE)")

test <- leafout_simulations2 %>%
  left_join(clusters, join_by(mod)) %>%
  dplyr::filter(stade != 10) %>%
  group_by(lat, lon, clust, mod, year) %>%
  reframe(rmse = rmse(sim_doy, mean_doy)) %>%
  mutate(mod = reorder(mod, rmse, median))
test$clust <- ifelse(test$mod == 'expert', 0, test$clust)

boxplots <- test %>%
  mutate(mod = reorder(mod, rmse, median, decreasing = FALSE)) %>% 
  ggplot() +
  geom_boxplot(aes(x = mod, y = rmse, color = clust, fill = clust),
               outlier.shape = NA, alpha = 0.3) +
  theme_minimal() +
  labs(x = "", y = "RMSE (day)") +
  theme(axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(), legend.position = "none") +
  scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  labs(title = "Yearly RMSE on leafout day (order by median RMSE)") +
  scale_y_continuous(breaks = seq(0, 200, 15), limits = c(0,90)) +
  geom_hline(data = median_rmse, aes(yintercept = median_rmse, color = clust), 
             lty = "dashed", linewidth = 0.7)
  
ggsave(boxplots, filename = file.path(wd, "scripts/explore/graphs/last/isa/part1/pheno", "boxplots_rmse_all.pdf"),
       width = 297, height = 210, units = "mm")


library(ggdist)

median_rmse <- test %>%
  group_by(clust) %>%
  summarise(median_rmse = median(rmse)) %>%
  dplyr::filter(clust != "3_1")

cal_sel <- c("subset3_rep1","subset6_rep4","subset4_rep3","subset6_rep2","subset6_rep9","subset3_rep7","expert","subset6_rep8","subset4_rep10",
             "subset1_rep4","subset2_rep1")

ridgeplots <- test %>%
  mutate(mod = reorder(mod, rmse, median, decreasing = TRUE)) %>% 
  dplyr::filter(mod %in% cal_sel) %>%
  ggplot(aes(mod, rmse, 
             fill = clust,
             color = clust,
             alpha = after_stat(cut_cdf_qi(cdf, .width = c(0.66, 0.95, 1))))) +
  stat_halfeye() +
  # stat_interval() +
  # stat_summary(geom = "point", fun = median) +
  scale_x_discrete(labels = toupper) +
  scale_y_continuous(breaks = seq(0, 200, 15)) +
  # scale_color_manual(values = MetBrewer::met.brewer("Hokusai3")) +
  coord_flip(ylim = c(0, 60), clip = "on") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
    axis.text.y = element_blank(),
    plot.margin = margin(4, 4, 4, 0),
    legend.position = 'none'
  ) +
  labs(y = "RMSE (day)", x = "") +
  geom_hline(data = median_rmse, aes(yintercept = median_rmse, color = clust), lty = "dashed", linewidth = 0.7) +
  scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("0", "1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_alpha_manual(values = c(0.7, 0.5, 0.3)) +
  labs(title = "Yearly RMSE on leafout day (10 best calibrations)")

ggsave(ridgeplots, filename = file.path(wd, "scripts/explore/graphs/last/isa/part1/pheno", "ridgeplot_rmse_10bests.pdf"),
       width = 210, height = 297, units = "mm")



