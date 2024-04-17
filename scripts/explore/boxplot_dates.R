
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
library(dplyr)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

library(doFuture)

species <- data.frame(
  name = c("fagus_sylvatica"),
  occ_path = c("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds")
)

models <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))
sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")

s <- 1
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
fhleafmin_values <- readRDS(file.path(wd, "data", "metrics", "fhleafmin_values.rds"))

dates_df2 <- dates_df %>%
  left_join(clusters, join_by(mod)) %>%
  left_join(fhleafmin_values, by = c("mod"="rep")) 

dates_df2$clust <- ifelse(dates_df2$mod == "expert", 0, dates_df2$clust)
# dates_df2$FHleafmin <- ifelse(dates_df2$mod == "expert", -5.3, dates_df2$FHleafmin)

dates_df2$sub <- stringr::str_split(dates_df2$mod, "_", simplify = T)[, 1]

dates_df2 <- dates_df2 %>%
  mutate(mod = reorder(mod, dormancy_date, median)) %>% 
  tidyr::pivot_longer(cols = c("dormancy_date", "ecodormancy_length", "leafout_date", "flowering_date", "maturation_date", "senescence_date"),
                      names_to = "var", values_to = "index")

boxplot_fagus <- ggplot(data = dates_df2 %>% dplyr::filter(mod != "expert" & var != "flowering_date")) +
  facet_wrap(~ var, switch = "y", scales = "free_y", shrink = TRUE, ncol = 1) +
  geom_boxplot(aes(x = mod, y = index, fill = as.factor(clust), color = as.factor(clust)), outlier.shape = NA, alpha = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), axis.title = element_blank(),
        legend.position = "none", strip.text.x = element_text(size = 11)) +
  scale_fill_manual(values = c("#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  scale_color_manual(values = c("#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                    breaks = c("1_1", "1_2", "3_1", "2_1", "2_2"))

ggsave(boxplot_fagus, filename = file.path(wd, "scripts/explore/graphs/last/isa/part1", "boxplotdates_newclusters.pdf"),
       width = 210, height = 297, units = "mm")


boxplot_leafout <- ggplot(data = dates_df2[dates_df2$var %in% c("dormancy_date", "ecodormancy_length"), ]) +
  facet_grid(var ~ obs, switch = "y", scales = "free_y", shrink = TRUE) +
  geom_boxplot(aes(x = mod, y = index, fill = clust, color = clust), outlier.shape = NA, alpha = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), axis.title = element_blank(),
        legend.position = "none", strip.text.x = element_text(size = 11))
ggsave(boxplot_leafout, filename = file.path(wd, "scripts", "explore", "graphs", "last", "boxplots_leafdates_slide.pdf"),
       height = 21, width = 29.7, units = "cm")

