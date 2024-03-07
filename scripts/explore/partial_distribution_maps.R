
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
library(cowplot)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

calibrations <- c(paste0("partial/relmax/subset",rep(1, each = 5),"_rep", 1:5))
fit_dir <- file.path(wd, "data", "fit")

species <- "fagus_sylvatica"

fitted_rast <- sum(rast(lapply(calibrations, function(c){
  sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
  output <- read_mean_outputvalue(file.path(sim_dir, species, c), 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
  output <- rast(output[c(2,1,3)])
  background <- ifel(is.na(output), NA, 0) %>% crop(ext(-10.35, 34.85, 34.65, 70.55))
  
  threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
  output <- ifel(output < threshold, 0, 1)
  
  return(output)
  
}))) %>% crop(ext(-10.35, 34.85, 34.65, 70.55))

background0 <- ifel(is.na(background), 1, 0)

fitted_map <- ggplot() + 
  geom_spatraster_contour(data = background0, na.rm = FALSE) +
  geom_spatraster(data = fitted_rast) +
  scale_fill_gradientn(colours = c("#edf2f4", "#f9c74f", "#90be6d", "#4d908e", "#577590"), na.value = NA) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.2, ticks = FALSE)) +
  theme(legend.key.height = unit(0.15, 'cm'), legend.key.width = unit(0.8, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size = 7),
        panel.border = element_rect(colour = "black", fill=NA, size=0.2),
        plot.margin = unit(c(2,2,2,2), units = 'mm'),
        legend.margin=margin(t = 0.05, b=0.05, r = 0.3, l = 0.3, unit='cm'),
        legend.position=c(.7,.9),
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  coord_cartesian(expand = FALSE)

ggsave(fitted_map, filename = file.path(wd, "scripts", "explore", "graphs", "last", "partial_relmax_distribution_maps.pdf"),
       width = 90, height = 80, unit = "mm")

