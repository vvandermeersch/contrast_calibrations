
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
library(cowplot)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

calibrations <- paste0("subset",rep(1:10, each = 10),"_rep", 1:10)
fit_dir <- file.path(wd, "data", "fit")

species <- "fagus_sylvatica"
occ_path <- "D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds"

fitted_rast <- sum(rast(lapply(calibrations, function(c){
  sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
  output <- read_mean_outputvalue(file.path(sim_dir, species, c), 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
  output <- rast(output[c(2,1,3)])
  
  threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
  background <- ifel(is.na(output), NA, 0)
  output <- ifel(output >= threshold, NA, 1)
  
  presence <- rast(readRDS(occ_path))
  presence[presence == 0] <- NA
  
  background <- crop(background, presence)
  output <- mask(crop(output, presence), presence)
  
  return(sum(c(output, background), na.rm =TRUE))
})))

background0 <- ifel(is.na(background), 1, 0)

fitted_map <- ggplot() + 
  geom_spatraster_contour(data = background0, na.rm = FALSE) +
  geom_spatraster(data = fitted_rast) +
  scale_fill_gradientn(colours = c("#edf2f4", "#f9c74f", "#ff5733", "#c70039", "#900c3f"), na.value = NA) +
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


expert_rast <- sum(rast(lapply("expert", function(c){
  sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
  output <- read_mean_outputvalue(file.path(sim_dir, species, c), 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
  output <- rast(output[c(2,1,3)])
  
  threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
  background <- ifel(is.na(output), NA, 0)
  output <- ifel(output >= threshold, NA, 1)
  
  presence <- rast(readRDS(occ_path))
  presence[presence == 0] <- NA
  
  background <- crop(background, presence)
  output <- mask(crop(output, presence), presence)
  
  return(sum(c(output, background), na.rm =TRUE))
})))

expert_map <- ggplot() + 
  geom_spatraster_contour(data = background0, na.rm = FALSE) +
  geom_spatraster(data = expert_rast) +
  scale_fill_gradientn(colours = c("#edf2f4", "#f9c74f", "#ff5733", "#c70039", "#900c3f"), na.value = NA) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "vertical", 
                               frame.colour = "black", frame.linewidth = 0.3, ticks = FALSE)) +
  theme(legend.key.height = unit(0.8, 'cm'), legend.key.width = unit(0.2, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size = 8)) +
  theme(legend.key.height = unit(0.8, 'cm'), legend.key.width = unit(0.2, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill=NA, size=0.2),
        plot.margin = unit(c(2,2,2,2), units = 'mm')) +
  coord_cartesian(expand = FALSE)

error_maps <- 
  plot_grid(expert_map + theme(legend.position = "none"), fitted_map, 
            ncol = 2, rel_widths = c(1,1), labels = c("a.", "b."), label_size = 10, hjust = -2, vjust = 3)

ggsave(error_maps, filename = file.path(wd, "scripts", "explore", "graphs", "last", "error_maps.pdf"),
       width = 180, height = 80, unit = "mm")


# Link with altitude ?

altitude <- rast(fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_Altitude.fit")[,c(2,1,3)])

nfitted_wrong <- 10
common_errors <- ifel(fitted_rast >= nfitted_wrong & expert_rast > 0, 1, NA)
fitted_only_errors <- ifel(fitted_rast >= nfitted_wrong & expert_rast == 0, 1, NA)
expert_only_errors <- ifel(fitted_rast == 0 & expert_rast > 0, 1, NA)

altitude_common_errors <- mask(crop(altitude, common_errors), common_errors) %>%
  as.data.frame() %>% mutate(cat = "Common\nerrors")
altitude_fitted_errors <- mask(crop(altitude, fitted_only_errors), fitted_only_errors) %>%
  as.data.frame() %>% mutate(cat = "Fitted\nonly")
altitude_expert_errors <- mask(crop(altitude, expert_only_errors), expert_only_errors) %>%
  as.data.frame() %>% mutate(cat = "Expert\nonly")

altitude_errors <- rbind(altitude_common_errors, altitude_fitted_errors, altitude_expert_errors) %>% rename("Altitude" = "V3")
errors_count <- altitude_errors %>% group_by(cat) %>% summarise(nerrors = n())

alt_boxplot <- ggplot(data = altitude_errors) +
  geom_boxplot(aes(y = Altitude, x = cat), fill = "#edf2f4", alpha = 1,
               outlier.size = .1, width = 0.5, linewidth = 0.3, fatten = 1.5) +
  theme_minimal() + xlab("") +
  theme(axis.text = element_text(size = 8), axis.title = element_text(size = 9),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.2),
        plot.margin = unit(c(2,2,-3,2), units = 'mm'))

error_maps_walt <-
  plot_grid(expert_map + theme(legend.position = "none"), fitted_map, alt_boxplot,
            ncol = 3, rel_widths = c(1,1, 0.6), labels = c("a.", "b.", "c."), label_size = 10, hjust = -2, vjust = 3)

ggsave(error_maps_walt, filename = file.path(wd, "scripts", "explore", "graphs", "last", "error_maps_walt.pdf"),
       width = 230, height = 80, unit = "mm")

