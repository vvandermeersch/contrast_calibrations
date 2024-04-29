
# Historical maps
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

calibrations <- paste0("subset",rep(1:10, each = 10),"_rep", 1:10)
fit_dir <- file.path(wd, "data", "fit")

species <- "fagus_sylvatica"

if(reload_data_fig1){
  fitted_rast <- sum(rast(lapply(calibrations, function(c){
    sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
    output <- read_mean_outputvalue(file.path(sim_dir, species, "pbm", c), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
    output <- rast(output[c(2,1,3)])
    background <- ifel(is.na(output), NA, 0) %>% crop(ext(-10.35, 34.85, 34.65, 70.55))
    
    threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
    output <- ifel(output < threshold, 0, 1)
    
    return(output)
    
  }))) %>% crop(ext(-10.35, 34.85, 34.65, 70.55))
  saveRDS(fitted_rast, file = file.path(wd, "figures", "data", "fig1", "fitted_map_historical.rds"))
}else{
  fitted_rast <- readRDS(file = file.path(wd, "figures", "data", "fig1", "fitted_map_historical.rds"))
}

# Load background 
sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
output <- read_mean_outputvalue(file.path(sim_dir, species, "pbm", "subset1_rep1"), 
                                years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
output <- rast(output[c(2,1,3)])
background <- ifel(is.na(output), 1, 0) %>% crop(ext(-10.35, 34.85, 34.65, 70.55))
rm(output)

fitted_map <- ggplot() + 
  geom_spatraster_contour(data = background, na.rm = FALSE, color = "grey") +
  geom_spatraster(data = fitted_rast) +
  scale_fill_gradientn(colours = c("#edf2f4", "#f9c74f", "#90be6d", "#4d908e", "#577590"), na.value = NA) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size = 6, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(2,2,2,2), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.3, l = 0.3, unit='cm'),
        legend.position=c(.6,.9),
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  coord_cartesian(expand = FALSE)

if(reload_data_fig1){
  expert_rast <- sum(rast(lapply("expert", function(c){
    sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-LAND")
    output <- read_mean_outputvalue(file.path(sim_dir, species, "pbm", c), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
    output <- rast(output[c(2,1,3)])
    
    threshold <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$best_threshold
    output <- ifel(output < threshold, 0, 1)
    
    return(output)
    
  }))) %>% crop(ext(-10.35, 34.85, 34.65, 70.55))
  saveRDS(expert_rast, file = file.path(wd, "figures", "data", "fig1", "expert_map_historical.rds"))
}else{
  expert_rast <- readRDS(file = file.path(wd, "figures", "data", "fig1", "expert_map_historical.rds"))
}


expert_map <- ggplot() + 
  geom_spatraster_contour(data = background, na.rm = FALSE, color = "grey") +
  geom_spatraster(data = expert_rast) +
  scale_fill_gradientn(colours = c("#edf2f4", "#f9c74f", "#90be6d", "#4d908e", "#577590"), na.value = NA) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.2, ticks = FALSE)) +
  theme(legend.key.height = unit(0.15, 'cm'), legend.key.width = unit(0.8, 'cm'),
        legend.title = element_blank(), legend.text = element_text(size = 7),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(2,2,2,2), units = 'mm'),
        legend.margin=margin(t = 0.05, b=0.05, r = 0.3, l = 0.3, unit='cm'),
        legend.position=c(.7,.9),
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  coord_cartesian(expand = FALSE)

historical_maps <- 
  plot_grid(expert_map + theme(legend.position = "none"), fitted_map,
            ncol = 2, rel_widths = c(1,1,0.1), labels = c("a.", "b."), label_size = 10, hjust = -2, vjust = 3)
