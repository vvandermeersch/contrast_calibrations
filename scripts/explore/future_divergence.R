
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(terra)
library(tidyterra)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))


gcms <- c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0")
calibrations <- c("subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7")
era5land_thresholds <- c(0.785, 0.674, 0.663, 0.803)


scenario <- "ssp245"
ssp2_dvg <- rast(lapply(gcms, function(m){
  output <- sum(rast(lapply(1:length(calibrations), function(i){
    sim_dir <- file.path(wd, "data", "simulations", "future", m, scenario)
    output <- read_mean_outputvalue(file.path(sim_dir, calibrations[i]), 
                                    years = c(2080:2100), model = "PHENOFIT", output_var = "Fitness")
    output <- rast(output[c(2,1,3)])
    output <- ifel(output < era5land_thresholds[i], 0, 1)
  })))
  output <- as.factor(output)
  names(output) <- paste(scenario, m, sep = "_")
  return(output)
}))
ssp2_dvgplots <- ggplot() + 
  geom_spatraster(data = ssp2_dvg) +
  facet_wrap(~lyr, nrow = 1) +
  scale_fill_manual(values = c("#edf2f4", "#f9c74f", "#90be6d", "#4d908e", "#577590"), na.translate = F) +
  theme_void()

scenario <- "ssp585"
ssp5_dvg <- rast(lapply(gcms, function(m){
  output <- sum(rast(lapply(1:length(calibrations), function(i){
    sim_dir <- file.path(wd, "data", "simulations", "future", m, scenario)
    output <- read_mean_outputvalue(file.path(sim_dir, calibrations[i]), 
                                    years = c(2080:2100), model = "PHENOFIT", output_var = "Fitness")
    output <- rast(output[c(2,1,3)])
    output <- ifel(output < era5land_thresholds[i], 0, 1)
  })))
  output <- as.factor(output)
  names(output) <- paste(scenario, m, sep = "_")
  return(output)
}))
ssp5_dvgplots <- ggplot() + 
  geom_spatraster(data = ssp5_dvg) +
  facet_wrap(~lyr, nrow = 1) +
  scale_fill_manual(values = c("#edf2f4", "#f9c74f", "#90be6d", "#4d908e", "#577590"), na.translate = F) +
  theme_void()
