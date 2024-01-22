
# Run future simulations on chosen parameter sets

wd <- 'E:/USERS/VanderMeersch/projects/contrast_calibrations'
source(file.path(wd, "scripts", "functions", "run_phenofit.R"))

climate_folder <- "D:/CMIP6-Adjust"
parameter_dir <- file.path(wd, "data", "parameters")
output_dir <- file.path(wd, "data", "simulations")

species_files <- data.frame(
  name = c("expert", "subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7", "subset7_rep5"),
  path = c("expert/fagus_sylvatica.species", "fitted/subset_4/cmaes_fit_subset4_rep1.species",
           "fitted/subset_5/cmaes_fit_subset5_rep4.species", "fitted/subset_3/cmaes_fit_subset3_rep8.species",
           "fitted/subset_1/cmaes_fit_subset1_rep7.species", "fitted/subset_7/cmaes_fit_subset7_rep5.species")
)

scenario <- "ssp585"
gcm <- "IPSL-CM6A-LR"

for(s in 1:nrow(species_files)){
  
  cat(paste0(species_files[s, "name"], "\n"))
  output_dir_s <- file.path(output_dir, gcm, scenario, species_files[s, "name"])
  
  run_phenofit(species_file = file.path(parameter_dir,  species_files[s, "path"]),
               years = c(2020, 2100), 
               output_dir = output_dir_s, 
               clim_name = gcm, 
               data_dir = file.path(climate_folder, gcm, scenario, "phenofit_format"),
               quiet_mode = TRUE, mem = 10000)
  
  fitness <- read_fitness(output_dir_s)
  saveRDS(fitness, file.path(output_dir, "simulation_output.rds")) 
  
}


