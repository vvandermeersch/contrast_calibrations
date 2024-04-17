
# Run paleo simulations on chosen parameter sets

wd <- 'C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations'
source(file.path(wd, "scripts", "functions", "run_phenofit.R"))

climate_folder <- "D:/climate/HadCM3B_60Kyr_Climate/2023_dataset/phenofit_format/dscl_15min"
parameter_dir <- file.path(wd, "data", "parameters")
output_dir <- file.path(wd, "data", "simulations")

species_files <- data.frame(
  name = c(paste0("subset",rep(1:5, each = 10),"_rep", 1:10)),
  path = paste0("fitted/", paste0("subset_",rep(1:5, each = 10)), "/cmaes_fit_subset", rep(1:5, each = 10),"_rep", 1:10, ".species")
)


for(s in 1:nrow(species_files)){
  
  cat(paste0(species_files[s, "name"], "\n"))
  output_dir_s <- file.path(wd, "data", "simulations", "paleo", "7000BP", species_files[s, "name"])
  
  run_phenofit(species_file = file.path(parameter_dir,  species_files[s, "path"]),
               years = c(-7014,-6985), 
               output_dir = output_dir_s, 
               clim_name = "HadCM3B", 
               data_dir = file.path(climate_folder, "7000BP"),
               cd_capsis = "cd/d D:/applications/capsis4", script_name = "ScriptVictor",
               java8 = "cd C:/Program Files/Java/scripts && java8.cmd",
               quiet_mode = TRUE, mem = 10000)
  
  fitness <- read_fitness(output_dir_s)
  saveRDS(fitness, file.path(output_dir, "simulation_output.rds")) 
  
}


