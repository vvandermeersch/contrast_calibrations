#----------------------------------------------#
# Small script to calculate best threshold/GCM #
#----------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(dplyr)
library(AUC)
library(terra)
source(file.path(wd, "scripts", "functions", "compute_best_threshold.R"))
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

# calibrations <- c(paste0("subset",rep(1:2, each = 5),"_rep", 1:5))
calibrations <- c(paste0("partial/frost_flo_mat/subset",rep(1:2, each = 5),"_rep", 1:5))

# source <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
source <- "ERA5-LAND"
sim_dir <- file.path(wd, "data", "simulations")
sim_dir <- "D:/simulations/phenofit/present/fitted"

output_dir <- file.path(wd, "data", "fit")

# species data
sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fraxinus_excelsior/fraxinus_excelsior_presabs.rds"))
sp_name <- "fraxinus_excelsior"


for(m in source){
  dir.create(file.path(output_dir, m, sp_name))
  for(c in calibrations){
    
    # simpath <- file.path(sim_dir, "historical", m, sp_name, c)
    #simpath <- file.path(sim_dir, sp_name, m, "1970_2000", c)
    simpath <- file.path(sim_dir, sp_name, c)
    
    survival <- rast(read_mean_outputvalue(simpath, 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Survival")[c(2,1,3)])
    fruit <- rast(read_mean_outputvalue(simpath, 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "FruitIndex")[c(2,1,3)])
    maturation <- rast(read_mean_outputvalue(simpath, 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "MaturationIndex", 
                                    correct_mat = TRUE)[c(2,1,3)])
    fitness <- survival*fruit*maturation
    plot(fitness)
    output <- as.data.frame(fitness, xy = TRUE)
    names(output) <- c("lon", "lat", "pred")
    output$lat <- round(output$lat, 1)
    output$lon <- round(output$lon, 1)
    
    compute_best_threshold(output, sp_presabs, sp_name, filename = c, dir = file.path(output_dir, m))
    
  }
}

