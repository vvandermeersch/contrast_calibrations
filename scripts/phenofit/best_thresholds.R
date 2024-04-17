#----------------------------------------------#
# Small script to calculate best threshold/GCM #
#----------------------------------------------#

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(dplyr)
library(AUC)
source(file.path(wd, "scripts", "functions", "compute_best_threshold.R"))
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

# calibrations <- c("expert", paste0("subset",rep(1:2, each = 5),"_rep", 1:5))
calibrations <- c(paste0("partial/pheno_fhmaxmin/subset",rep(1, each = 5),"_rep", 1:5))

#calibrations <- c(paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

# source <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
source <- "ERA5-LAND"
#sim_dir <- file.path("D:/projects/contrast_calibrations", "data", "simulations")
sim_dir <- file.path(wd, "data", "simulations")

output_dir <- file.path(wd, "data", "fit")

# species data
sp_folder <- "D:/species/processed"
sp_presabs <- readRDS(file.path(sp_folder, "fagus_sylvatica/fagus_sylvatica_presabs.rds"))
sp_name <- "fagus_sylvatica"


for(m in source){
  dir.create(file.path(output_dir, m, sp_name))
  for(c in calibrations){
    
    output <- read_mean_outputvalue(file.path(sim_dir, "historical", m, species, c), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = "Fitness")
    names(output) <- c("lat", "lon", "pred")
    compute_best_threshold(output, sp_presabs, sp_name, filename = c, dir = file.path(output_dir, m))
    
  }
}

