# Genouest cluster adaptation

# Working directory
wd <- "/scratch/vvandermeersch/phenofit_calibration_2"

#wd <- 'C:/Users/vandermeersch/Documents/CEFE/thesis/phenofit/Phenofit_from_R/inverse_modelling_cluster'

#library(progressr)

source(paste0(wd, "/functions/cmaes_calibration.R"))
source(paste0(wd, "/functions/auc_test.R"))
source(paste0(wd, "/functions/read_species_file.R"))
source("functions/linear_scaling.R")



################
#### SET-UP ####
################

# Parameter inital values and bounds
param_init <- read_species_file(paste0(wd, "/input_files/fagus_sylvatica/Fagus_sylvatica_init.species"))
param_fixed <- read_species_file(paste0(wd, "/input_files/fagus_sylvatica/Fagus_sylvatica_lb.species"))
param_lb <- read_species_file(paste0(wd, "/input_files/fagus_sylvatica/Fagus_sylvatica_lb.species"))
param_ub <- read_species_file(paste0(wd, "/input_files/fagus_sylvatica/Fagus_sylvatica_ub.species"))
scale_factor <- 10
parameters=parameters=list(param_init=param_init, param_fixed=param_fixed, 
                           param_lb=param_lb, param_ub=param_ub, scale_factor=scale_factor)

# Constraints
constraint_function <- function(x){
   x[6] <= x[7] # Leaf Fcrit <= Flower Fcrit for F. sylvatica
  #x[8] <= x[9] # Leaf Fcrit <= Flower Fcrit for Q. ilex
}

# CMA-ES settings
cmaes_controls=list(sigma=2, mu=10, lambda=20, maxit=300)
cmaes_settings=list(rand_init=FALSE, ipop=FALSE, ipop_factor=NULL)

# Parallelization settings
parallel_settings=list(ncores_runs=1, ncores_eval=20)

# Capsis settings
structure_file <- paste0(wd, "/input_files/Fagus_sylvatica_EvolLett2019.species")
species=list(structure=structure_file, file="fagus_sylvatica")
folders=list(wd=wd, 
             climate="/scratch/vvandermeersch/data/climate/phenofit_format/ERA5Land_fagsyl/1000pres_1000abs/subset_1")
simulation=list(climate_scenario="ERA5LAND", starting_year="1970", ending_year="2000", quiet_mode="true")
capsis_settings=list(java8=NA, cd_java8=NA, cd_capsis="cd /home/genouest/mnhn_cesco/vvandermeersch/capsis4")



################
#### CMA-ES #### 
################

# Donnees occurrence
load("/scratch/vvandermeersch/data/species/fagsyl/1000pres_1000abs/occurrence_subset_1.Rdata")

rep <- 10

for(i in 6:rep){
	
	cat(paste0("Currently doing repetition ", i, " ..."))

	custom_index <- paste0("subset1_rep", i)
	
	cmaes_fit <- cmaes_calibration(nruns=1, obj_function = auc_test, Yob = species_occurrence,
                      parameters=parameters,
                      is_feasible = constraint_function,
                      controls=cmaes_controls,
                      parallel=parallel_settings,
                      cmaes_settings=cmaes_settings,
                      species=species,
                      folders=folders,
                      simulation=simulation,
                      capsis_settings=capsis_settings,
					  index_out = custom_index, seed = i * 1997)

	save(cmaes_fit, file=paste0(wd, "/output_files/cmaes_output_", custom_index, ".Rdata"))
	cat("\n")
}





# For 4 functions evaluations, (4 runs on 2 years with 9892 points), SerialGC and setmem 3500 :
#     - CMA-ES took nearly 259 seconds
#     - parallelized CMA-ES on 2 clusters took nearly 132 seconds
#     - parallelized CMA-ES on 4 clusters took nearly 72 seconds
# It works !


# For 16 functions evaluations, (16 runs on 2 years with 9892 points); SerialGC and setmem 3500 :
#     - parallelized CMA-ES on 8 clusters took nearly 182 seconds

# For 24 functions evaluations, (24 runs on 2 years with 9892 points); SerialGC and setmem 3500 :
#     - parallelized CMA-ES on 12 clusters took nearly 224 seconds


# For 5600 functions evaluations, (5600 runs on 2 years with 9892 points); SerialGC and setmem 3500 :
#     - parallelized CMA-ES on 14 clusters took nearly 12 hours


#mémoire ~44Go, 12 coeurs, lambda 24




