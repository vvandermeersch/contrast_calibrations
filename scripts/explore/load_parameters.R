
# Load calibrated parameters

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(gtools)
library(dplyr)
library("xml2")
library(latex2exp)
source(file.path(wd, "scripts", "functions", "read_species_file.R"))


cal_folder <- 'D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/paper_data'
sim_folder <- 'D:/simulations/phenofit/backward/fagus_sylvatica/paper_data'

forward_species_file <- "C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/forward/Fagus_sylvatica_VVanderMeersch.species"
forward_values <- as.numeric(read_species_file(forward_species_file))

# Bounds
bd_folder <- 'C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/backward/fagus_sylvatica'
species_lb  <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_lb.species')))
species_ub <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_ub.species')))
species_init <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_init.species')))

# Calibrations
species_files <- mixedsort(list.files(path = file.path(cal_folder,"CMAES"), pattern = "\\.species$", full.names = T, recursive = T))

# Load values
best_values <- as.numeric(read_species_file("D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/paper_data/CMAES/subset_4/cmaes_fit_subset4_rep1.species"))
species_parameters <- lapply(species_files, function(x){
  species_values <- data.frame(value = read_species_file(x)) %>% 
    mutate_all(as.numeric)
  subset <- strsplit(x, "/")[[1]][8]
  rep <- strsplit(strsplit(strsplit(x, "/")[[1]][9], "_")[[1]][4], "[.]")[[1]][1]
  species_values$rep <- paste0(subset, "_", rep)
  species_values$var <- rownames(species_values)
  species_values$lb <- species_lb
  species_values$ub <- species_ub
  species_values$init <- species_init
  species_values$forward <- forward_values
  species_values$best <- best_values
  species_values
})
species_parameters <- do.call(rbind, species_parameters)
species_parameters <- na.omit(species_parameters) #remove fixed parameters (bounds = NA)

leaf_parameters <- species_parameters %>%
  dplyr::filter(grepl("leaf", species_parameters$var))
leaf_parameters$var <- factor(leaf_parameters$var, 
                                 labels = c('leaf1' = parse(text=TeX('$t_0$')),
                                            'leaf2' = parse(text=TeX('$T_b$')),
                                            'leaf3' = parse(text=TeX('$d_T$')),
                                            'leaf4' = parse(text=TeX('$T_{50}$')),
                                            'leaf5' = parse(text=TeX('$C_{crit}$')),
                                            'leaf6' = parse(text=TeX('$F_{crit}$'))
                                            ))

flower_parameters <- species_parameters %>%
  dplyr::filter(grepl("flower", species_parameters$var))
flower_parameters$var <- factor(flower_parameters$var, 
                              labels = c('flower1' = parse(text=TeX('$t_0$')),
                                         'flower2' = parse(text=TeX('$T_b$')),
                                         'flower3' = parse(text=TeX('$d_T$')),
                                         'flower4' = parse(text=TeX('$T_{50}$')),
                                         'flower5' = parse(text=TeX('$C_{crit}$')),
                                         'flower6' = parse(text=TeX('$F_{crit}$'))
                              ))

fruit_parameters <- species_parameters %>%
  dplyr::filter(grepl("fruit", species_parameters$var))
fruit_parameters$var <- factor(fruit_parameters$var, 
                              labels = c('fruit1' = parse(text=TeX('$aa$')),
                                         'fruit2' = parse(text=TeX('$bb$')),
                                         'fruit3' = parse(text=TeX('$F_{crit}$')),
                                         'fruit4' = parse(text=TeX('$T_{opt}$')),
                                         'fruit5' = parse(text=TeX('$Mat_{moy}$')),
                                         'fruit6' = parse(text=TeX('$\\sigma$')),
                                         'fruit7' = parse(text=TeX('$pfe_{50}$'))
                              ))

senescence_parameters <- species_parameters %>%
  dplyr::filter(grepl("senes", species_parameters$var))
senescence_parameters$var <- factor(senescence_parameters$var, 
                               labels = c('senes1' = parse(text=TeX('$P_b$')),
                                          'senes2' = parse(text=TeX('$T_b$')),
                                          'senes3' = parse(text=TeX('$\\alpha$')),
                                          'senes4' = parse(text=TeX('$\\beta$')),
                                          'senes5' = parse(text=TeX('$S_{crit}$')),
                                          'senes6' = parse(text=TeX('$\\sigma$'))
                               ))

frost_parameters <- species_parameters %>%
  dplyr::filter(grepl("frost", species_parameters$var))
frost_parameters$var <- factor(frost_parameters$var, levels = paste0('frost',1:12))

frost_par_sum <- frost_parameters %>%
  group_by(rep) %>%
  summarize(
    FHleafmin = value[var == "frost3"],
    FHflowermin = value[var == "frost4"],
    FHleafmax = sum(value[var %in% c("frost7", "frost9")]),
    FHflowermax = sum(value[var %in% c("frost8", "frost10")])
    )
frost_par_sum <- reshape2::melt(frost_par_sum, id.vars=c("rep")) %>%
  rename(var = variable) %>%
  left_join(data.frame(var = c("FHleafmin", "FHflowermin", "FHleafmax", "FHflowermax"),
                       forward = c(-5.3, -5.3, -20, -20),
                       ub = c(0, 0, -7, -7),
                       lb = c(-10, -10, -35, -35)))
frost_par_sum$var <- factor(frost_par_sum$var, levels = c("FHleafmin", "FHflowermin", "FHleafmax", "FHflowermax"))

levels(frost_parameters$var) <- c(parse(text=TeX('$FH^{fr}_{max1}$')), parse(text=TeX('$FH^{fr}_{max2}$')),
                                  parse(text=TeX('$FH^{min}_{leaf}$')), parse(text=TeX('$FH^{min}_{flower}$')),
                                  parse(text=TeX('$Te_1$')), parse(text=TeX('$Te_2$')),
                                  parse(text=TeX('$FHt^{leaf}_{max}$')), parse(text=TeX('$FHt^{flower}_{max}$')),
                                  parse(text=TeX('$FHp^{leaf}_{max}$')), parse(text=TeX('$FHp^{flower}_{max}$')),
                                  parse(text=TeX('$NL_1$')), parse(text=TeX('$NL_2$')))

levels(frost_par_sum$var) <- c(parse(text=TeX('$FH^{min}_{leaf}$')), parse(text=TeX('$FH^{min}_{flower}$')),
                                    parse(text=TeX('$FH^{max}_{leaf}$')), parse(text=TeX('$FH^{max}_{flower}$')))



drought_parameters <- species_parameters %>%
  dplyr::filter(grepl("drought", species_parameters$var))
drought_parameters$var <- factor(drought_parameters$var, 
                               labels = c('drought1' = parse(text=TeX('$pp_{extlow}$')),
                                          'drought2' = parse(text=TeX('$pp_{low}$')),
                                          'drought3' = parse(text=TeX('$pp_{high}$')),
                                          'drought4' = parse(text=TeX('$pp_{exthigh}$'))
                               ))
