# Partial calibrations
library(gtools)
library(dplyr)
library("xml2")
library(latex2exp)
source(file.path(wd, "scripts", "functions", "read_species_file.R"))
species <- "fagus_sylvatica"

# Bounds
bd_folder <- 'C:/Users/vandermeersch/Dropbox/These_Victor/Phenofit4/species/backward/fagus_sylvatica'
species_lb  <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_lb.species')))
species_ub <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_ub.species')))
species_init <- as.numeric(read_species_file(file.path(bd_folder, 'Fagus_sylvatica_init.species')))

# Load parameter values
parameters <- data.frame()
{
## Partial: FHmin and Maturation ----
par_dir <- "D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/partial"
cal <- paste0("subset_",rep(1:2, each = 5),"/cmaes_fit_subset", rep(1:2, each = 5),"_rep", 1:5, ".species")
species_parameters <- lapply(cal, function(x){
  species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
    mutate_all(as.numeric)
  species_values$type <- "1fhmin_mat"
  species_values$rep <- x
  species_values$var <- rownames(species_values)
  species_values$lb <- species_lb
  species_values$ub <- species_ub
  species_values$init <- species_init
  species_values
})
species_parameters <- do.call(rbind, species_parameters)
species_parameters <- na.omit(species_parameters) #remove fixed parameters (bounds = NA)
parameters <- rbind(parameters, species_parameters)
## Partial: FHmin/max and Maturation ----
par_dir <- "D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/partial/relmax"
cal <- paste0("subset_",rep(1:1, each = 5),"/cmaes_fit_subset", rep(1:1, each = 5),"_rep", 1:5, ".species")
species_parameters <- lapply(cal, function(x){
  species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
    mutate_all(as.numeric)
  species_values$type <- "2fh_mat"
  species_values$rep <- x
  species_values$var <- rownames(species_values)
  species_values$lb <- species_lb
  species_values$ub <- species_ub
  species_values$init <- species_init
  species_values
})
species_parameters <- do.call(rbind, species_parameters)
species_parameters <- na.omit(species_parameters) #remove fixed parameters (bounds = NA)
parameters <- rbind(parameters, species_parameters)
## Partial: FHmin/max and leaf phenology ----
par_dir <- "D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/partial/pheno_fhmaxmin"
cal <- paste0("subset_",rep(1:1, each = 5),"/cmaes_fit_subset", rep(1:1, each = 5),"_rep", 1:5, ".species")
species_parameters <- lapply(cal, function(x){
  species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
    mutate_all(as.numeric)
  species_values$type <- "3fh_pheno"
  species_values$rep <- x
  species_values$var <- rownames(species_values)
  species_values$lb <- species_lb
  species_values$ub <- species_ub
  species_values$init <- species_init
  species_values
})
species_parameters <- do.call(rbind, species_parameters)
species_parameters <- na.omit(species_parameters) #remove fixed parameters (bounds = NA)
parameters <- rbind(parameters, species_parameters)
}

# Process frost parameters
frost_parameters <- parameters %>%
  dplyr::filter(grepl("frost", parameters$var))
frost_parameters$var <- factor(frost_parameters$var, levels = paste0('frost',1:12))
frost_par_sum <- frost_parameters %>%
  group_by(type, rep) %>%
  summarize(
    FHleafmin = value[var == "frost3"],
    FHflowermin = value[var == "frost4"],
    FHleafmax = sum(value[var %in% c("frost7", "frost9")]),
    FHflowermax = sum(value[var %in% c("frost8", "frost10")])
  )
frost_par_sum <- reshape2::melt(frost_par_sum, id.vars=c("type","rep")) %>%
  rename(var = variable) %>%
  rbind(data.frame(type = rep("0expert", 4),
                   rep = rep("0expert", 4),
                   var = c("FHleafmin", "FHflowermin", "FHleafmax", "FHflowermax"),
                   value = c(-5.3, -5.3, -20, -20))) %>%
  left_join(data.frame(var = c("FHleafmin", "FHflowermin", "FHleafmax", "FHflowermax"),
                       ub = c(0, 0, -7, -7),
                       lb = c(-10, -10, -35, -35)))
frost_par_sum$var <- factor(frost_par_sum$var, levels = c("FHleafmin", "FHflowermin", "FHleafmax", "FHflowermax"))
levels(frost_par_sum$var) <- c(parse(text=TeX('$FH^{min}_{leaf}$')), parse(text=TeX('$FH^{min}_{flower}$')),
                               parse(text=TeX('$FH^{max}_{leaf}$')), parse(text=TeX('$FH^{max}_{flower}$')))

# Process leaf parameters
leaf_parameters <- parameters %>%
  dplyr::filter(grepl("leaf", parameters$var)) %>%
  dplyr::select(type, rep, var, value) %>%
  rbind(data.frame(type = rep("0expert", 6),
                   rep = rep("0expert", 6),
                   var = c("leaf1", "leaf2", "leaf3", "leaf4", "leaf5", "leaf6"),
                   value = c(-62, 12, -0.08, 13, 73.7, 37.3))) %>%
  left_join(data.frame(var = c("leaf1", "leaf2", "leaf3", "leaf4", "leaf5", "leaf6"),
                       ub = c(0, 12, -0.05, 20, 150, 120),
                       lb = c(-122, -5, -2, 5, 1, 1)))
leaf_parameters$var <- factor(leaf_parameters$var, 
                              labels = c('leaf1' = parse(text=TeX('$t^{leaf}_0$')),
                                         'leaf2' = parse(text=TeX('$T^{leaf}_b$')),
                                         'leaf3' = parse(text=TeX('$d^{leaf}_T$')),
                                         'leaf4' = parse(text=TeX('$T^{leaf}_{50}$')),
                                         'leaf5' = parse(text=TeX('$C^{leaf}_{crit}$')),
                                         'leaf6' = parse(text=TeX('$F^{leaf}_{crit}$'))))

# Process fruit parameters
fruit_parameters <- parameters %>%
  dplyr::filter(grepl("fruit", parameters$var)) %>%
  dplyr::select(type, rep, var, value) %>%
  rbind(data.frame(type = rep("0expert", 7),
                   rep = rep("0expert", 7),
                   var = c("fruitmat1", "fruitmat2", "fruitmat3", "fruitmat4", "fruitmat5", "fruitmat6", "fruitmat7"),
                   value = c(-0.2, 14.7297, 9, 5.002, 100, 3.7, 0.6))) %>%
  left_join(data.frame(var = c("fruitmat1", "fruitmat2", "fruitmat3", "fruitmat4", "fruitmat5", "fruitmat6", "fruitmat7"),
                       ub = c(-0.05, 20, 60, 20, 120, 10, 0.80),
                       lb = c(-2, 5, 1, 5, 1, 0.01, 0.20)))
fruit_parameters$var <- factor(fruit_parameters$var, 
                               labels = c('fruit1' = parse(text=TeX('$aa$')),
                                          'fruit2' = parse(text=TeX('$bb$')),
                                          'fruit3' = parse(text=TeX('$F^{fruit}_{crit}$')),
                                          'fruit4' = parse(text=TeX('$T_{opt}$')),
                                          'fruit5' = parse(text=TeX('$Mat_{moy}$')),
                                          'fruit6' = parse(text=TeX('$\\sigma^{fruit}$')),
                                          'fruit7' = parse(text=TeX('$pfe_{50}$'))))

# Combine
param_of_interests <- rbind(frost_par_sum, 
                            leaf_parameters, 
                            fruit_parameters)

param_of_interests$col_group <- factor(param_of_interests$type)
levels(param_of_interests$col_group) <- list("1" = "0expert", "2" = "1fhmin_mat", "3" = "2fh_mat", "4" = "3fh_pheno")

data_distrib <- param_of_interests %>%
  na.omit() %>% 
  dplyr::filter(var %in% c("FH[leaf]^{\n    min\n}", "FH[leaf]^{\n    max\n}", "Mat[moy]",
                           "T[b]^{\n    leaf\n}", "C[crit]^{\n    leaf\n}", "F[crit]^{\n    leaf\n}")) %>% 
  group_by(col_group, var) %>%
  mutate(fixed = (n_distinct(value)== 1)) %>%
  ungroup()          

param_distrib <- 
  ggplot() +
  facet_wrap(~ var, scales="free_y", labeller= label_parsed, nrow = 2) + 
  # geom_violin(fill = 'lightgrey', alpha= 0.2, size = 0.4, colour = 'darkgrey', width = 7) +
  geom_point(
    data = data_distrib %>% dplyr::filter(fixed == TRUE) %>% dplyr::select(col_group, var, value) %>% unique(),
    aes(y = value, x = as.numeric(col_group), fill = factor(col_group), col = factor(col_group)),
    shape = 23) +
  geom_beeswarm(
    data = data_distrib %>% dplyr::filter(fixed == FALSE),
    aes(y = value, x = as.numeric(col_group), fill = factor(col_group), col = factor(col_group)),
    alpha = 0.3, cex = 2.5, size = 0.4) +
  geom_boxplot(
    data = data_distrib %>% dplyr::filter(fixed == FALSE),
    aes(y = value, x = as.numeric(col_group), fill = factor(col_group), col = factor(col_group)), 
    alpha= 0.2, size = 0.4) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8),
        strip.text.x = element_text(face = "bold", size = 10),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(color = "grey", linewidth = 0.3),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "grey", fill = NA, linewidth = 0.3),
        legend.position="none", plot.margin = margin(b = 15, r = 5.5, l = 0, t = 0)) +
  labs(y = "") +
  ggh4x::facetted_pos_scales(y = list(
    var == "FH[leaf]^{\n    min\n}" ~ scale_y_continuous(limits = c(-10, 0), breaks = c(-10, -5, 0), expand = c(0,0)),
    var == "FH[leaf]^{\n    max\n}" ~ scale_y_continuous(limits = c(-35, 0), breaks = c(-35, -17.5, 0), expand = c(0,0)),
    var == "T[b]^{\n    leaf\n}" ~ scale_y_continuous(limits = c(-5, 12), breaks = c(-5, 3.5, 12), expand = c(0,0)),
    var == "Mat[moy]" ~ scale_y_continuous(limits = c(1, 120), breaks = c(1, 60, 120), expand = c(0,0)),
    var == "C[crit]^{\n    leaf\n}" ~ scale_y_continuous(limits = c(1, 150), breaks = c(1, 75, 150), expand = c(0,0)),
    var == "F[crit]^{\n    leaf\n}" ~ scale_y_continuous(limits = c(1, 120), breaks = c(1, 60, 120), expand = c(0,0))
  )) + coord_cartesian(clip = "off") +
  scale_fill_manual(values = c("#ff595e", "#ffca3a", "#8ac926", "#1982c4")) +
  scale_color_manual(values = c("#ff595e", "#ffca3a", "#8ac926", "#1982c4"))

test <- plot_grid(var_contrib, param_distrib)
