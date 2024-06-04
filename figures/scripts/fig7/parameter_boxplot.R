


# Load parameter values
parameters <- data.frame()
{
  
  ## Abies alba
  par_dir <- "D:/calibrations/phenofit/abies_alba/1000pres_1000abs/partial/drought_frost"
  cal <- paste0("subset_",rep(1, each = 5),"/cmaes_fit_subset", rep(1, each = 5),"_rep", 1:5, ".species")
  species_parameters <- lapply(cal, function(x){
    species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
      mutate_all(as.numeric)
    species_values$species <- "atop(NA, atop(textstyle('Abies'), textstyle('alba')))"
    species_values$rep <- x
    species_values$var <- rownames(species_values)
    species_values
  })
  species_parameters <- do.call(rbind, species_parameters)
  parameters <- rbind(parameters, species_parameters)
  
  ## Betula pendula
  par_dir <- "D:/calibrations/phenofit/betula_pendula/1000pres_1000abs/partial/frost_flo_mat"
  cal <- paste0("subset_",rep(1, each = 5),"/cmaes_fit_subset", rep(1, each = 5),"_rep", 1:5, ".species")
  species_parameters <- lapply(cal, function(x){
    species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
      mutate_all(as.numeric)
    species_values$species <- "atop(NA, atop(textstyle('Betula'), textstyle('pendula')))"
    species_values$rep <- x
    species_values$var <- rownames(species_values)
    species_values
  })
  species_parameters <- do.call(rbind, species_parameters)
  parameters <- rbind(parameters, species_parameters)
  
  ## Fagus sylvatica
  par_dir <- "D:/calibrations/phenofit/fagus_sylvatica/1000pres_1000abs/partial/relmax"
  cal <- paste0("subset_",rep(1, each = 5),"/cmaes_fit_subset", rep(1, each = 5),"_rep", 1:5, ".species")
  species_parameters <- lapply(cal, function(x){
    species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
      mutate_all(as.numeric)
    species_values$species <- "atop(NA, atop(textstyle('Fagus'), textstyle('sylvatica')))"
    species_values$rep <- x
    species_values$var <- rownames(species_values)
    species_values
  })
  species_parameters <- do.call(rbind, species_parameters)
  parameters <- rbind(parameters, species_parameters)
  
}

# Process frost parameters
frost_parameters <- parameters %>%
  dplyr::filter(grepl("frost", parameters$var))

frost_parameters$var <- factor(frost_parameters$var, levels = paste0('frost',1:12))
frost_par_sum <- frost_parameters %>%
  group_by(species, rep) %>%
  summarize(
    FHleafmin = value[var == "frost3"],
    FHflowermin = value[var == "frost4"],
    FHleafmax = sum(value[var %in% c("frost7", "frost9")]),
    FHflowermax = sum(value[var %in% c("frost8", "frost10")])
  )

frost_par_sum <- reshape2::melt(frost_par_sum, id.vars=c("species","rep")) %>%
  rename(var = variable)

frost_par_sum$var <- factor(frost_par_sum$var, levels = c("FHleafmin", "FHflowermin", "FHleafmax", "FHflowermax"))

levels(frost_par_sum$var) <- c(parse(text=TeX('$FH^{min}_{leaf}$')), parse(text=TeX('$FH^{min}_{flower}$')),
                               parse(text=TeX('$FH^{max}_{leaf}$')), parse(text=TeX('$FH^{max}_{flower}$')))

# frost_par_sum <- reshape2::melt(frost_par_sum, id.vars=c("species","rep")) %>%
#   rename(var = variable) %>%
#   mutate(var = ifelse(var %in% c("FHleafmin", "FHleafmax"), "FHmin", "FHmax"))
# 
# frost_par_sum$var <- factor(frost_par_sum$var, levels = c("FHmax", "FHmin"))
# frost_par_sum$var <- factor(frost_par_sum$var, levels = c("FHmin", "FHmax"))
# 
# levels(frost_par_sum$var) <- c(parse(text=TeX('$FH^{min}$')), parse(text=TeX('$FH^{max}$')))

expert_parameters <-
  data.frame(
    species = rep(unique(frost_par_sum$species), 2),
    var = rep(c("FH[leaf]^{\n    min\n}", "FH[flower]^{\n    min\n}"), each = 3),
    value = c(-3, -3.2, -5.3, -3, -3, -5.3)
  )

parameter_boxplot <- frost_par_sum %>%
  dplyr::filter(var %in% c("FH[leaf]^{\n    min\n}", "FH[flower]^{\n    min\n}")) %>%
  ggplot() +
  facet_grid(var ~ species, scales="free_y", labeller = label_parsed, switch="y") + 
  geom_beeswarm(
    aes(y = value, x = 1.5), col = "#f9c74f", fill = "#f9c74f",
    alpha = 0.5, cex = 2.5, size = 0.6) +
  geom_boxplot(
    aes(y = value, x = 1.5), col = "#f9c74f", fill = "#f9c74f",
    alpha= 0.5, linewidth = 0.4, outliers = FALSE) +
  geom_point(data = expert_parameters, aes(y = value, x = 1.5),
             shape = 23, col = "#577590", fill = "#577590", alpha = 0.5,
             stroke = 1,  size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7),
        strip.text.x = element_text(face = "bold", size = 9),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(color = "grey85", linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
        legend.position="none", plot.margin = margin(b = 5.5, r = 5.5, l = -5, t = -5),
        ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
        strip.placement = "outside", 
        strip.text.y.left = element_text(size = 8, color = "black", margin = margin(t=0,b=0,l=0,r=-1)),
        strip.text.x.top  = element_text(size = 7, color = "grey30", margin = margin(t=5.3,b=3,l=4.4,r=4.4)),
        panel.spacing.y = unit(10, "pt")) +
  labs(y = "") +
  ggh4x::facetted_pos_scales(y = list(
    var == "FH[leaf]^{\n    min\n}" ~ scale_y_continuous(limits = c(-10, 0), breaks = c(-10, -5, 0), expand = c(0,0), minor_breaks= c(-2.5, -7.5), guide = "axis_minor"),
    var == "FH[flower]^{\n    min\n}" ~ scale_y_continuous(limits = c(-10, 0), breaks = c(-10, -5, 0), expand = c(0,0), minor_breaks= c(-2.5, -7.5), guide = "axis_minor"),
    var == "FH[leaf]^{\n    max\n}" ~ scale_y_continuous(limits = c(-35, 0), breaks = c(-35, -17.5, 0), expand = c(0,0), minor_breaks= c(-8.75, -26.25), guide = "axis_minor"),
    var == "T[b]^{\n    leaf\n}" ~ scale_y_continuous(limits = c(-5, 12), breaks = c(-5, 3.5, 12), expand = c(0,0), minor_breaks= c(7.75, -0.75), guide = "axis_minor"),
    var == "Mat[moy]" ~ scale_y_continuous(limits = c(1, 120), breaks = c(1, 60, 120), expand = c(0,0), minor_breaks= c(30.5, 90), guide = "axis_minor"),
    var == "C[crit]^{\n    leaf\n}" ~ scale_y_continuous(limits = c(1, 150), breaks = c(1, 75, 150), expand = c(0,0), minor_breaks= c(38, 112.5), guide = "axis_minor"),
    var == "F[crit]^{\n    leaf\n}" ~ scale_y_continuous(limits = c(1, 120), breaks = c(1, 60, 120), expand = c(0,0), minor_breaks= c(30.5, 90), guide = "axis_minor")
  )) + 
  coord_cartesian(clip = "off", xlim = c(0.5, 2.5))
