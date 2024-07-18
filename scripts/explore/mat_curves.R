
## Quercus_robur
par_dir <- "D:/calibrations/phenofit/quercus_robur/1000pres_1000abs/partial"
cal <- paste0("subset_",rep(1:2, each = 5),"/cmaes_fit_subset", rep(1:2, each = 5),"_rep", 1:5, ".species")
species_parameters <- lapply(cal, function(x){
  species_values <- data.frame(value = read_species_file(file.path(par_dir,x))) %>% 
    mutate_all(as.numeric)
  species_values$species <- "atop(NA, atop(textstyle('Quercus'), textstyle('robur')))"
  species_values$rep <- x
  species_values$var <- rownames(species_values)
  species_values
})
parameters <- do.call(rbind, species_parameters)


# Process fruit maturation parameters
fruit_parameters <- parameters %>%
  dplyr::filter(grepl("fruit", parameters$var))

fruit_parameters$var <- factor(fruit_parameters$var, levels = paste0('fruitmat',1:8))

fruit_parameters <- fruit_parameters %>% 
  pivot_wider(names_from = var, values_from = value) %>%
  dplyr::rename(Topt = fruitmat4, Matmoy = fruitmat5) %>%
  dplyr::filter(species == "atop(NA, atop(textstyle('Quercus'), textstyle('robur')))") %>%
  dplyr::select(species, rep, Topt, Matmoy)

fruit_parameters <- rbind(fruit_parameters,
                          c("atop(NA, atop(textstyle('Quercus'), textstyle('robur')))", "expert", 5, 48))

# Wang and Engel from Lizzie script
matcurves <- data.frame()
for(c in unique(fruit_parameters$rep)){
  Tmin <- -40
  Tmax <- 40
  Topt <- as.numeric(fruit_parameters[fruit_parameters$rep == c, "Topt"])
  
  alpha  <- log(2)/(log((Tmax-Tmin)/(Topt-Tmin)))
  curvex <- seq(-20,30, length.out=10000)
  curve <- ((2*(curvex-Tmin)^alpha) * ((Topt-Tmin)^alpha) - (curvex-Tmin)^(2*alpha))/
    ((Topt-Tmin)^(2*alpha))
  
  matcurves <- rbind(matcurves,
                     data.frame(x = curvex, y = curve, mod = c, type = ifelse(c == "expert", "expert", "fitted")))
  
}

matcurves %>%
  ggplot(aes(x = x, y = y, color = type)) +
  # facet_wrap(~ mod) + 
  geom_line(linewidth = 0.5)
