
# Explore the difference between MaturationIndex
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"

# Difference between indices, by year
sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"
calibrations <- c("subset5_rep4", "subset7_rep5")
variables <- c("MaturationIndex")
outputs <- data.frame()
for(v in variables){
  for(r in calibrations){
    for(y in 1970:2000){
      output <- read_mean_outputvalue(file.path(sim_dir, r), 
                                      years = y, model = "PHENOFIT", output_var = v)
      output$id <- r
      output$var <- v
      output$year <- y
      outputs <- rbind(outputs, output)
    }
  }
}

output_plots <- ggplot(data = outputs, aes(x = lon, y = lat, fill = value)) +
  facet_grid(year ~ id, switch = "y") +
  geom_raster() +
  scale_fill_gradientn(colours = hcl.colors(10, palette = "Plasma")) +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))
ggsave(filename=file.path(wd, "scripts", "explore", "graphs", "unexpldiff_matindex.pdf"), 
       plot=output_plots, height=46, width=5)

# Difference between dates, by year
sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"
calibrations <- c("subset5_rep4", "subset7_rep5")
variables <- c("FruitMaturationDate")
outputs <- data.frame()
for(v in variables){
  for(r in calibrations){
    for(y in 1970:2000){
      output <- read_mean_outputvalue(file.path(sim_dir, r), 
                                      years = y, model = "PHENOFIT", output_var = v,
                                      correct_date = TRUE)
      output$id <- r
      output$var <- v
      output$year <- y
      outputs <- rbind(outputs, output)
    }
  }
}

output_plots <- ggplot(data = outputs, aes(x = lon, y = lat, fill = value)) +
  facet_grid(year ~ id, switch = "y") +
  geom_raster() +
  scale_fill_gradientn(colours = hcl.colors(10, palette = "Plasma")) +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))
ggsave(filename=file.path(wd, "scripts", "explore", "graphs", "unexpldiff_matdate.pdf"), 
       plot=output_plots, height=46, width=5)
