# Explore difference across calibrations, future climates

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

# Difference between indices
scenario <- "ssp585"
gcm <- "GFDL-ESM4"
sim_dir <- file.path(wd, "data", "simulations", gcm, scenario)
simulations <- c("expert", "subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7", "subset7_rep5")
variables <- c("DroughtSurvival", "LeafIndex", "FruitIndex", "MaturationIndex", "Fitness")
outputs <- data.frame()
for(v in variables){
  for(c in simulations){
    output <- read_mean_outputvalue(file.path(sim_dir, c), 
                                    years = c(2070:2100), model = "PHENOFIT", output_var = v)
    output$id <- c
    output$var <- v
    outputs <- rbind(outputs, output)
  }
}
outputs$id <- factor(outputs$id, levels=simulations, labels=simulations)
outputs$var <- factor(outputs$var, levels=variables, labels=variables)


output_plots <- ggplot(data = outputs, aes(x = lon, y = lat, fill = value)) +
  facet_grid(var ~ id, switch = "y") +
  geom_raster() +
  scale_fill_gradientn(colours = rainbow(10)) +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))
ggsave(filename=file.path(wd, "scripts", "explore", "graphs", scenario, "contrast_indexmaps_fsylvatica_20702100_gfdl.pdf"), 
       plot=output_plots, height=10, width=14)

# Difference between dates
scenario <- "ssp585"
gcm <- "IPSL-CM6A-LR"
sim_dir <- file.path(wd, "data", "simulations", gcm, scenario)
simulations <- c("expert", "subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7", "subset7_rep5")
variables <- c("LeafDormancyBreakDate", "LeafUnfoldingDate" ,"FloweringDate", "FruitMaturationDate", "LeafSenescenceDate")
outputs <- data.frame()
for(v in variables){
  for(c in simulations){
    output <- read_mean_outputvalue(file.path(sim_dir, c), 
                                    years = c(200:2100), model = "PHENOFIT", output_var = v,
                                    correct_date = TRUE)
    output$id <- c
    output$var <- v
    outputs <- rbind(outputs, output)
  }
}
outputs$id <- factor(outputs$id, levels=simulations, labels=simulations)
outputs$var <- factor(outputs$var, levels=variables, labels=variables)


output_plots <- ggplot(data = outputs, aes(x = lon, y = lat, fill = value)) +
  facet_grid(var ~ id, switch = "y") +
  geom_raster() +
  scale_fill_gradientn(colours = rainbow(10)) +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))
ggsave(filename=file.path(wd, "scripts", "explore", "graphs", scenario, "contrast_datemaps_fsylvatica_20702100.pdf"), 
       plot=output_plots, height=10, width=14)
