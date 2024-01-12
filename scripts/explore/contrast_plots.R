
# Explore difference across calibrations

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

# Difference between indices
## Inverse calibrations
sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"
calibrations <- c("subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7", "subset7_rep5")
variables <- c("DroughtSurvival", "LeafIndex", "FruitIndex", "MaturationIndex", "Fitness")
outputs <- data.frame()
for(v in variables){
  for(r in calibrations){
    output <- read_mean_outputvalue(file.path(sim_dir, r), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = v)
    output$id <- r
    output$var <- v
    outputs <- rbind(outputs, output)
  }
}

## Add expert simulation
expert_outputs <- data.frame()
for(v in variables){
  output <- read_mean_outputvalue("D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch", 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = v)
  output$id <- "expert"
  output$var <- v
  expert_outputs <- rbind(expert_outputs, output)
}
outputs <- rbind(outputs, expert_outputs)
outputs$id <- factor(outputs$id, levels=c("expert", calibrations), labels=c("expert", calibrations))
outputs$var <- factor(outputs$var, levels=variables, labels=variables)

output_plots <- ggplot(data = outputs, aes(x = lon, y = lat, fill = value)) +
  facet_grid(var ~ id, switch = "y") +
  geom_raster() +
  scale_fill_gradientn(colours = rainbow(10)) +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))
ggsave(filename=file.path(wd, "scripts", "explore", "graphs", "contrast_indexmaps_fsylvatica.pdf"), 
       plot=output_plots, height=10, width=14)


# Difference between dates
## Inverse calibrations
sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"
calibrations <- c("subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7", "subset7_rep5")
variables <- c("LeafDormancyBreakDate", "LeafUnfoldingDate" ,"FloweringDate", "FruitMaturationDate", "LeafSenescenceDate")
outputs <- data.frame()
for(v in variables){
  for(r in calibrations){
    output <- read_mean_outputvalue(file.path(sim_dir, r), 
                                    years = c(1970:2000), model = "PHENOFIT", output_var = v, correct_date = TRUE)
    output$id <- r
    output$var <- v
    outputs <- rbind(outputs, output)
  }
}

## Add expert simulation
expert_outputs <- data.frame()
for(v in variables){
  output <- read_mean_outputvalue("D:/simulations/phenofit/present/expert/fagus_sylvatica/VVanderMeersch", 
                                  years = c(1970:2000), model = "PHENOFIT", output_var = v, correct_date = TRUE)
  output$id <- "expert"
  output$var <- v
  expert_outputs <- rbind(expert_outputs, output)
}
outputs <- rbind(outputs, expert_outputs)
outputs$id <- factor(outputs$id, levels=c("expert", calibrations), labels=c("expert", calibrations))
outputs$var <- factor(outputs$var, levels=variables, labels=variables)

output_plots <- ggplot(data = outputs, aes(x = lon, y = lat, fill = value)) +
  facet_grid(var ~ id, switch = "y") +
  geom_raster() +
  scale_fill_gradientn(colours = rainbow(10)) +
  theme_void() +
  theme(strip.text.y.left = element_text(angle = 90))
ggsave(filename=file.path(wd, "scripts", "explore", "graphs", "contrast_datemaps_fsylvatica.pdf"), 
       plot=output_plots, height=10, width=14)
