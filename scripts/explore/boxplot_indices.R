
species <- data.frame(
  name = c("fagus_sylvatica"),
  occ_path = c("D:/species/processed/fagus_sylvatica/fagus_sylvatica_presabs.rds")
)

models <- c()
models_fs <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))
sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")

indices_df <- data.frame()
for(s in 1:nrow(species)){
  
  models_temp <- models
  if(species[s, "name"] == "fagus_sylvatica"){
    models_temp <- models_fs
  }
  
  for(m in models_temp){
    
    sim_path <- file.path(sim_dir, species[s, "name"], m)
    carbsurv <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                           output_var = "CarbonSurvival")[,c(2,1,3)])
    carbsurv <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                           output_var = "CarbonSurvival")[,c(2,1,3)])
    drgsurv <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                          output_var = "DroughtSurvival")[,c(2,1,3)])
    leafind <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                          output_var = "LeafIndex")[,c(2,1,3)])
    fruitind <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                           output_var = "FruitIndex")[,c(2,1,3)])
    matind <- rast(read_mean_outputvalue(sim_path, years = c(1970:2000), model = "PHENOFIT", 
                                         output_var = "MaturationIndex")[,c(2,1,3)])
    indices <- c(carbsurv, drgsurv, leafind, fruitind, matind)
    names(indices) <- c("carbsurv", "drgsurv", "leafind", "fruitind", "matind")
    
    presence <- rast(readRDS(species[s, "occ_path"]))
    absence <- presence
    presence[presence == 0] <- NA
    absence[absence == 1] <- NA
    
    ind_pres <- mask(crop(indices, presence), presence) %>%
      as.data.frame()
    ind_abs <- mask(crop(indices, absence), absence) %>%
      as.data.frame()
    
    indices_df <-rbind(
      indices_df,
      data.frame(mod = m, species = species[s, "name"], obs = c("presence"), ind_pres),
      data.frame(mod = m, species = species[s, "name"], obs = c("absence"), ind_abs))
    
  }
  
}
gc()


index_clusters <- readRDS(file.path(wd, "data", "metrics", "index_cluster_calibrations.rds"))

indices_df <- indices_df %>%
  left_join(index_clusters)
  
indices_df$clust <- ifelse(indices_df$mod == "expert", 0, indices_df$clust)

indices_df2 <- indices_df %>%
  tidyr::pivot_longer(cols = c("carbsurv", "drgsurv", "leafind", "fruitind", "matind"),
                      names_to = "var", values_to = "index")

boxplot_fagus <- ggplot(data = indices_df2) +
  facet_grid(var ~ obs, switch = "y") +
  geom_boxplot(aes(x = paste0(clust,mod), y = index, fill = as.factor(clust), color = as.factor(clust)), outlier.shape = NA, alpha = 0.3) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank(), axis.title = element_blank(),
        legend.position = "none", strip.text.x = element_text(size = 11))
ggsave(boxplot_fagus, filename = file.path(wd, "scripts", "explore", "graphs", "last", "boxplots_index.pdf"),
       height = 21, width = 29.7, units = "cm")


           