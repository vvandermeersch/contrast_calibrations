
# Identify similar calibration runs 
# with a simple clustering

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"

library(data.table)
library(ggplot2)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land", "fagus_sylvatica")

# First step: clustering on dormancy/unfolding
outputs <- c()
for(v in c("LeafDormancyBreakDate", "LeafUnfoldingDate")){
  outputs_var <- c()
  for(s in 1:10){
    for(r in 1:10){
      output <- read_mean_outputvalue(file.path(sim_dir, paste0("subset",s,"_rep",r)), years = c(1970:2000), 
                                      model = "PHENOFIT", output_var = v, correct_date = TRUE)
      names(output) <- c("lat", "lon", "value")
      outputs_var <- rbind(outputs_var, t(output$value))
    }
  }
  rownames(outputs_var) <- paste0("subset", rep(1:10, each = 10),"_rep", 1:10)
  outputs <- cbind(outputs, outputs_var)
}
M <- cor(t(outputs))
distM <- dist(M)
treeM <- hclust(distM)

dormunfold_clusters <- cutree(treeM, k = 3)
dormunfold_clusters <- data.frame(mod = names(dormunfold_clusters), clust = dormunfold_clusters)

colors = c("#ac92eb", "#4fc1e8", "#a0d568", '#ffce54', "#ed5564")
clus = cutree(treeM, 3)
plot(as.phylo(treeM), type = "fan", tip.color = colors[clus], cex = 0.9)

# Second step: clustering on maturation/senescence, inside previous clusters
matsen_clusters <- data.frame()
for(cl in 1:2){
  
  mods <- dormunfold_clusters[dormunfold_clusters$clust == cl, "mod"]
    
  outputs <- c()
  for(v in c("FruitMaturationDate", "LeafSenescenceDate")){
    outputs_var <- c()
    for(c in mods){
      output <- read_mean_outputvalue(file.path(sim_dir, c), years = c(1970:2000), 
                                      model = "PHENOFIT", output_var = v, correct_date = TRUE)
      names(output) <- c("lat", "lon", "value")
      outputs_var <- rbind(outputs_var, t(output$value))
    }
    rownames(outputs_var) <- mods
    outputs <- cbind(outputs, outputs_var)
  }
  
  M2 <- cor(t(outputs))
  distM2 <- dist(M2)
  treeM2 <- hclust(distM2)
  
  matsen_clusters_cl <- cutree(treeM2, k = 2)
  matsen_clusters <- rbind(matsen_clusters,
                           data.frame(mod = names(matsen_clusters_cl), clust = matsen_clusters_cl))
  
  plot(as.phylo(treeM2), type = "fan", tip.color = colors[matsen_clusters_cl], cex = 0.9)
  
}

# Final 
names(matsen_clusters) <- c("mod", "clust.niv2")
final_clusters <- dormunfold_clusters %>%
  reframe(mod = mod, clust.niv1 = clust) %>%
  left_join(matsen_clusters, join_by(mod)) %>%
  mutate(clust.niv2 = ifelse(is.na(clust.niv2), 1, clust.niv2), 
         clust = paste(clust.niv1, clust.niv2, sep = "_"))

saveRDS(final_clusters, file.path(wd, "data", "metrics", "niv2_clusters.rds"))



library("ggdendro")

dend <- as.dendrogram(treeM)
dend_data <- dendro_data(dend, type = "rectangle")

dend_data$labels <- dend_data$labels %>%
  left_join(final_clusters, by = c("label" = "mod"))

clusters_2steps <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label, col = clust),
            hjust = 1.05, angle = 60, size = 4)+
  ylim(-0.4, 3.2) +
  scale_color_manual(values = c("#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                     breaks = c("1_1", "1_2", "3_1", "2_1", "2_2")) +
  theme_void() + theme(legend.position = 'none')

ggsave(clusters_2steps, filename = file.path(wd, "scripts/explore/graphs/last/isa/part1", "newclusters_2steps.pdf"),
       width = 297, height = 210, units = "mm")
