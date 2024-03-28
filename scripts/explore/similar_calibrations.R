
# Identify similar calibration runs with a simple correlation plot

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(corrplot)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land", "fagus_sylvatica")

# Correlation between mean index (30 years)
outputs <- c()
for(v in c("LeafIndex", "FruitIndex", "MaturationIndex", "DroughtSurvival")){
  outputs_var <- c()
  for(s in 1:10){
    for(r in 1:10){
      output <- read_mean_outputvalue(file.path(sim_dir, paste0("subset",s,"_rep",r)), 
                                      years = c(1970:2000), model = "PHENOFIT", output_var = v)
      outputs_var <- rbind(outputs_var, t(output$value))
    }
  }
  rownames(outputs_var) <- paste0("subset", rep(1:10, each = 10),"_rep", 1:10)
  outputs <- cbind(outputs, outputs_var)
}
M <- cor(t(outputs))
col1 <- colorRampPalette(c('red','yellow','green','blue'))
pdf(file = file.path(wd, "scripts", "explore", "graphs", "similar_corrindex_fsylvatica.pdf"), width = 20, height = 20)
index_corrplot <- corrplot(M, order = 'hclust', addrect = 6, rect.col = 'red', rect.lwd = 5, 
                             col=col1(100), col.lim=c(0.25,1), is.corr = FALSE)
dev.off()

ggsave(filename=, 
       plot=replayPlot(index_corrplot), height=20, width=20)

dist <- dist(M)
tree <- hclust(dist)
index_clusters <- cutree(tree, k = 6)
index_clusters <- data.frame(mod = names(index_clusters), clust = index_clusters)
saveRDS(index_clusters, file.path(wd, "data", "metrics", "index_cluster_calibrations.rds"))

# Correlation between mean phenological dates
outputs <- c()
for(v in c("LeafDormancyBreakDate", "LeafUnfoldingDate", "FruitMaturationDate")){
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
col1 <- colorRampPalette(c('red','yellow','green','blue'))
pdf(file = file.path(wd, "scripts", "explore", "graphs", "last", "cluster_dates_woSen.pdf"), width = 20, height = 20)
date_corrplot <- corrplot(M, order = 'hclust', addrect = 5, rect.col = 'red', rect.lwd = 5, 
                           col=col1(100),  is.corr = FALSE)
dev.off()

dist <- dist(M)
tree <- hclust(dist)
date_clusters <- cutree(tree, k = c(2,3,4,5,6,7,8))
date_clusters <- data.frame(mod = rownames(date_clusters), clust = date_clusters)
saveRDS(date_clusters, file.path(wd, "data", "metrics", "date_clusters_woSen.rds"))




colors = c("#ac92eb", "#4fc1e8", "#a0d568", '#ffce54', "#ed5564")
clus = cutree(tree, 5)
plot(as.phylo(tree), type = "fan", tip.color = colors[clus], cex = 0.9)
