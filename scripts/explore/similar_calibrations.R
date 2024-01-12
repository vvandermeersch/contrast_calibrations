
# Identify similar calibration runs with a simple correlation plot

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(data.table)
library(ggplot2)
library(corrplot)
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

sim_dir <- "D:/simulations/phenofit/present/fitted/fagus_sylvatica/paper_data/CMAES"

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
index_corrplot <- corrplot(M, order = 'hclust', addrect = 5, rect.col = 'red', rect.lwd = 5, 
                             col=col1(100), col.lim=c(0.25,1), is.corr = FALSE)
dev.off()

ggsave(filename=, 
       plot=replayPlot(index_corrplot), height=20, width=20)


# Correlation between mean phenological dates
outputs <- c()
for(v in c("LeafUnfoldingDate", "FruitMaturationDate", "LeafSenescenceDate")){
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
pdf(file = file.path(wd, "scripts", "explore", "graphs", "similar_corrdate_fsylvatica.pdf"), width = 20, height = 20)
date_corrplot <- corrplot(M, order = 'hclust', addrect = 5, rect.col = 'red', rect.lwd = 5, 
                           col=col1(100), col.lim=c(0.25,1), is.corr = FALSE)
dev.off()
