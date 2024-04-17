
# Explore difference across calibrated parameters

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(ggplot2)
library(ggbeeswarm)
library(ggh4x)
library(extrafont)
library(cowplot)
source(file.path(wd, "scripts", "explore", "load_parameters.R"))

# colors based on Fcrit of leaf unfolding submodel
col_Fcrit_temp <- leaf_parameters[leaf_parameters$var == "F[crit]^{\n    leaf\n}",]
col_Fcrit_temp <- col_Fcrit_temp[order(col_Fcrit_temp$value),]
col_Fcrit_temp <- split(col_Fcrit_temp, ceiling(seq_along(col_Fcrit_temp$value)/10))
for(i in 1:10){
  col_Fcrit_temp[[i]]['col_group'] <- i
}
col_Fcrit_temp <- do.call("rbind", col_Fcrit_temp) %>% dplyr::select(c(rep, col_group))

# colors based on clusters
clusters <- readRDS(file.path(wd, "data", "metrics", "niv2_clusters.rds")) %>%
  mutate(rep = mod, col_group = clust) %>%
  dplyr::select(rep, col_group)

clusters$col_group <- factor(clusters$col_group)
levels(clusters$col_group) <- list("1" = "1_1", "2" = "1_2", "3" = "3_1", "4" = "2_1", "5" = "2_2")

plot_list <- lapply(c("leaf_flower_parameters", "senescence_parameters",
                      "fruit_parameters", "frost_parameters", "drought_parameters"), function(p){
                        
  if(p == "leaf_flower_parameters"){
    param1 <- leaf_parameters
    param2 <- flower_parameters %>% dplyr::filter(var == "F[crit]^{\n    flower\n}")
    parameters <- rbind(param1, param2)
  }else{
    parameters <- eval(parse(text=p))
  }
                        
  # parameters <- left_join(parameters, col_Fcrit_temp)
  parameters <- left_join(parameters, clusters)
  
  parameter_plot <- parameters %>%
    ggplot(aes(y = value, x = 3)) +
    geom_violin(fill = 'lightgrey', alpha= 0.2, size = 0.4, colour = 'darkgrey', width = 7) +
    geom_beeswarm(
      aes(x = as.numeric(col_group), fill = factor(col_group), col = factor(col_group)),
      alpha = 0.3, cex = 2.5, size = 0.4) +
    geom_boxplot(aes(x = as.numeric(col_group), fill = factor(col_group), col = factor(col_group)), alpha= 0.2, size = 0.4) +
    # scale_y_continuous(breaks = breaks_fun1) +
    geom_point(aes(y = forward, x= 0.55), fill = 'darkred', size = 1.5, alpha = 0.9, shape = 23, colour="white", stat = "unique") +
    # geom_point(data = data.frame(best = unique(data_leaf_par$best), var_name = unique(data_leaf_par$var_name)),
    #            aes(y = best, x= c(0.55, 0.65, 0.55, 0.55, 0.55, 0.55)), fill = 'darkblue', size = 1.5, 
    #            alpha = 0.9, shape = 23, colour="white", inherit.aes = F) +
    # scale_fill_manual(values = c("#002f61", "#004a72", "#00647e", "#007c84", "#009382", "#00aa76", "#3bbd60", "#83c846", "#bdce29", "#f4d004")) +
    # scale_color_manual(values = c("#002f61", "#004a72", "#00647e", "#007c84", "#009382", "#00aa76", "#3bbd60", "#83c846", "#bdce29", "#f4d004")) +
    scale_fill_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                      breaks = c("0", "1", "2", "3", "4", "5")) +
    scale_color_manual(values = c("grey30", "#577590", "#43AA8B", "#ac92eb", '#F9C74F', "#F9844A"),
                       breaks = c("0", "1", "2", "3", "4", "5")) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(family = "Helvetica", size = 9),
          axis.title.y = element_text(family = "Helvetica"),
          strip.text.x = element_text(family = "Helvetica", face = "bold", size = 11),
          axis.ticks.x=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position="none") +
    geom_blank(aes(y = lb)) +
    geom_blank(aes(y = ub)) +
    labs(y = "")
  
  if(p == "frost_parameters"){
    parameter_plot <- parameter_plot + facet_manual("var", scales="free_y", labeller= label_parsed, design = matrix(seq(14), ncol = 7), 
                                                    trim_blank = FALSE, widths = 1)
  }else{
    parameter_plot <- parameter_plot + facet_manual("var", scales="free_y", labeller= label_parsed, design = matrix(seq(7), ncol = 7), 
                                                    trim_blank = FALSE, widths = 1)
  }

})

parameter_plot <- plot_grid(plotlist = plot_list, ncol = 1, align = "hv", axis = 'btlr', rel_heights = c(1,1,1,2,1))

ggsave(plot = parameter_plot, file = file.path(wd, "scripts", "explore", "graphs", "last", "isa", "part1", "parameters_fsylvatica_newclusters.pdf"),
       width = 210, height = 297, unit = "mm")
