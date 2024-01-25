
# Explore difference across calibrated parameters

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
library(ggplot2)
library(ggbeeswarm)
library(extrafont) 
source(file.path(wd, "scripts", "explore", "load_parameters.R"))


parameters <- frost_par_sum

parameter_plot <- parameters %>%
  ggplot(aes(y = value, x = 1)) +
  geom_violin(fill = 'lightgrey', alpha= 0.2, size = 0.4, colour = 'darkgrey') +
  geom_beeswarm( 
                alpha = 0.5, cex = 2.5, size = 0.4) +
  facet_wrap("var", scales="free_y",
             labeller= label_parsed) +
  # scale_y_continuous(breaks = breaks_fun1) +
  geom_point(aes(y = forward, x= 0.55), fill = 'darkred', size = 1.5, alpha = 0.9, shape = 23, colour="white", stat = "unique") +
  # geom_point(data = data.frame(best = unique(data_leaf_par$best), var_name = unique(data_leaf_par$var_name)),
  #            aes(y = best, x= c(0.55, 0.65, 0.55, 0.55, 0.55, 0.55)), fill = 'darkblue', size = 1.5, 
  #            alpha = 0.9, shape = 23, colour="white", inherit.aes = F) +
  scale_fill_manual(values = c("#002f61", "#004a72", "#00647e", "#007c84", "#009382", "#00aa76", "#3bbd60", "#83c846", "#bdce29", "#f4d004")) +
  scale_color_manual(values = c("#002f61", "#004a72", "#00647e", "#007c84", "#009382", "#00aa76", "#3bbd60", "#83c846", "#bdce29", "#f4d004")) +
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

ggsave(plot = parameter_plot, file = file.path(wd, "scripts", "explore", "graphs", "parameters", "frostsum_parameters_fsylvatica.pdf"))
