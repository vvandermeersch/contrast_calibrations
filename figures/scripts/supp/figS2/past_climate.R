
past_climdiss <- readRDS(file.path("C:/Users/vandermeersch/Documents/CEFE/projects/past_robustness", "data/climate/metrics","HadCM3B_hypervolume_similarity_CRUbaseline.rds"))
names(past_climdiss) <- paste0("clim_hpv_sorensen.",names(past_climdiss))

sorensen_plot <- ggplot() +
  geom_ribbon(data = past_climdiss, aes(x = clim_hpv_sorensen.year, 
                                        ymin = 1-clim_hpv_sorensen.q2.5, ymax = 1-clim_hpv_sorensen.q97.5), 
              fill = "#6867ac", alpha = 0.2) + 
  geom_line(data = past_climdiss,
            aes(x = clim_hpv_sorensen.year, y = 1-clim_hpv_sorensen.mean), col = "#6867ac") +
  coord_cartesian(xlim = c(12000, 1000), 
                  ylim =  c(0, 0.60),
                  clip = "on") +
  scale_x_reverse(breaks = seq(1000,15000, 2000),
                  expand = c(0, 0),
                  name = "Years (BP)") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     breaks = seq(0,0.6, 0.2),
                     name = "Climatic dissimilarity") +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 8),
        axis.title = element_text(colour = "black", size = 9, margin = margin(r = 4.5)),
        legend.position="none", legend.title=element_blank(),
        plot.margin = margin(t = 5.5, b = 5.5, r = 9, l = 5.5))
