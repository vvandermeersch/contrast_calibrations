data <- data.frame(
  x = c(rep(-0.3,3), rep(1,5), rep(2.3,5)),
  y = c(rep(2.5,3), 1.5, 1.5, 3, 3, 4, c(1,2,2.5,3.5,4)),
  count = c(rep(NA,8), 9, 56, 27, 7, 1),
  col = c(rep("grey30",8), "#577590", "#43AA8B", '#F9C74F', "#F9844A", "#ac92eb"),
  shape =  c(rep(16,8), rep(15, 5)),
  size = c(rep(1.2,8), rep(1.7,5)),
  group = c(1,2,3, 1,1.1, 2,2.1, 3,  1, 1.1, 2, 2.1 ,3)
)

cluster_tree <- ggplot() +
  geom_vline(aes(xintercept = c(1)), linetype = "dashed", color = "grey80") +
  geom_line(data = unique(data[data$x < 2,]), aes(x = x, y = y, group = group), col = "grey70", linewidth = 0.5) +
  geom_line(data = data[data$x > 0,], aes(x = x, y = y, group = group), col = "grey70", linewidth = 0.5) +
  geom_point(data = unique(data[,c("x", "y", "col", "shape", "size")]), 
             aes(x = x, y = y, col = col, shape = shape, size = size)) +
  geom_text(data = na.omit(data), aes(x = x+0.2, y = y+0.11, 
                                      label = paste0("n=",count)), color = "grey40", size = 2) +
  geom_text(aes(x = c(0.3, 1.7), y = c(.4,.4), angle = c(90,90), 
                label = c("Bud dormancy,\nbud burst", "Fruit maturation,\nleaf senescence")),
            color = "grey40", size = 2.3) +
  scale_shape_identity() + scale_color_identity() + scale_size_identity() +
  theme_void() +
  coord_cartesian(xlim = c(-1,3.3), ylim = c(4.2, -0.15), expand = FALSE) +
  theme(plot.margin = margin(t = 20, b = 25, r = 5.5, l = 5.5))
