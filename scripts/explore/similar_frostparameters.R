

frost_parameters <- species_parameters %>%
  dplyr::filter(grepl("frost", species_parameters$var))
frost_parameters$var <- factor(frost_parameters$var, levels = paste0('frost',1:12))

frost_par_sum <- frost_parameters %>%
  group_by(rep) %>%
  summarize(
    FHleafmin = value[var == "frost3"],
    FHflowermin = value[var == "frost4"],
    FHtfemax = value[var == "frost7"],
    FHtflmax = value[var == "frost8"],
    FHpfemax = value[var == "frost9"],
    FHpflmax = value[var == "frost10"],
    FHflmax = FHtflmax + FHpflmax,
    FHfemax = FHtfemax + FHpfemax
  ) %>%
  ungroup()
head(frost_par_sum)



frost_pmat <- as.matrix(frost_par_sum[, 2:3])
rownames(frost_pmat) <- unlist(frost_par_sum[, 1])
# frost_pmat <- scale(frost_pmat)

M <- cor(t(frost_pmat))

index_corrplot <- corrplot(M, order = 'hclust', addrect = 2)

dist <- dist(M)
tree <- hclust(dist)
plot(tree)
frostpar_clusters <- cutree(tree, k = 2)
frostpar_clusters <- data.frame(mod = names(frostpar_clusters), clust = frostpar_clusters)
saveRDS(frostpar_clusters, file.path(wd, "data", "metrics", "frostpar_cluster_calibrations.rds"))

frost_par_sum <- frost_par_sum %>%
  left_join(frostpar_clusters, by = c("rep"="mod"))
