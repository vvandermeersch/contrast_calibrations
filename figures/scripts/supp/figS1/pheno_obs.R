
# Load ERA5-Land grid
era5_grid <- readRDS(file = file.path(wd, "figures", "data", "fig1", "expert_map_historical.rds"))
ctr <- ifel(is.na(era5_grid), 1, 0) %>% crop(ext(-10.35, 34.85, 34.65, 70.55)) %>% as.polygons()

# Coarser resolution
era5_grid <- aggregate(era5_grid, 2.5)

# Fagus observation points (leafout)
species <- "fagus_sylvatica"
stades <- c(10:15)
tempo_dir <- "D:/phenology/tempo/fagus_sylvatica/Vdm_Victor_202405021641"
source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")
fagusleaf <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.1, l = 0.1, unit='cm'),
        legend.position= "none",
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  labs(title = expression(paste(italic("F. sylvaltica"), ", leafout obs.")))

# Fagus observation points (flowering)
species <- "fagus_sylvatica"
stades <- c(60:65)
# source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")
fagusflo <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.1, l = 0.1, unit='cm'),
        legend.position= "none",
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  labs(title = expression(paste(italic("F. sylvaltica"), ", flowering obs.")))

# Fagus observation points (maturation)
species <- "fagus_sylvatica"
stades <- c(80:86)
# source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")
fagusmat <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.1, l = 0.1, unit='cm'),
        legend.position= "none",
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  labs(title = expression(paste(italic("F. sylvaltica"), ", fruit maturation obs.")))

# Fagus observation points (senescence)
species <- "fagus_sylvatica"
stades <- c(90:95)
source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")
fagussen <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.1, l = 0.1, unit='cm'),
        legend.position= "none",
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  labs(title = expression(paste(italic("F. sylvaltica"), ", leaf senescence obs.")))

# Quercus observation points (maturation)
species <- "quercus_robur"
stades <- c(80:86)
tempo_dir <- "D:/phenology/tempo/quercus_robur/Victor_Vdm_202406251802"
source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")
quercusmat <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.1, l = 0.1, unit='cm'),
        legend.position= "none",
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  labs(title = expression(paste(italic("Q. robur"), ", fruit maturation obs.")))

# Betula observation points (flowering)
species <- "betula_pendula"
stades <- c(60:65)
tempo_dir <- "D:/phenology/tempo/betula_pendula/Victor_Vdm_202405271800"
source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")
betulaflo <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.1, 'cm'), legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.1, b=0.05, r = 0.1, l = 0.1, unit='cm'),
        legend.position= "none",
        legend.background = element_rect(colour="black", fill="white", linewidth = 0.2)) +
  labs(title = expression(paste(italic("B. pendula"), ", flowering obs.")))

# Picea observation points (flowering)
species <- "picea_abies"
stades <- c(60:65)
tempo_dir <- "D:/phenology/tempo/picea_abies/Victor_Vdm_202406251804"
source(file.path(wd, "figures", "scripts", "fig7", "load_pheno_data.R"))
record_points <- all_records %>%
  dplyr::filter(stade %in% stades & year %in% c(1970:2000)) %>%
  vect() %>%
  rasterize(era5_grid, fun = "count")

piceaflo <- ggplot() + 
  geom_spatraster(data = ifel(record_points>100,100, record_points)) +
  scale_fill_gradientn(colours = c("#eaad5a", "#90be6d", "#4d908e", "#577590", "#905775"), na.value = NA, 
                       limits = c(1,100), breaks = c(1,25,50,75,100), labels = c(1, 25, 50, 75, ">100")) +
  geom_spatvector(data = ctr, color = "grey40", fill = NA, linewidth = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(-10.35, 34.85)) +
  scale_y_continuous(expand = c(0, 0), limits = c(34.65, 70.55)) +
  theme_void() +
  guides(fill = guide_colorbar(title.position = "top", direction = "horizontal", 
                               frame.colour = "black", frame.linewidth = 0.1, ticks.colour = NA)) +
  theme(legend.key.height = unit(0.2, 'cm'), legend.key.width = unit(1, 'cm'),
        legend.title = element_blank(),
        plot.title = element_text(size = 8), 
        legend.text = element_text(size = 7, margin = margin(t = 2)),
        panel.border = element_rect(colour = "grey85", fill=NA, size=0.75),
        plot.margin = unit(c(t = 0,b = 0, r= 1, l=1), units = 'mm'),
        legend.margin=margin(t = 0.3, b=0.05, r = 0.15, l = 0.15, unit='cm'),
        legend.position= "none") +
  labs(title = expression(paste(italic("P. abies"), ", flowering obs.")))
