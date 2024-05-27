
# Data from TEMPO
# dir <- "D:/phenology/tempo/betula_pendula/Victor_Vdm_202405271800"
# 
# stations_tempo  <- fread(file.path(dir, "sites.csv")) %>% 
#   dplyr::select("site id", latitude, longitude)
# 
# records_tempo <- fread(file.path(dir, "pheno-pmp.txt")) %>%
#   dplyr::filter(ANNEE %in% c(1970:2000)) %>%
#   mutate(across(everything(), ~na_if(., -9999))) %>%
#   pivot_longer(cols = -c(CODE_POSTE, VAR_PROV, ANNEE),
#                names_to = "stade", values_to = "doy", values_drop_na = TRUE) %>%
#   left_join(stations_tempo, by = c("CODE_POSTE" = "site id")) %>%
#   reframe(lat = latitude, lon = longitude, year = ANNEE, 
#           stade.raw = stade, 
#           stade = as.numeric(str_split(stade.raw, pattern = "-", simplify = TRUE)[,2]), 
#           doy = doy, source = "TEMPO")

# Data from PEP725
dir <- file.path("D:/phenology/PEP725", species)

stations_pep <- fread(file.path(dir, "stations_merged.csv")) %>% 
  dplyr::select(PEP_ID, LON, LAT) %>% unique()

records_pep <- fread(file.path(dir, "records_merged.csv")) %>%
  left_join(stations_pep, by = "PEP_ID") %>%
  reframe(lat = LAT, lon = LON, year = YEAR, 
          stade.raw = BBCH, stade = BBCH, 
          doy = DAY, source = "PEP725")

# Merge
records <- records_pep

# give.n <- function(x){
#   return(c(y = 310, label = length(x))) 
# }
# ggplot(data = records, aes(x = stade.raw, y = doy)) +
#   facet_wrap(~ source, scales = "free_x") + 
#   geom_boxplot() +
#   theme_minimal() +
#   stat_summary(fun.data = give.n, geom = "text", fun.y = median)

# Rasterize observations according to ERA5-Land resolution
# grid <- fread("D:/climate/ERA5-Land/phenofit_format/transformed/ERA5LAND_Altitude.fit")
all_records <- records %>%
  mutate(lat = round(lat, 1), lon = round(lon, 1)) %>%
  group_by(lat, lon, year, stade, source) %>%
  summarise(mean_doy = mean(doy), count_obs = n())


# give.n <- function(x){
#   return(c(y = 368, label = length(x))) 
# }
# 
# ggplot(data = all_records %>% filter(year %in% c(1970:2000)), aes(x = factor(stade), y = mean_doy)) +
#   geom_boxplot() +
#   stat_summary(fun.data = give.n, geom = "label", fun.y = median,
#                position = position_dodge(width = 0.75))
