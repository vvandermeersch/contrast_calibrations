library(data.table)
library(future.apply)

# Simulations against data
wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/contrast_calibrations"
source(file.path(wd, "scripts", "functions", "read_mean_outputvalue.R"))

sim_dir <- file.path(wd, "data", "simulations", "historical", "ERA5-Land")
species <- "fagus_sylvatica"

models <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

plan(multisession, workers = 10)
leafout_simulations <- future_lapply(models, function(m){
  
  leafout_m <- data.frame()
  sim_path <- file.path(sim_dir, species, m)
  for(yr in 1970:2000){
    
    leafout <- read_mean_outputvalue(sim_path, years = yr, model = "PHENOFIT",
                                     output_var = "LeafUnfoldingDate", correct_date = TRUE)
    names(leafout) <- c("lat", "lon", as.character(yr))
    
    leafout <- leafout %>%
      inner_join(unique(records[records$year == yr,c("lat", "lon")]), by = join_by(lat, lon))
    
    if(yr == 1970){
      leafout_m <- leafout
    }else{
      leafout_m <- leafout_m %>%
        full_join(leafout, by = join_by(lat, lon))
    }
    
  }
  gc()
  
  leafout_m <- leafout_m %>%
    pivot_longer(cols = -c(lat, lon), names_to = "year", values_to = "sim_doy", values_drop_na = TRUE) %>%
    mutate(year = as.numeric(year), mod = m) %>% 
    left_join(records[c("lat", "lon", "year", "stade", "mean_doy")], by = join_by(lat, lon, year))
  
  # leafout_rmse <- leafout_m %>%
  #   group_by(lat, lon, year, stade, sim_doy, mean_doy) %>%
  #   reframe(rmse = rmse(sim_doy, mean_doy),  mod = m)
  
  return(leafout_m)
  
})
plan(sequential); gc()
leafout_simulations2 <- as.data.frame(do.call(rbind, leafout_simulations))

leafout_simulations2 %>%
  group_by(mod, stade) %>%
  reframe(rmse = rmse(sim_doy, mean_doy))






