library(future.apply)

wd <- "C:/Users/vandermeersch/Documents/CEFE/projects/future_forests"
datad <- "D:/projects/future_forests"
fit_dir <- file.path(wd, "data", "fit")

species <- "fagus_sylvatica"
calibrations <- c("expert", paste0("subset",rep(1:10, each = 10),"_rep", 1:10))

gcms <- c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0","UKESM1-0-LL", "IPSL-CM6A-LR")
scenarios <- c("ssp245", "ssp585")

ssps <- c("ssp245", "ssp585")

years <- 2020:2100
window <- 21


# Compute divergence between future predictions across different calibrations (in terms of Sorensen index)
r <- embed(years, window)[, c(window,1)]
yr_windows <- split(r, row(r))

yr_windows <- list(c(2020,2040), c(2040,2060), c(2060,2080), c(2080,2100))

plan(multisession, workers = 21)
sorensen_index <- as.data.frame(do.call(rbind, lapply(yr_windows, function(yr){
  cat(paste0((yr[1]+yr[2])/2, "...\n"))
  
  # Load simulations
  simulations <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    sim <- as.data.frame(do.call(rbind, lapply(ssps, function(s){
      sim <- as.data.frame(do.call(rbind, future_lapply(1:length(calibrations), function(i){
        sim_dir <- file.path(datad, "data", "simulations", species, m, s)
        output <- read_mean_outputvalue(file.path(sim_dir, "2020_2100", calibrations[i]), 
                                        years = c(yr[1]:yr[2]), model = "PHENOFIT", output_var = "Fitness")
        names(output) <- c("lat", "lon", "fitness")
        
        threshold <- readRDS(file.path(fit_dir, m,  species, paste0(calibrations[i], ".rds")))$best_threshold
        
        output$pres <- ifelse(output$fitness < threshold, 0, 1)
        output$cal <- calibrations[i]
        return(output)
      })))
      sim$gcm <- m
      sim$ssp <- s
      return(sim)
    })))
    return(sim)
  })))
  
  # Compute pairwise Sorensen index - by GCM/SSP
  sor <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    sor <- as.data.frame(do.call(rbind, lapply(scenarios, function(s){
      sim <- simulations[simulations$gcm == m & simulations$ssp == s, ]
      combs <- t(combn(x = unique(sim$cal), m = 2))
      sor <- apply(X = combs, MARGIN = 1, function(i){
        sim1 <- sim[sim$cal == i[1],]
        sim2 <- sim[sim$cal == i[2],]
        sim_join <- left_join(sim1[,c("lat", "lon", "pres")], sim2[,c("lat", "lon", "pres")], by = c("lat", "lon"))
        sorensen <- 2*nrow(sim_join[sim_join$pres.x == 1 & sim_join$pres.y == 1,])/
          (nrow(sim_join[sim_join$pres.x == 1,])+nrow(sim_join[sim_join$pres.y == 1,]))
        return(c(sorensen, paste0(i[1],".",i[2]), ifelse(i[1] == "expert" | i[2] == "expert", "with.expert", "only.inverse")))
      })
      return(data.frame(gcm = m, ssp = s, sorensen = sor[1,], comb = sor[2,], cat = sor[3,]))
    })))
    return(sor)
  })))
  sor$year <- (yr[1]+yr[2])/2
  
  return(sor)
  
})))
plan(sequential)
gc()

saveRDS(sorensen_index, file.path(wd, "sorensen_index.rds"))

fut_div_data <- sorensen_index %>%
  mutate(sorensen = as.numeric(sorensen), yr = as.numeric(year)) %>%
  dplyr::group_by(year, cat) %>%
  dplyr::summarise(median = median(1-sorensen),
                   q025 = quantile(1-sorensen, 0.025), 
                   q975 = quantile(1-sorensen, 0.975))

ggplot() +
  geom_line(data = fut_div_data, aes(x = year, y = median, col = cat)) +
  geom_ribbon(data = fut_div_data, aes(x = year, ymin = q025, ymax = q975, fill = cat), alpha = 0.2) +
  scale_fill_manual(values = c("#0c86c8", "#40b8ab")) +
  scale_color_manual(values = c("#0c86c8", "#40b8ab")) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2018, seq(2030, 2090, 10)), labels = c("Historical\n(1970-2000)", seq(2030, 2090, 10))) +
  theme(panel.grid.minor.x = element_blank(), axis.title.x = element_blank()) + ylim(0, 0.85) +  ylab("Divergence between predictions")
