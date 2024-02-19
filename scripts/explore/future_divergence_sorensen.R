
library(future.apply)



years <- 2020:2100
window <- 21
gcms <- c("GFDL-ESM4", "MPI-ESM1-2-HR", "MRI-ESM2-0")
calibrations <- c("subset4_rep1", "subset5_rep4", "subset3_rep8", "subset1_rep7")
thresholds <- c(0.785, 0.674, 0.663, 0.803)


# Compute divergence between future predictions across different calibrations (in terms of Sorensen index)
r <- embed(years, window)[, c(window,1)]
yr_windows <- split(r, row(r))

plan(multisession, workers = 20)
sorensen_index <- as.data.frame(do.call(rbind, future_lapply(yr_windows, function(yr){
  cat(paste0((yr[1]+yr[2])/2, "...\n"))
  
  # Load simulations
  simulations <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    sim <- as.data.frame(do.call(rbind, lapply(1:length(calibrations), function(i){
      sim_dir <- file.path(wd, "data", "simulations", "future", m, scenario)
      output <- read_mean_outputvalue(file.path(sim_dir, calibrations[i]), 
                                      years = c(yr[1]:yr[2]), model = "PHENOFIT", output_var = "Fitness")
      names(output) <- c("lat", "lon", "fitness")
      output$fitness <- ifelse(output$fitness < thresholds[i], 0, 1)
      output$cal <- calibrations[i]
      return(output)
    })))
    sim$gcm <- m
    return(sim)
  })))
  
  # Compute pairwise Sorensen index - by GCM
  sor <- as.data.frame(do.call(rbind, lapply(gcms, function(m){
    sim <- simulations[simulations$gcm == m, ]
    combs <- t(combn(x = unique(sim$cal), m = 2))
    sor <- apply(X = combs, MARGIN = 1, function(i){
      sim1 <- sim[sim$cal == i[1],]
      sim2 <- sim[sim$cal == i[2],]
      sim_join <- left_join(sim1[,1:3], sim2[,1:3], by = c("lat", "lon"))
      sorensen <- 2*nrow(sim_join[sim_join$fitness.x == 1 & sim_join$fitness.y == 1,])/
        (nrow(sim_join[sim_join$fitness.x == 1,])+nrow(sim_join[sim_join$fitness.y == 1,]))
      return(c(sorensen, paste0(i[1],".",i[2])))
    })
    return(data.frame(gcm = m, sorensen = sor[1,], comb = sor[2,]))
  })))
  sor$year <- (yr[1]+yr[2])/2
  
  return(sor)
  
})))



hist_div_data <- readRDS(file.path(wd, "data", "metrics", "sorensen_divergence_historical.rds"))
fut_div_data <- sorensen_index %>%
  mutate(sorensen = as.numeric(sorensen), yr = as.numeric(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(median = median(1-sorensen),
                   q025 = quantile(1-sorensen, 0.025), 
                   q975 = quantile(1-sorensen, 0.975))
hist_fut_div_plot <- ggplot() +
  geom_boxplot(data = hist_div_data, aes(x = 2020, y = 1-sorensen), width = 5) +
  geom_line(data = fut_div_data, aes(x = year, y = median)) +
  geom_ribbon(data = fut_div_data, aes(x = year, ymin = q025, ymax = q975), alpha = 0.2) +
  theme_minimal() +
  scale_x_continuous(breaks = c(2020, seq(2030, 2090, 10)), labels = c("Historical\n(1970-2000)", seq(2030, 2090, 10))) +
  theme(panel.grid.minor.x = element_blank(), axis.title.x = element_blank()) + ylim(0, 0.4) +  ylab("Divergence between predictions")







