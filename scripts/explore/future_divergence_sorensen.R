
# Compute divergence between future predictions across different calibrations (in terms of Sorensen index)

years <- 2020:2100
window <- 21
gcms
calibrations
thresholds <- c(0.785, 0.674, 0.663, 0.803)


r <- embed(years, window)[, c(window,1)]
yr_windows <- split(r, row(r))




sorensen_index <- as.data.frame(do.call(rbind, lapply(yr_windows, function(yr){
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


sorensen_index %>%
  mutate(sorensen = as.numeric(sorensen), yr = as.numeric(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(median = median(1-sorensen),
                   q025 = quantile(1-sorensen, 0.025), 
                   q975 = quantile(1-sorensen, 0.975)) %>%
  ggplot(aes(x = year, y = median)) +
  geom_line() +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.2) +
  theme_minimal() +
  ylab("Divergence between predictions")
  
  






