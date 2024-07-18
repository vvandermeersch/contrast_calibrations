
compute_best_threshold <- function(output, 
                                   sp_presabs, sp_name,
                                   filename, dir){
  
  cat(paste0(filename, "\n"))
  # Compute AUC on every pres/abs points
  fitness_presabs <- inner_join(sp_presabs, output, by = c("lat", "lon"))
  auc_tot <- round(AUC::auc(roc(fitness_presabs$pred, as.factor(sp_presabs$pres))),6)
  cat(paste0("   AUC: ", round(auc_tot, 3), "\n"))
  
  # Best threshold
  youden_index <- AUC::sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure +
    AUC::specificity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$measure - 1
  thresholds <- AUC::sensitivity(fitness_presabs$pred, as.factor(sp_presabs$pres), perc.rank = F)$cutoffs
  best_threshold <- thresholds[which(youden_index == max(youden_index))]
  cat(paste0("   Best threshold: ", round(best_threshold, 3), "\n"))
  
  # Confusion matrix and TSS
  fitness_presabs$pred_pres <- ifelse(fitness_presabs$pred < best_threshold,0 , 1)
  tp <- nrow(fitness_presabs[fitness_presabs$pred_pres == 1 & fitness_presabs$pres == 1,])
  fp <- nrow(fitness_presabs[fitness_presabs$pred_pres == 1 & fitness_presabs$pres == 0,])
  tn <- nrow(fitness_presabs[fitness_presabs$pred_pres == 0 & fitness_presabs$pres == 0,])
  fn <- nrow(fitness_presabs[fitness_presabs$pred_pres == 0 & fitness_presabs$pres == 1,])
  mig_sens = tp/(tp+fn)
  mig_spec = tn/(tn+fp)
  tss = mig_sens + mig_spec - 1
  cat(paste0("   TSS: ", round(tss, 3), "\n"))
  sorensen = 2*tp/(fn + 2*tp + fp)
  cat(paste0("   Sorensen index: ", round(sorensen, 3), "\n"))
  
  # save (as in CSDM)
  outfile <- list()
  outfile$species <- sp_name
  outfile$name <- "PHENOFIT" # model name
  outfile$species_file <- filename
  outfile$auc_all <- auc_tot # auc on every species points
  outfile$tss_all <- tss # tss on every species points
  outfile$sorensen_all <- sorensen # sorensen on every species points
  outfile$best_threshold <- best_threshold # best threshold to discriminate probabilites
  saveRDS(outfile, file = file.path(dir, sp_name, paste0(filename, ".rds")))
  
}




