


fit_dir <- file.path(wd, "data", "fit")

species <- "fagus_sylvatica"

# Expert AUC
c <- "expert"
expert_auc <- readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$auc_all

# Load full calibration AUC
calibrations <- paste0("subset",rep(1:10, each = 10),"_rep", 1:10)
full_auc <- data.frame(
  mod = "full",
  auc = sapply(calibrations, function(c) readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$auc_all))

# Load partial calibration AUC - no constraint on maturation and FHmin
calibrations <- paste0("partial/subset",rep(1:2, each = 5),"_rep", 1:5)
partial_auc <- data.frame(
  mod = "partial\n(mat. & FHmin)",
  auc = sapply(calibrations, function(c) readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$auc_all))

# Load partial calibration AUC - no constraint on maturation and FHmin/max
calibrations <- paste0("partial/relmax/subset",rep(1, each = 5),"_rep", 1:5)
partial_auc2 <- data.frame(
  mod = "partial\n(mat. & FHmin/max)",
  auc = sapply(calibrations, function(c) readRDS(file.path(fit_dir, "ERA5-LAND", species, paste0(c, ".rds")))$auc_all))

# Plot
calibration_auc <- rbind(full_auc, partial_auc, partial_auc2)
ggplot() +
  geom_boxplot(data = calibration_auc, aes(x = mod, y = auc)) +
  geom_hline(aes(yintercept = expert_auc), color = "darkblue", linetype = "dashed", size = 1) +
  theme_minimal() + xlab("")
