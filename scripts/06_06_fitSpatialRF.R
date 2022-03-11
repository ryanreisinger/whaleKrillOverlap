# Fit spatial models

setwd("D:\\UCSC\\Analysis\\megaKrill_HOLD")

library(ranger)
library(dplyr)
library(ggplot2)
library(raster)
library(spatialEco)
library(patchwork)

# Frame to save model error estimates
model_error <- data.frame("species" = NA,
                          "month" = NA,
                          "error" = NA)

# Frame to save variable importance
importance_all <- data.frame()
                          
for (j in c("humpback", "minke")) {
  
  # which_whale <- "humpback"
  which_whale <- j
  print(j)

if (which_whale == "humpback") {
  the_months <- seq(1, 7)
}

if (which_whale == "minke") {
  the_months <- seq(2, 6)
}

master_dat <- data.frame()
r_stack <- stack()

for (k in the_months) {
  
  this_month <- k
  cat("Fitting month", this_month, "\n")

# Get data
these_tracks <- readRDS(paste0("./data_out/spmods_tracks_w_distance&env_", which_whale, "_", this_month, ".RDS"))
grd_dataframe <- readRDS(paste0("./data_out/spmods_grid_w_distance&env_", which_whale, "_", this_month, ".RDS"))

# Double check that ice cv is 0 instead of NA
these_tracks$ICEDIST_cv[is.na(these_tracks$ICEDIST_cv)] <- 0
grd_dataframe$ICEDIST_cv[is.na(grd_dataframe$ICEDIST_cv)] <- 0

these_tracks$ICECONC_cv[is.na(these_tracks$ICECONC_cv)] <- 0
grd_dataframe$ICECONC_cv[is.na(grd_dataframe$ICECONC_cv)] <- 0

# Some distance covariates are all NA when locs were on land and no path could be calculated
# - remove these columns
these_tracks <- these_tracks[, colSums(is.na(these_tracks)) != (nrow(these_tracks)-1)]
grd_dataframe <- grd_dataframe[, colSums(is.na(grd_dataframe)) != (nrow(grd_dataframe)-1)]

# if (which_whale == "humpback" & this_month == 5) {
#   these_tracks <- these_tracks[, colSums(is.na(these_tracks)) != (nrow(these_tracks)-2)]
#   grd_dataframe <- grd_dataframe[, colSums(is.na(grd_dataframe)) != (nrow(grd_dataframe)-2)]
# }

# Complete data rows only
these_tracks <- these_tracks[complete.cases(these_tracks), ]

# Create formula
dn0 <- paste(names(these_tracks)[15:ncol(these_tracks)], collapse=" + ")
fm0 <- as.formula(paste("b_state ~ ", dn0))

# Filter to select only certain b-states
these_tracks <- filter(these_tracks, b_state == "R" | b_state == "T")

# As factor for rf
these_tracks$b_state <- as.factor(these_tracks$b_state)

# Fit the rf
m_0 <- ranger(fm0, data = these_tracks, num.trees = 500,
              probability = T,
              oob.error = TRUE,
              importance = "impurity",
              replace = TRUE,
              keep.inbag = TRUE)

# Get error
this_result <- data.frame("species" = which_whale,
                          "month" = k,
                          "error" = m_0$prediction.error)

model_error <- rbind(model_error, this_result)
rm(this_result)

# Get variable importance
imp <- importance(m_0)
imp <- imp[(length(imp)-7):length(imp)]
out <- data.frame("importance" = imp,
                  "variable" = names(imp))
out$species <- which_whale
out$month <- k
row.names(out) <- NULL

importance_all <- rbind(importance_all, out)

# Index for complete cases in prediction data
dx <- which(complete.cases(grd_dataframe))

# Predict
grd_dataframe$p[dx] <- predict(m_0, data = grd_dataframe[dx,], type = "response")$predictions[,"R"]
grd_dataframe$se[dx] <- predict(m_0, data = grd_dataframe[dx,], type = "se")$se[,"R"]

# Where mean sea ice concentration is too high, set to NA to mask predictions
# grd_dataframe[!is.na(grd_dataframe$ICECONC_mean) & grd_dataframe$ICECONC_mean > 90, ]$p <- NA
# grd_dataframe[!is.na(grd_dataframe$ICECONC_mean) & grd_dataframe$ICECONC_mean > 90, ]$se <- NA


# Add to dataframe
grd_dataframe$month <- this_month
grd_dataframe$species <- which_whale
master_dat <- rbind(master_dat, dplyr::select(grd_dataframe, x, y, p, se, month, species))

# Check
# ggplot(data = grd_dataframe, aes(x = x, y = y, fill = p)) + geom_raster() + coord_quickmap()
# ggplot(data = grd_dataframe, aes(x = x, y = y, fill = se)) + geom_raster()  + coord_quickmap()

# Save output
saveRDS(grd_dataframe, paste0("./data_out/rf_predictions/rf_pred_frame_", which_whale, "_", this_month, ".RDS"))

# Convert to raster and save
r <- rasterFromXYZ(dplyr::select(grd_dataframe, x, y, p))
writeRaster(r, paste0("./data_out/rf_predictions_raster/rf_pred_raster_", which_whale, "_", this_month, ".grd"),
            format = "raster",
            overwrite = T)

r_stack <- stack(r_stack, r)

}

p <- ggplot(data = master_dat, aes(x = x, y = y, fill = p)) +
  geom_raster() +
  coord_quickmap() +
  facet_wrap(~month) +
  scale_fill_viridis_c(name = "p(Restricted\nbehavioural state)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(x = "", y = "")

png(paste0("./figures/rf_predictions_month_", which_whale, ".png"),
    height = 200/0.7,
    width = 160/0.7,
    units = "mm",
    res = 700)
plot(p)
dev.off()

# ggplot(data = grd_dataframe, aes(x = x, y = y, fill = se)) + geom_raster()  + coord_quickmap()

# Calculate trend
r_tau <- raster.kendall(x = r_stack, tau = TRUE)

# plot(r_tau[[2]])

writeRaster(r_tau[[2]],
            paste0("./data_out/rf_predictions_trend/rf_trend_", which_whale, ".grd"),
            format = "raster",
            overwrite = T)

}

# Save model error
write.csv(model_error, "./data_out/model_prediction_error.csv", row.names = F)

# Save variable importance
write.csv(importance_all, "./data_out/model_variable_importance.csv", row.names = F)

# Plot of variable importance
p1 <- ggplot(filter(importance_all, species == "humpback"), aes(y = reorder(variable, importance, function(x)mean(x, na.rm = T)), x = importance)) +
  geom_point() +
  labs(x = "Variable importance", y = "Variable", subtitle = "a) Humpback") +
  theme_bw()
p2 <- ggplot(filter(importance_all, species == "minke"), aes(y = reorder(variable, importance, function(x)mean(x, na.rm = T)), x = importance)) +
  geom_point() +
  theme_bw() +
  labs(x = "Variable importance", y = "Variable", subtitle = "b) Minke")

png("./figures/variable_importance.png",
    height = 100/0.7,
    width = 120/0.7,
    units = "mm",
    res = 700)
p1 + p2
dev.off()