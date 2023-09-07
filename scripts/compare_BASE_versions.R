#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)

# define root path with here package and 
here::i_am("scripts/compare_BASE_versions.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))


prefix_string <- "v5"
analysis_target <- "BurntFraction_Natural"
intermediates_dir <- here("intermediates", "GLMs", paste(analysis_target, prefix_string))
text.multiplier <- 2.5
obs_name <- "FireCCI51"

# aesthetics for the plots
obs_colour <- "black"
  
baseline_colour <- "grey"
  
alternative_palette <- viridis
obs_linewidth <- 2
baseline_linewidth <- 2
alternative_linewidth <- 1
obs_linetype <- "dashed"
baseline_linetype <- "solid"
alternative_linetype <- "solid"


#### DEFINE THE AND READ THE BASELINE ####
baseline_base_version <- paste0(prefix_string, ".0_baseline")
baseline_base <- readRDS(file.path(intermediates_dir, baseline_base_version,  paste("DT", baseline_base_version, "rds", sep = ".")))
setnames(baseline_base, c("Observed_burnt_area", "Predicted_burnt_area_raw"), c(obs_name, "Baseline")) 
baseline_yearmean <- baseline_base[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = c( obs_name,"Baseline")]
baseline_spatial <- baseline_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = c(obs_name,"Baseline")]
baseline_seasonal <-  baseline_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = c( obs_name, "Baseline")]
baseline_iav <-   baseline_base[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = c(obs_name, "Baseline")]
this_overlay <- makeOverLay(baseline_spatial)


#### FACTOR 1 - FUEL DRYNESS ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 1
factor_title <- "Fuel_Drying"
baseline_name <- "FWI (baseline)"
these_factors <- c("Tmax", "FFMC", "DMC", "DC", "ISI", "BUI", "DSR")

# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)



#### FACTOR 2 - FUEL AVAILABILITY ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 2
factor_title <- "Fuel_Availability"
baseline_name <- "Mean annual GPP (baseline)"
these_factors <-  c("log_Mean_annual_GPP", 
                    "FAPAR12", "Mean_annual_FAPAR",
                    "GPP12", "log_GPP12",
                    "AGB_Natural", "AGB_Gridcell",
                    "log_AGB_Natural", "log_AGB_Gridcell",
                    "Treecover_Natural", "Treecover_Gridcell")

# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)



#### FACTOR 3 - FUEL CURING ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 3
factor_title <- "Fuel_Curing"
baseline_name <- "FAPAR (baseline)"
these_factors <-   c("FAPAR_index", "GPP", "GPP_index")

# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)




#### FACTOR 4 - HUMAN FACTORS ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 4
factor_title <- "Human"
baseline_name <- "HDI and Pop_Dens (baseline)"
these_factors <-   c("None", "PopDensxHDI", "UrbanxHDI", "PopDens_quadratic_HDI")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)



#### FACTOR 5 - TREE FLAMMABILITY ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 5
factor_title <- "Tree_Flammability"
baseline_name <- "Group (baseline)"
these_factors <-   c("none", "Dominant_species")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)


#### FACTOR 6 - Landscape continuity ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 6
factor_title <- "Continuity"
baseline_name <- "None (baseline)"
these_factors <-   c("Natural", "Natural_quadratic")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)



#### FACTOR 7 - Topography ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 7
factor_title <- "Topography"
baseline_name <- "None (baseline)"
these_factors <-   c("Slope", "TPI", "SlopexTPI")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)




#### FACTOR 8 - Windspeed ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 8
factor_title <- "WindSpeed"
baseline_name <- "None (baseline)"
these_factors <-   c("WindSpeed")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  #### SPECIAL
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)


#### FACTOR 8 - Windspeed ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 8
factor_title <- "WindSpeed"
baseline_name <- "None (baseline)"
these_factors <-   c("WindSpeed")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  #### SPECIAL
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)





#### FACTOR 9 - SPOTTING ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 9
factor_title <- "Spotting"
baseline_name <- "None (baseline)"
these_factors <-   c("WindspeedxAGB_Natural", "WindspeedxAGB_Gridcell", "WindspeedxTreecover_Natural", "WindspeedxTreecover_Gridcell")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)




#### FACTOR 10 - LAND ABANDONMENT ####

# adjust these, and also adust the spatial plot sizes below 
factor_number <- 10
factor_title <- "Land_Abandonment"
baseline_name <- "None (baseline)"
these_factors <-    c("deltaNatural10", "deltaNatural20", "deltaNatural30", "deltaNatural40")


# strings and directories
factor_name <- paste(factor_number,  factor_title, sep = "_")
this_dir <- here("results", "Per_Factor_Analyses", paste(analysis_target, prefix_string, sep = "_"), factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

# prepare the baseline and observed data
this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", baseline_name)
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", baseline_name)
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", baseline_name)

# aesthetics
these_cols <- c(obs_colour, baseline_colour)
these_linewidths <-  c( obs_linewidth, baseline_linewidth)
these_linetypes <-  c(obs_linetype, baseline_linetype)
these_names <- c(obs_name, baseline_name)

# plot the difference between the baseline model and each  factor
for(this_factor in these_factors) {
  print(this_factor)
  
  # read, rename and calculate the year mean
  this_base_version <- paste(paste0(prefix_string, ".", factor_number), gsub(pattern = " ", replacement = "_", tolower(factor_title)), this_factor, sep = "_")
  
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")]
  setnames(this_base_prediction, "Predicted_burnt_area_raw",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  # spatial
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_spatial <- merge(this_spatial, this_base_spatial, by = c("Lon", "Lat"))
  
  # seasonal
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_seasonal <- merge(this_seasonal, this_base_seasonal, by = c("Month"))
  
  # interannual variability
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_iav <- merge(this_iav, this_base_iav, by = c("Year"))
  
  # and appropriate aesthetics
  these_linewidths <-  append(these_linewidths, c(alternative_linewidth))
  these_linetypes <-  append(these_linetypes, c(alternative_linetype))
  these_cols <-  append(these_cols, c(viridis(length(these_factors))[match(this_factor, these_factors)]))
  these_names <-  append(these_names, this_factor)
  
}

# add names for aesthetics
names(these_linewidths) <- these_names
names(these_linetypes) <- these_names
names(these_cols) <- these_names

# plot all dimensional reductions
spatialPlot(this_spatial, 
            overlay = this_overlay,  
            filename = file.path(this_dir, paste("Spatial", factor_name, sep = "_")), 
            width = 1400, height = 1200)
spatialPlotDifferenceFromBaseline(this_spatial, 
                                  baseline = baseline_name, 
                                  overlay = this_overlay,  
                                  filename = file.path(this_dir, paste("SpatialDifferenceFromBaseline", factor_name, sep = "_")), 
                                  width = 1400, height = 1200)
spatialDeltaError(this_spatial,
                  baseline = baseline_name,  
                  obs = obs_name, 
                  overlay = this_overlay,
                  filename = file.path(this_dir, paste("SpatialDeltaError", factor_name, sep = "_")), 
                  width = 1400, height = 1200)
IAVPlot(this_iav, cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
        filename = file.path(this_dir, paste("IAV", factor_name, sep = "_")), width = 1400, height = 900)
seasonalPlot(this_seasonal,  cols = these_cols, linewidths = these_linewidths, linetypes = these_linetypes,
             filename = file.path(this_dir, paste("Seasonal", factor_name, sep = "_")), width = 1400, height = 900)


