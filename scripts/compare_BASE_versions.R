#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)

# define root path with here package and 
here::i_am("scripts/compare_BASE_versions.R")
library(here)
source(here("scripts", "plot_utils.R"))


prefix_string <- "v3.0"
analysis_target <- "BurntFraction_Natural"
intermediates_dir <- here("intermediates", analysis_target)


overlay <- rnaturalearth::ne_countries(returnclass = "sf")
sf::sf_use_s2(FALSE)


ba_cuts <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000,10000)
ba_cols <- turbo(length(ba_cuts)-1)

#### DEFINE THE AND READ THE BASELINE ####
baseline_base_version <- paste(prefix_string, "baseline", sep = "_")
baseline_base <- readRDS(file.path(intermediates_dir, baseline_base_version,  paste("DT", baseline_base_version,"rds", sep = ".")))

baseline_base[  , FireCCI51 := Observed_burnt_area] 
baseline_base[  , Baseline := Predicted_burnt_area_glm] 

baseline_yearmean <- baseline_base[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = c( "FireCCI51","Baseline")]
baseline_spatial <- baseline_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = c("FireCCI51","Baseline")]
baseline_spatial <- melt(baseline_spatial, id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area")

baseline_seasonal <-  baseline_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = c( "FireCCI51", "Baseline")]
baseline_seasonal <- melt(baseline_seasonal, id.vars = c("Month"), variable.name = "Source", value = "Burnt Area")

baseline_iav <-   baseline_base[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = c("FireCCI51", "Baseline")]
baseline_iav <- melt(baseline_iav, id.vars = c("Year"), variable.name = "Source", value = "Burnt Area")


#### FACTOR 1 - FUEL DRYNESS ####
factor_name <- "1-FuelDryness"
this_dir <- here("results", "FactorAnalyses", factor_name)
dir.create(this_dir, showWarnings = FALSE, recursive = TRUE)

dryness_factors <- c("Tmax", "FFMC", "DMC", "DC", "ISI", "BUI", "DSR") 
#dryness_factors <- c("Tmax")


this_spatial <- copy(baseline_spatial)
setnames(this_spatial, "Baseline", "FWI (baseline)")
this_seasonal <- copy(baseline_seasonal)
setnames(this_seasonal, "Baseline", "FWI (baseline)")
this_iav <- copy(baseline_iav)
setnames(this_iav, "Baseline", "FWI (baseline)")


# plot the difference between the baseline model and each fuel dryness factor
for(this_factor in dryness_factors){
  
  this_base_version <- paste("v3.1", "fuel_drying", this_factor, sep = "_")
  this_base <- readRDS(file.path(intermediates_dir, this_base_version, paste("DT", this_base_version, "rds", sep = ".")))
  
  this_base_prediction <- this_base[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_glm")]
  setnames(this_base_prediction, "Predicted_burnt_area_glm",  this_factor)
  this_base_yearmean <- this_base_prediction[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_factor]
  
  this_base_spatial <- this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_factor]
  this_base_spatial <- melt(this_base_spatial, id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area")
  this_spatial <- rbind(this_spatial, this_base_spatial)
  
  this_base_seasonal <-  this_base_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_factor]
  this_base_seasonal <- melt(this_base_seasonal, id.vars = c("Month"), variable.name = "Source", value = "Burnt Area")
  this_seasonal <- rbind(this_seasonal, this_base_seasonal)
  
  this_base_iav <-   this_base_prediction[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_factor]
  this_base_iav <- melt(this_base_iav, id.vars = c("Year"), variable.name = "Source", value = "Burnt Area")
  this_iav <- rbind(this_iav, this_base_iav)
  
}


# make an appropriate overlay
all_lons <- sort(unique(this_spatial[["Lon"]]))
all_lats <- sort(unique(this_spatial[["Lat"]]))
plot_region <- c(xmin = min(all_lons), xmax = max(all_lons), ymin = min(all_lats), ymax = max(all_lats))
this_overlay <- st_crop(overlay, plot_region)

# plot spatial 
this_spatial[ , value := cut(`Burnt Area`, ba_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
spatial_plot <- ggplot(this_spatial) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + scale_fill_viridis(option = "H", name = "Burnt area (ha)", discrete = TRUE) + facet_wrap(~Source) 
spatial_plot <- spatial_plot + coord_cartesian() + geom_sf(data=this_overlay, 
                                                           fill = "transparent", 
                                                           linewidth = 0.1,
                                                           colour= "black")
print(spatial_plot)

# plot IAV time series
iav_plot <- ggplot(this_iav) + geom_line(aes(x = Year, y = `Burnt Area`, col = Source))
print(iav_plot)

# plot season cycle
seasonal_plot <- ggplot(this_seasonal) + geom_line(aes(x = Month, y = `Burnt Area`, col = Source))
print(seasonal_plot)

