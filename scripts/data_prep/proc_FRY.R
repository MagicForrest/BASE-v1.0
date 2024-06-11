library(data.table)
library(terra)
library(viridis)
# define root path with here package and 
here::i_am("scripts/data_prep/proc_FRY.R")
library(here)
source(here("scripts", "build_dt_helper_functions.R"))

ba_cuts <- c(0,5,10,20,50,100,200,500,1000,2000,5000)
ba_cols <- turbo(length(ba_cuts)-1)

#### SETTINGS ####

base_dir <- here("external_files", "gridded_9km")
fwi_raw_dir <- here("external_files", "daily_fwi/")
climate_dir <- here("external_files", "era5_climate/")
fry_dir <- file.path(base_dir, "FRY_v2.0")

ncv_lcc_types <- c(50, # broadleaved evergreen tree
                   60, 61, 62, # broadleaved deciduous tree
                   70, 71, 72, # needleaved evergreen tree
                   80, 81, 82, # needleaved deciduous tree
                   90, # mixed leaftype tree
                   100, # mosaic woody-herbaceous
                   110, # mosaic herbaceous-woody
                   120, 121, 122, # shrublands
                   130, # grassland
                   140, # lichens and mosses
                   150, 151, 152, 153,  # sparse vegetation
                   160, 170, 180) # flooded vegetation types


#### TARGET GRID ####

# get target grid and make the cuts and lookup for longitude and lat
ref_grid_terra <- rast(file.path(base_dir, "FirEUrisk_ref_grid.nc"))
lon_boundaries <- seq(xmin(ref_grid_terra), xmax(ref_grid_terra),  by = res(ref_grid_terra)[1])
lat_boundaries <- seq(ymin(ref_grid_terra), ymax(ref_grid_terra),  by = res(ref_grid_terra)[2])
lon_centres <- lon_boundaries[1:(length(lon_boundaries)-1)] + res(ref_grid_terra)[1]/2
lat_centres <- lat_boundaries[1:(length(lat_boundaries)-1)] + res(ref_grid_terra)[2]/2


### READ AND QUICKLY SUBSET DATA ###

# read the data
tic()
all_data <- fread("/data/shared/Fire/FRY/v2.0/tables/FIRECCI51_plain_table_6D.txt.gz")
toc()

# quick subset for the European (Area 3 patches)
euro_patches <- grep('AREA_3', all_data$ptch_id) 
euro_data <- all_data[euro_patches,]

#### ADD GRIDCELL COORDINATES AND MONTH ####

# get the gridcell coords
euro_data[ , LON_INDEX := cut(x = LON, breaks = lon_boundaries, labels = FALSE)]
euro_data[ , LON_GRID := lon_centres[LON_INDEX]]
euro_data[ , LAT_INDEX := cut(x = LAT, breaks = lat_boundaries, labels = FALSE)]
euro_data[ , LAT_GRID := lat_centres[LAT_INDEX]]

# get the month and day
euro_data[ , MONTH := month(minBD)]
euro_data[ , DAY := yday(minBD)]


#### SUBSET TO FIREURISK DOMAIN ####

# build mask (taking from FirEUrisk land cover)
landcover_terra <-  rast(file.path(base_dir, "LandUse", "Lu_6bands_firEUrisk.nc"))
mask_dt <- as.data.table(landcover_terra[[1]], xy = TRUE)
mask_dt[ , LON_INDEX := cut(x = x, breaks = lon_boundaries, labels = FALSE)]
mask_dt[ , LON_GRID := lon_centres[LON_INDEX]]
mask_dt[ , LAT_INDEX := cut(x = y, breaks = lat_boundaries, labels = FALSE)]
mask_dt[ , LAT_GRID := lat_centres[LAT_INDEX]]
mask_dt[ , LON_INDEX, LAT_INDEX]

# apply
fireurisk_dt <- merge.data.table(x = mask_dt[ , LON_INDEX, LAT_INDEX], y = euro_data, by = c("LON_INDEX", "LAT_INDEX"))

#### SUBSET CROPLAND & NCV ####

fireurisk_cropland_dt <- fireurisk_dt[L1 == 10 & LP1 > 95, ]
mean(fireurisk_cropland_dt[["area"]])
sum(fireurisk_cropland_dt[["area"]])



fireurisk_ncv_dt <- fireurisk_dt[L1 %in% ncv_lcc_types, ]
mean(fireurisk_ncv_dt[["area"]])
sum(fireurisk_ncv_dt[["area"]])


#### WRITE TABLES ####

write.table(fireurisk_ncv_dt, file = file.path(fry_dir, paste0("FIRECCI51_6D_NCV", ".txt")), row.names = FALSE)


stop()


#### GET QUANTILES ####

ncv_fire_sizes_ha <- fireurisk_ncv_dt[["area"]] * 100
ncv_quartiles <- quantile(x = ncv_fire_sizes_ha)
ncv_deciles <- quantile(x = ncv_fire_sizes_ha, seq(0,1,by = 0.1),)
ncv_twentyciles <- quantile(x = ncv_fire_sizes_ha, seq(0,1,by = 0.05),)

length(ncv_fire_sizes_ha[ which(ncv_fire_sizes_ha > 440)])


cropland_fire_sizes_ha <- fireurisk_cropland_dt[["area"]] * 100
cropland_quartiles <- quantile(cropland_fire_sizes_ha)

all_fire_sizes_ha_cumsum <- cumsum(all_fire_sizes_ha)



quantile(all_fire_sizes_ha_cumsum)



# large BA
num_years <- length(unique(euro_data$YR))
total_ba <- euro_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
total_ba_sum <- euro_data[, .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]

total_ba_sum <- na.omit(total_ba_sum)

total_ba_sum[ Area > 10000, Area := NA]

total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = Area))



# calculated and plot each land cover type separately
for(this_lc in sort(unique(euro_data$L1))) {
  
  total_ba <- euro_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
  total_ba_sum <- euro_data[ L1 == this_lc , .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]
  total_ba_sum <- na.omit(total_ba_sum)
  total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = Area))
  total_plot <- total_plot + scale_fill_viridis(trans = "log", option = "H")
  total_plot <- total_plot + labs(title = paste(this_lc))
  total_plot <- total_plot + coord_cartesian()
  print(total_plot)
  
}



# calculated and plot each land cover type separately
for(this_lc in sort(unique(euro_data$L1))) {
  
  #total_ba <- euro_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
  total_ba_sum <- euro_data[ L1 == this_lc & area > 10, .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]
  total_ba_sum <- na.omit(total_ba_sum)
  total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = Area))
  total_plot <- total_plot + scale_fill_viridis(trans = "log", option = "H")
  total_plot <- total_plot + labs(title = paste(this_lc))
  total_plot <- total_plot + coord_equal()
  print(total_plot)
  
  
  
}

#total_ba <- euro_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
total_ba_sum <- euro_data[ L1 != 10 & area > 2, .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]
total_ba_sum <- na.omit(total_ba_sum)
total_ba_sum[ , value := cut(`Area`, ba_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE, dig.lab =4)]
total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = value)) 
total_plot <- total_plot + scale_fill_viridis(option = "H", name = "Burnt area (ha)", discrete = TRUE, drop = FALSE) 
total_plot <- total_plot + coord_equal()
print(total_plot)

