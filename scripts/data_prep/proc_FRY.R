library(data.table)
library(terra)
library(viridis)
# define root path with here package and 
here::i_am("scripts/tests/proc_FRY.R")
library(here)
source(here("scripts", "build_dt_helper_functions.R"))

ba_cuts <- c(0,5,10,20,50,100,200,500,1000,2000,5000)
ba_cols <- turbo(length(ba_cuts)-1)

#### SETTINGS ####

base_dir <- here("external_files", "gridded_9km/")
fwi_raw_dir <- here("external_files", "daily_fwi/")
climate_dir <- here("external_files", "era5_climate/")


# read the data
tic()
all_data <- fread("/data/shared/Fire/FRY/v2.0/tables/FIRECCI51_plain_table_6D.txt.gz")
toc()

# get target grid and make the cuts and lookup for longitude and lat
ref_grid_terra <- rast(file.path(base_dir, "FirEUrisk_ref_grid.nc"))
lon_boundaries <- xmin(ref_grid_terra) + 0:ncol(ref_grid_terra)*res(ref_grid_terra)[1]
lon_centres <- xmin(ref_grid_terra) + 1:ncol(ref_grid_terra)*res(ref_grid_terra)[1] - res(ref_grid_terra)[1]/2
lat_boundaries <- ymin(ref_grid_terra) + 0:nrow(ref_grid_terra)*res(ref_grid_terra)[2]
lat_centres <- ymin(ref_grid_terra) + 1:nrow(ref_grid_terra)*res(ref_grid_terra)[2] - res(ref_grid_terra)[2]/2


area3_patches <- grep('AREA_3', all_data$ptch_id) 
area3_data <- all_data[area3_patches,]


# get the gridcell coords
area3_data[ , LON_INDEX := cut(x = LON, breaks = lon_boundaries, labels = FALSE)]
area3_data[ , LON_GRID := lon_centres[LON_INDEX]]
area3_data[ , LAT_INDEX := cut(x = LAT, breaks = lat_boundaries, labels = FALSE)]
area3_data[ , LAT_GRID := lat_centres[LAT_INDEX]]

# get the month
area3_data[ , MONTH := month(minBD)]
 
# large BA
num_years <- length(unique(area3_data$YR))
total_ba <- area3_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
total_ba_sum <- area3_data[, .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]

total_ba_sum <- na.omit(total_ba_sum)

total_ba_sum[ Area > 10000, Area := NA]

total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = Area))



# calculated and plot each land cover type separately
for(this_lc in sort(unique(area3_data$L1))) {
  
  total_ba <- area3_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
  total_ba_sum <- area3_data[ L1 == this_lc , .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]
  total_ba_sum <- na.omit(total_ba_sum)
  total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = Area))
  total_plot <- total_plot + scale_fill_viridis(trans = "log", option = "H")
  total_plot <- total_plot + labs(title = paste(this_lc))
  total_plot <- total_plot + coord_cartesian()
  print(total_plot)
  
}



# calculated and plot each land cover type separately
for(this_lc in sort(unique(area3_data$L1))) {
  
  #total_ba <- area3_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
  total_ba_sum <- area3_data[ L1 == this_lc & area > 10, .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]
  total_ba_sum <- na.omit(total_ba_sum)
  total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = Area))
  total_plot <- total_plot + scale_fill_viridis(trans = "log", option = "H")
  total_plot <- total_plot + labs(title = paste(this_lc))
  total_plot <- total_plot + coord_equal()
  print(total_plot)
  
  
  
}

#total_ba <- area3_data[, .(Area = sum(area)), by = c("LON_GRID", "LAT_GRID", "MONTH", "YR")]
total_ba_sum <- area3_data[ L1 != 10 & area > 2, .(Area = sum(area)/num_years * 100), by = c("LON_GRID", "LAT_GRID")]
total_ba_sum <- na.omit(total_ba_sum)
total_ba_sum[ , value := cut(`Area`, ba_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE, dig.lab =4)]
total_plot <- ggplot(total_ba_sum) + geom_raster(aes(x=LON_GRID, y = LAT_GRID, fill = value)) 
total_plot <- total_plot + scale_fill_viridis(option = "H", name = "Burnt area (ha)", discrete = TRUE, drop = FALSE) 
total_plot <- total_plot + coord_equal()
print(total_plot)

