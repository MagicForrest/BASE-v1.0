library(terra)
library(sf)
library(data.table)
library(ncdf4)
library(tictoc)

# define root path with here package and 
here::i_am("scripts/build_BASE_dt.R")
library(here)
source(here("scripts", "build_dt_helper_functions.R"))


tic()

#### SETTINGS ####

base_dir <- here("external_files", "gridded_9km/")
fwi_raw_dir <- here("external_files", "daily_fwi/")
climate_dir <- here("external_files", "era5_climate/")

version <- "9.0_new_lc_classes"

# flag to include long term means and deviations (currently not enabled becuase these weren't found to be useful) 
if_add_long_terms_means_and_deviations <- FALSE

# select which data sets to use
doLFMC <- FALSE

ref_grid_terra <- rast(file.path(base_dir, "FirEUrisk_ref_grid.nc"))
ref_grid_dt <- as.data.frame(ref_grid_terra, xy = TRUE)
setnames(ref_grid_dt, c("x", "y"), c("Lon", "Lat"))



#### FIRECCI BURNT AREA ####

# make times for the ESA FireCCI51 data
times_firecci <- seq(as.Date("2001-01-15"), as.Date("2020-12-15"), by="months")#
times_landcover <- seq(as.Date("2001-01-15"), as.Date("2020-12-15"), by="years")#


# remove the master tables from memory
rm(master_full_dt)
gc()

for(landcover_class in c("NCV", "NonFlammable", "PureCropland")) {
  
  # read the burnt area data
  firecci51_terra <- rast(file.path(base_dir, "FireCCI51", paste0("ESA_CCI_51_9km_", tolower(landcover_class) ,"_frac.tif")))
  lc_area_terra <- rast(file.path(base_dir, "FireCCI51", paste0("ESA_CCI_LC_9km_", tolower(landcover_class) ,"_frac.tif")))
  #time(firecci51_terra) <- times_firecci
  #time(lc_area_terra) <- lc_area_terra
  
  #firecci51_terra <- resample(firecci51_terra, ref_grid_terra)
  #firecci51_terra <- mask(firecci51_terra, ref_grid_terra)
  firecci51_dt <- as.data.table(as.data.frame(firecci51_terra, xy = TRUE))
  lc_area_dt <- as.data.table(as.data.frame(lc_area_terra, xy = TRUE))
  
  # melt, set Month and Year from layer names and tidy names 
  setnames(firecci51_dt, new = c("Lon", "Lat", format(times_firecci, "%Y-%m")))
  firecci51_dt_melted <- melt.data.table(firecci51_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE)
  setnames(firecci51_dt_melted, c("Lon", "Lat", "Date", paste("BurntFraction", landcover_class, sep = "_")))
  setnames(lc_area_dt, new = c("Lon", "Lat", format(times_landcover, "%Y-%m")))
  lc_area_dt_melted <- melt.data.table(lc_area_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE)
  setnames(lc_area_dt_melted, c("Lon", "Lat", "Date",  paste("LandcoverFraction", landcover_class, sep = "_")))
  
  # get FireCCI Year and Month
  firecci51_dt_melted[ , c("Year", "Month") := tstrsplit(Date, "-")]
  firecci51_dt_melted[ , Date := NULL]
  firecci51_dt_melted[ , Year := as.integer(Year)]
  firecci51_dt_melted[ , Month := as.integer(Month)]
  
  # Get land cover fraction Year
  lc_area_dt_melted[ , c("Year") := as.integer(tstrsplit(Date, "-")[[1]])]
  lc_area_dt_melted[ , Date := NULL]
  
  
  # merge and handle data
  this_lc_final_dt <- merge(firecci51_dt_melted, lc_area_dt_melted, c("Lon", "Lat", "Year"), all.x = TRUE)
  
  # round the coords to 7 decimal places to match with the next 
  this_lc_final_dt[ , Lon := round(Lon, 7)]
  this_lc_final_dt[ , Lat := round(Lat, 7)]
  
  # for all years
  if(exists("master_full_dt")){
    master_full_dt <- merge.data.table(x = master_full_dt, y = this_lc_final_dt, by = c("Lon", "Lat", "Year", "Month"), all.x = TRUE, all.y = TRUE) 
  } else {
    master_full_dt <- copy(this_lc_final_dt)
  }
  print(master_full_dt)
  
  # rm(firecci51_terra, lc_area_terra,
  #    firecci51_dt, lc_area_dt,
  #    firecci51_dt_melted, lc_area_dt_melted,
  #    this_lc_final_dt, this_lc_final_dt)
  # 
} 

# remove the gridcells where non-flammable landcover types are in the very large majority ( > 99%)
# TODO consider lower this threshold for cleaner data
master_full_dt <- master_full_dt[ LandcoverFraction_NonFlammable < 0.99, ]

# initialise tables sizes
current_row_size <- nrow(master_full_dt)
message(paste("Initial table size:", current_row_size))



#### FRY ####

# read the FRY RDS (as already prepared by Maik)
FRY_dt <- readRDS(file.path(base_dir, "FRY", "FRY_dt.rds"))
FRY_dt <- FRY_dt[, c("Lon", "Lat", "Year", "Month", "N_patches_firecci_CUTOFF12", "N_patches_firecci_CUTOFF6", "N_patches_MODIS_CUTOFF12", "N_patches_MODIS_CUTOFF6")]
FRY_dt[ , Lon := round(Lon, 7)]
FRY_dt[ , Lat := round(Lat, 7)]
FRY_dt[ , Month := as.integer(Month)]
FRY_dt[ , Year := as.integer(Year)]

# all years 
FRY_dt_full <- FRY_dt[ ,  lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Year", "Month")]
master_full_dt <- merge.data.table(x = master_full_dt, y = FRY_dt_full, by = c("Lon", "Lat", "Year", "Month"), all.x = TRUE, all.y = FALSE)

# aggregate to climatology of monthly values and merge
FRY_dt <- FRY_dt[ ,  lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month")]
FRY_dt[, Year := NULL]


# Report and tidy up
message(paste0("*** Completed FRY ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)


# special case for FRY - set NAs to zero 
master_full_dt[is.na(master_full_dt)] <- 0



#### LAND COVER ####

# read the land cover data
landcover_terra <-  rast(file.path(base_dir, "LandUse", "Lu_6bands_firEUrisk.nc"))

# select the years of interest
first_year <- 1960
last_year <- 2020
first_year_index <- first_year-1960+1
last_year_index  <- first_year_index + last_year-first_year

landuse_types <- c("Urban", "Cropland", "Forest", "Shrub", "Bareground", "Pasture")
# for land cover types

rm(lu_dt)
for(lu_band in 1:length(landuse_types)){
  
  this_lu_rast <- resample(subset(landcover_terra, paste0("landuse_lu_type=", lu_band, "_", first_year_index:last_year_index)), ref_grid_terra)
  names(this_lu_rast) <- paste(first_year:last_year)
  this_lu_dt <- as.data.table(as.data.frame(this_lu_rast, xy = TRUE))
  setnames(this_lu_dt, c("x", "y"), c("Lon", "Lat"))
  this_lu_dt <- melt.data.table(this_lu_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE)
  setnames(this_lu_dt, c("variable", "value"), c("Year", landuse_types[lu_band]))
  this_lu_dt[, Year := as.integer(Year)]
  
  # merge this LU layer to the overall
  if(exists("lu_dt")) {
    lu_dt <- merge.data.table(lu_dt, this_lu_dt, by = c("Lon", "Lat", "Year"))
  }
  else {
    lu_dt <- this_lu_dt
  }
  
  
}


# Do a little extra with the land use

# calculate Natural from Shrub and Forest and clean up the unneedded
lu_dt[ , Natural := Shrub + Forest]
lu_dt[ , Shrub := NULL]
lu_dt[ , Forest := NULL]

# calculate the land abandoment (growth of Natural, over the last 40 years)
setkey(lu_dt, Lon, Lat, Year)
lu_dt[, Natural1 := data.table::shift(Natural, n=1), by=c("Lon", "Lat")]
lu_dt[, NaturalChange := pmax(Natural - Natural1, 0)]
lu_dt[, deltaNatural40 := data.table::frollsum(NaturalChange, align = "right", n = 40), by=c("Lon", "Lat")]
lu_dt[, deltaNatural30 := data.table::frollsum(NaturalChange, align = "right", n = 30), by=c("Lon", "Lat")]
lu_dt[, deltaNatural20 := data.table::frollsum(NaturalChange, align = "right", n = 20), by=c("Lon", "Lat")]
lu_dt[, deltaNatural10 := data.table::frollsum(NaturalChange, align = "right", n = 10), by=c("Lon", "Lat")]
lu_dt[, Natural1 := NULL]
lu_dt[, NaturalChange := NULL]


# round the table off to 7 decimal places
lu_dt[ , Lon := round(Lon, 7)]
lu_dt[ , Lat := round(Lat, 7)]

# add full data
master_full_dt <- merge.data.table(x = master_full_dt, y = lu_dt, by = c("Lon", "Lat", "Year"))

# Report and tidy up
message(paste0("*** Completed Land use ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)

rm(this_lu_dt, this_lu_rast)

#### AGB

agb_terra <-  c(rast(file.path(base_dir, "Biomass", "ESACCI-BIOMASS-FirEUrisk-9km-2010-GridcellMean.nc")), rast(file.path(base_dir, "Biomass", "ESACCI-BIOMASS-FirEUrisk-9km-2010-Natural_via_CCI_Landcover_300m_grid.nc")))
names(agb_terra) <- c("AGB_Gridcell", "AGB_Natural")

agb_terra <- resample(agb_terra, ref_grid_terra)
agb_dt <- as.data.table(as.data.frame(agb_terra, xy = TRUE))
setnames(agb_dt, c("x", "y"), c("Lon", "Lat"))

# round the table off to 7 decimal places
agb_dt[ , Lon := round(Lon, 7)]
agb_dt[ , Lat := round(Lat, 7)]

# add full data
master_full_dt <- merge.data.table(x = master_full_dt, y = agb_dt, by = c("Lon", "Lat"))

# Report and tidy up
message(paste0("*** Completed tree cover ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)

rm(agb_dt, agb_terra)


#### Landsat Tree Cover ####

# take the average of 2000, 2005, 2010, 2015
tc_years <- c( 2000, 2005, 2010, 2015)
treecover_all_lc_terra <- c(mean(c(rast(file.path(base_dir, "LandSat", paste0("LandSat_tree_cover_", tc_years, "_all_frac.v2.0.tif"))))), 
                      mean(c(rast(file.path(base_dir, "LandSat", paste0("LandSat_tree_cover_", tc_years, "_natural_frac.v2.0.tif"))))), 
                      mean(c(rast(file.path(base_dir, "LandSat", paste0("LandSat_tree_cover_", tc_years, "_cropland_frac.v2.0.tif"))))), 
                      mean(c(rast(file.path(base_dir, "LandSat", paste0("LandSat_tree_cover_", tc_years, "_purecropland_frac.v2.0.tif"))))), 
                      mean(c(rast(file.path(base_dir, "LandSat", paste0("LandSat_tree_cover_", tc_years, "_pasture_frac.v2.0.tif"))))))
names(treecover_all_lc_terra) <- c("Treecover_Gridcell", 
                      "Treecover_Natural", 
                      "Treecover_Cropland", 
                      "Treecover_Purecropland", 
                      "Treecover_Pasture")

treecover_dt <- as.data.table(treecover_all_lc_terra, xy = TRUE)
setnames(treecover_dt, c("x", "y"), c("Lon", "Lat"))


# round the table off to 7 decimal places
treecover_dt[ , Lon := round(Lon, 7)]
treecover_dt[ , Lat := round(Lat, 7)]

# add full data
master_full_dt <- merge.data.table(x = master_full_dt, y = treecover_dt, by = c("Lon", "Lat"))

# Report and tidy up
message(paste0("*** Completed AGB ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)

rm(treecover_dt, treecover_dt)



#### FWI #### 

# years of FWI to process
FWI_first_year <- 2001
FWI_last_year  <- 2014
FWI_file_name <- "monthly_2001_2014_FWI_and_climate_ERA5-LAND_historical.rds"
FWI_file_path <- file.path(base_dir, "FWI", FWI_file_name)

### # make the file if it is not there
if(!file.exists(FWI_file_path)) {
  tic()
  FWI_all_years_dt <- data.table()
  for(this_year in FWI_first_year:FWI_last_year) {
    print(this_year)
    tic()
    FWI_full_dt <- as.data.table(readRDS(file.path(fwi_raw_dir, paste(this_year, "DC_ERA5-LAND_Historical.rds", sep = "_"))))
    FWI_full_dt[ , ID := NULL]
    setnames(FWI_full_dt, c("LONG", "LAT", "YR", "MON"),  c("Lon", "Lat", "Year", "Month") )
    FWI_monthly_dt <- FWI_full_dt[ , lapply(.SD, mean), by =c("Lon", "Lat", "Year", "Month")]
    FWI_monthly_dt[ , DAY := NULL]
    
    # remap the Lons and Lats.  Gawd I love how fast data.table is.
    FWI_monthly_dt <- remapDataTableToReferenceGrid(FWI_monthly_dt,ref_grid_dt )
    
    FWI_all_years_dt <- rbind(FWI_all_years_dt, FWI_monthly_dt)
    toc()
    
  } # for each year
  
  # rename to avoid confusion
  setnames(FWI_all_years_dt, "TEMP", "Tmax")
  setnames(FWI_all_years_dt, "PREC", "Prec")
  setnames(FWI_all_years_dt, "RH", "RelHum")
  setnames(FWI_all_years_dt, "WS", "WindSpeed")
  
  
  #### TEMPERATURE ####
  # also add the mean monthly temperature from a separate netcdf file
  # here I uused the quite cool remapDataTableToReferenceGrid() function instead of the normal resample and round - but I am not sure why I did this!
  temp_rast <- rast(file.path(climate_dir, "TMean_ERA5-LAND_Historical_monmean.nc"))
  temp_dt <- as.data.table(temp_rast, xy = TRUE)
  setnames(temp_dt, c("Lon", "Lat", format(time(temp_rast), "%Y-%m")))
  temp_dt <- melt.data.table(temp_dt, id.vars = c("Lon", "Lat"))
  temp_dt[ , c("Year", "Month") := tstrsplit(variable, "-")]
  temp_dt[ , variable := NULL]
  setnames(temp_dt, "value", "Temp")
  temp_dt[ , Year := as.integer(Year)]
  temp_dt[ , Month := as.integer(Month)]
  temp_dt <- remapDataTableToReferenceGrid(temp_dt,ref_grid_dt )
  FWI_and_climate_dt <- merge(FWI_all_years_dt, temp_dt, by = c("Lon","Lat", "Year", "Month"))
  
  #### SOLAR RADIATION ####
  # also add the mean monthly solar radiation from a separate netcdf file
  # here do teh same resampling and round procedures to match the grid
  rad_rast <- rast(file.path(climate_dir, "SWR_ERA5-LAND_Historical_monmean.nc"))
  rad_rast <- resample(rad_rast, ref_grid_terra, "near")
  rad_dt <- as.data.table(rad_rast, xy = TRUE)
  setnames(rad_dt, c("Lon", "Lat", format(time(rad_rast), "%Y-%m")))
  rad_dt <- melt.data.table(rad_dt, id.vars = c("Lon", "Lat"))
  rad_dt[ , c("Year", "Month") := tstrsplit(variable, "-")]
  rad_dt[ , variable := NULL]
  setnames(rad_dt, "value", "Rad")
  rad_dt[ , Year := as.integer(Year)]
  rad_dt[ , Month := as.integer(Month)]

  rad_dt[ , Lon := round(Lon, 7)]
  rad_dt[ , Lat := round(Lat, 7)]
  FWI_and_climate_dt <- merge(FWI_and_climate_dt, rad_dt, by = c("Lon","Lat", "Year", "Month"))
  
  
  print(FWI_and_climate_dt)
  saveRDS(FWI_and_climate_dt, FWI_file_path)
  toc()
  
} # if file not present



# read the data 
FWI_dt <- readRDS(FWI_file_path)

# calculate the growing season data.table
temp_dt <- FWI_dt[ , lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon","Lat", "Month"), .SDcols = c("Temp") ]
gs_dt <- temp_dt[ Temp > 5, ]
gs_dt[ , Temp := NULL]


# checks
#gs_dt_count <- gs_dt[, .(count = .N), by = c("Lon", "Lat")]
#ggplot(gs_dt_count) + geom_raster(aes(x = Lon, y = Lat, fill = count))

#### ADD CLIMATE LONG TERM MEANS AND DEVIATIONS ####
# Extract the precip, temperature and solar radiation and calclulate the some rolling means and long term deviations etc

# prec
prec_dt <- FWI_dt[ , c("Lon", "Lat", "Year", "Month", "Prec")]
setnames(prec_dt, "Prec", "value")
prec_dt[ , Date := as.Date(paste(Year, Month, 15, sep = "-"))]
setkey(prec_dt, "Lon", "Lat", "Date")
if(if_add_long_terms_means_and_deviations) prec_dt <- add_long_term_means_and_deviations(prec_dt, gs_dt, agg_fun = sum) 
setnames(prec_dt, gsub(pattern = "value", replacement = "Prec", names(prec_dt)))
prec_dt[, Date := NULL]
prec_dt[, Prec := NULL]
FWI_dt <- merge(FWI_dt, prec_dt, by = c("Lon","Lat", "Year", "Month"))

# temp
temp_dt <- FWI_dt[ , c("Lon", "Lat", "Year", "Month", "Temp")]
setnames(temp_dt, "Temp", "value")
temp_dt[ , Date := as.Date(paste(Year, Month, 15, sep = "-"))]
setkey(temp_dt, "Lon", "Lat", "Date")
tic()
if(if_add_long_terms_means_and_deviations)  temp_dt <- add_long_term_means_and_deviations(temp_dt, gs_dt, agg_fun = mean) 
toc()
setnames(temp_dt, gsub(pattern = "value", replacement = "Temp", names(temp_dt)))
temp_dt[, Date := NULL]
temp_dt[, Temp := NULL]
FWI_dt <- merge(FWI_dt, temp_dt, by = c("Lon","Lat", "Year", "Month"))

# rad
rad_dt <- FWI_dt[ , c("Lon", "Lat", "Year", "Month", "Rad")]
setnames(rad_dt, "Rad", "value")
rad_dt[ , Date := as.Date(paste(Year, Month, 15, sep = "-"))]
setkey(rad_dt, "Lon", "Lat", "Date")
tic()
if(if_add_long_terms_means_and_deviations)  rad_dt <- add_long_term_means_and_deviations(rad_dt, gs_dt, agg_fun = mean) 
toc()
setnames(rad_dt, gsub(pattern = "value", replacement = "Rad", names(rad_dt)))
rad_dt[, Date := NULL]
rad_dt[, Rad := NULL]
FWI_dt <- merge(FWI_dt, rad_dt, by = c("Lon","Lat", "Year", "Month"))



# all years 
master_full_dt <- merge.data.table(x = master_full_dt, y = FWI_dt, by = c("Lon", "Lat", "Year", "Month"))


# Report and tidy up
message(paste0("*** Completed FWI ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(FWI_dt, FWI_terra)



#### FAPAR ####

FAPAR_terra <- rast(file.path(base_dir, "FAPAR", "FAPAR_monthly_Refgrid.nc"))
FAPAR_terra <- resample(FAPAR_terra, ref_grid_terra)
FAPAR_dt <- as.data.table(as.data.frame(FAPAR_terra, xy = TRUE))
FAPAR_dt[ , x := round(x, 7)]
FAPAR_dt[ , y := round(y, 7)]
# Note:  were are explicitly deriving and keeping the Date column for the rolling means etc below
setnames(FAPAR_dt, new = c("Lon", "Lat", as.character(time(FAPAR_terra))))
FAPAR_dt <- na.omit(melt.data.table(FAPAR_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE))
FAPAR_dt[, Date := as.Date(variable)]
FAPAR_dt[, variable := NULL]
FAPAR_dt[, Year := year(Date)]
FAPAR_dt[, Month := month(Date)]

# set keys (important for the rolling means) and calculate summaries  
setkey(FAPAR_dt, "Lon", "Lat", "Date")
if(if_add_long_terms_means_and_deviations) FAPAR_dt <- add_long_term_means_and_deviations(FAPAR_dt, gs_dt)
FAPAR_dt <- add_antecedent_values(FAPAR_dt, FUN = mean)
setnames(FAPAR_dt, gsub(pattern = "value", replacement = "FAPAR", names(FAPAR_dt)))
FAPAR_dt[, Date := NULL]

# all years 
master_full_dt <- merge.data.table(x = master_full_dt, y = FAPAR_dt, by = c("Lon", "Lat", "Year", "Month"))

# report and tidy up
message(paste0("*** Completed FAPAR ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(FAPAR_dt, FAPAR_terra)

#### FUEL TYPES (static map of dominant class) #### 

# fractional covers - not yet used
#Fuel_types_terra <- rast(file.path(base_dir, "FuelTypes", "FirEUrisk_Europe_fuel_map_9km_fc.tif"))

# dominant class
Fuel_types_terra <- rast(file.path(base_dir, "FuelTypes", "FirEUrisk_Europe_fuel_map_9km.tif"))
Fuel_types_terra <- mask(Fuel_types_terra, ref_grid_terra)
Fuel_types_dt <- as.data.table(as.data.frame(Fuel_types_terra, xy = TRUE))
setnames(Fuel_types_dt, c("Lon", "Lat", "FuelClass"))

Fuel_types_dt[ , Lon := round(Lon, 7)]
Fuel_types_dt[ , Lat := round(Lat, 7)]

master_full_dt <- merge.data.table(master_full_dt, Fuel_types_dt, by = c("Lon", "Lat"))

# Report and tidy up
message(paste0("*** Completed fuel types ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(Fuel_types_terra)


#### SPECIES TYPES

Species_terra <- rast(file.path(base_dir, "Brus_Species", "Brus_Dominant_Species_9km.tif"))
Species_terra <- mask(Species_terra, ref_grid_terra)
Species_dt <- as.data.table(as.data.frame(Species_terra, xy = TRUE))
setnames(Species_dt, c("Lon", "Lat", "Dominant_species"))
rm(Species_terra)

Group_terra <- rast(file.path(base_dir, "Brus_Species", "Brus_Dominant_Category_9km.tif"))
Group_terra <- mask(Group_terra, ref_grid_terra)
Group_dt <- as.data.table(as.data.frame(Group_terra, xy = TRUE))
setnames(Group_dt, c("Lon", "Lat", "Dominant_type"))
rm(Group_terra)

all_Brus_dt <- merge(Group_dt, Species_dt)
all_Brus_dt[ , Lon := round(Lon, 7)]
all_Brus_dt[ , Lat := round(Lat, 7)]

master_full_dt <- merge.data.table(master_full_dt, all_Brus_dt, by = c("Lon", "Lat"))

# Report and tidy up
message(paste0("*** Completed species types ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(Fuel_types_terra)


#### SOCIOECONOMICS ####

GDP_terra <- rast(file.path(base_dir, "SocioEconomic", "GDP_2005_FirEURisk_Grid.nc"))
GDP_terra <- mask(GDP_terra, ref_grid_terra)
GDP_terra <- resample(GDP_terra, ref_grid_terra)
GDP_dt <- as.data.table(as.data.frame(GDP_terra, xy = TRUE))
setnames(GDP_dt, c("Lon", "Lat", "GDP_gridcell"))
rm(GDP_terra)

RoadDens_terra <- rast(file.path(base_dir, "SocioEconomic", "RD_DENS_GRIP4_FirEURisk_Grid_2018.nc"))
RoadDens_terra <- mask(RoadDens_terra, ref_grid_terra)
RoadDens_terra <- resample(RoadDens_terra, ref_grid_terra)
RoadDens_dt <- as.data.table(as.data.frame(RoadDens_terra, xy = TRUE))
setnames(RoadDens_dt, c("Lon", "Lat", "RoadDens"))
socioeconomic_dt <- merge.data.table(GDP_dt, RoadDens_dt, by = c("Lon", "Lat"))
rm(RoadDens_terra)

PopDens_terra <- rast(file.path(base_dir, "SocioEconomic", "POPDENSITY_SSP1_2020.FirEURisk.nc"))
PopDens_terra <- mask(PopDens_terra, ref_grid_terra)
PopDens_terra <- resample(PopDens_terra, ref_grid_terra)
PopDens_dt <- as.data.table(as.data.frame(PopDens_terra, xy = TRUE))
setnames(PopDens_dt, c("Lon", "Lat", "PopDens"))
socioeconomic_dt <- merge.data.table(socioeconomic_dt, PopDens_dt, by = c("Lon", "Lat"))
rm(PopDens_terra)


socioeconomic_dt[ , Lon := round(Lon, 7)]
socioeconomic_dt[ , Lat := round(Lat, 7)]
master_full_dt <- merge.data.table(master_full_dt, socioeconomic_dt, by = c("Lon", "Lat"))

# Report and tidy up
message(paste0("*** Completed Socioeconomic***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(socioeconomic_dt, GDP_dt, RoadDens_dt, PopDens_dt)

#### KUMMU HDI ####

HDI_terra <- rast(file.path(base_dir, "HDI", "Kummu_HDI_FirEUrisk.nc"))
HDI_dt <- as.data.table(as.data.frame(HDI_terra, xy = TRUE))
setnames(HDI_dt, new = c("Lon", "Lat", as.character(time(HDI_terra))))
HDI_dt <- melt(HDI_dt, id.vars = c("Lon", "Lat"))
setnames(HDI_dt, c("variable", "value"), c("Date", "HDI"))
HDI_dt[, Date := as.Date(Date)]
HDI_dt[, Year := year(Date)]
HDI_dt[, Date := NULL]

HDI_dt[ , Lon := round(Lon, 7)]
HDI_dt[ , Lat := round(Lat, 7)]
master_full_dt <- merge.data.table(master_full_dt, HDI_dt, by = c("Lon", "Lat", "Year"))



# Report and tidy up
message(paste0("*** Completed HDI ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(HDI_dt, HDI_terra)


#### LFMC #### 

if(doLFMC){
  
  LFMC_terra <- rast(file.path(base_dir, "LFMC", "LMFC_monthly_Refgrid.nc"))
  LFMC_terra <- resample(LFMC_terra, ref_grid_terra)
  LFMC_dt <- as.data.table(as.data.frame(LFMC_terra, xy = TRUE))
  setnames(LFMC_dt, new = c("Lon", "Lat", as.character(time(LFMC_terra))))
  LFMC_dt <- melt.data.table(LFMC_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE)
  LFMC_dt[, Date := as.Date(variable)]
  LFMC_dt[, variable := NULL]
  LFMC_dt[, Year := year(Date)]
  LFMC_dt[, Month := month(Date)]
  LFMC_dt[, Date := NULL]
  setnames(LFMC_dt, "value", "LFMC")
  
  LFMC_dt[ , Lon := round(Lon, 7)]
  LFMC_dt[ , Lat := round(Lat, 7)]
  
  # TODO IMPLEMENT THIS TING
  
}

#### TERRAIN ####

all_terrain_terra <- rast()

# merged geomorpho file
mergedgeomorpho_sf <- read_sf(file.path(base_dir, "Terrain", "MergedGeomorpho90.shp"))
for(this_layer in names(mergedgeomorpho_sf)[!names(mergedgeomorpho_sf) %in% c("geometry")]){
  print(this_layer)
  all_terrain_terra <- c(all_terrain_terra, terra::rasterize(x = mergedgeomorpho_sf, y = ref_grid_terra, field = this_layer))
  #plot(all_terrain_terra[[this_layer]])
}

# merged merit file
# only use the following variables suggested by Luke
merit_vars <- c("elevation", "aspectCos", "aspectSin", "eastness", "northness", "northeastC", "northeastn", "NHSunFrac", "slope20Fra", "unmaskedAr", "reliefEn") 
mergedmerit_sf <- read_sf(file.path(base_dir, "Terrain", "MergedMERIT.shp"))
for(this_layer in merit_vars){
  print(this_layer)
  all_terrain_terra <- c(all_terrain_terra, terra::rasterize(x = mergedmerit_sf, y = ref_grid_terra, field = this_layer))
  #plot(all_terrain_terra[[this_layer]])
}

# convert to a data.table and merge
all_terrain_dt <-  na.omit(as.data.table(as.data.frame(all_terrain_terra, xy = TRUE)))
# NOTE: with this command we are swapping eastness and northness to correct an error in the dataset 
setnames(all_terrain_dt, 
         c("x", "y", "vrm", "cti", "tri", "slope", "tpi", "elevation", "aspectCos", "aspectSin", "eastness", "northness", "northeastC", "northeastn", "NHSunFrac", "slope20Fra", "unmaskedAr", "reliefEn"), 
         c("Lon", "Lat", "VRM", "CTI", "TRI", "Slope", "TPI", "Elevation", "AspectCos", "AspectSin", "Northness", "Eastness", "NortheastCos", "Northeastness", "SunFraction", "Slope20Fraction", "UnmaskedArea", "ReliefEnergy"))
all_terrain_dt[ , Lon := round(Lon, 7)]
all_terrain_dt[ , Lat := round(Lat, 7)]
master_full_dt <- merge.data.table(master_full_dt, all_terrain_dt, by = c("Lon", "Lat"))

message(paste0("*** Completed terrain ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(all_terrain_dt, all_terrain_terra)




#### GPP ####

GPP_terra <- rast(file.path(base_dir, "GPP", "GOSIF_GPP_FirEUrisk.nc"))
GPP_terra <- resample(GPP_terra, ref_grid_terra)
GPP_dt <- as.data.table(as.data.frame(GPP_terra, xy = TRUE))
GPP_dt[ , x := round(x, 7)]
GPP_dt[ , y := round(y, 7)]
# Note:  were are explicitly deriving and keeping the Date column for the rolling means etc below
setnames(GPP_dt, new = c("Lon", "Lat", as.character(time(GPP_terra))))
GPP_dt <- na.omit(melt.data.table(GPP_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE))
GPP_dt[, Date := as.Date(variable)]
GPP_dt[, variable := NULL]
GPP_dt[, Year := year(Date)]
GPP_dt[, Month := month(Date)]

# set keys (important for the rolling means) and calculate summaries  
setkey(GPP_dt, "Lon", "Lat", "Date")
if(if_add_long_terms_means_and_deviations) GPP_dt <- add_long_term_means_and_deviations(GPP_dt, gs_dt, agg_fun = sum)
GPP_dt <- add_antecedent_values(GPP_dt, FUN = sum)
setnames(GPP_dt, gsub(pattern = "value", replacement = "GPP", names(GPP_dt)))
GPP_dt[, Date := NULL]

# all years 
master_full_dt <- merge.data.table(x = master_full_dt, y = GPP_dt, by = c("Lon", "Lat", "Year", "Month"))


message(paste0("*** Completed GPP ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(GPP_dt, GPP_terra)


#### GPP LPJmL ####

GPP_LPJmL_terra <- rast(file.path(base_dir, "GPP_LPJml", "mgpp.nc"))
GPP_LPJmL_terra <- resample(GPP_LPJmL_terra, ref_grid_terra)
GPP_LPJmL_dt <- as.data.table(as.data.frame(GPP_LPJmL_terra, xy = TRUE))
GPP_LPJmL_dt[ , x := round(x, 7)]
GPP_LPJmL_dt[ , y := round(y, 7)]
# Note:  were are explicitly deriving and keeping the Date column for the rolling means etc below
setnames(GPP_LPJmL_dt, new = c("Lon", "Lat", as.character(time(GPP_LPJmL_terra))))
GPP_LPJmL_dt <- na.omit(melt.data.table(GPP_LPJmL_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE))
GPP_LPJmL_dt[, Date := as.Date(variable)]
GPP_LPJmL_dt[, variable := NULL]
GPP_LPJmL_dt[, Year := year(Date)]
GPP_LPJmL_dt[, Month := month(Date)]

# set keys (important for the rolling means) and calculate summaries  
setkey(GPP_LPJmL_dt, "Lon", "Lat", "Date")
if(if_add_long_terms_means_and_deviations) GPP_LPJmL_dt <- add_long_term_means_and_deviations(GPP_LPJmL_dt, gs_dt, agg_fun = sum)
GPP_LPJmL_dt <- add_antecedent_values(GPP_LPJmL_dt, FUN = sum)
setnames(GPP_LPJmL_dt, gsub(pattern = "value", replacement = "GPP_LPJmL", names(GPP_LPJmL_dt)))
GPP_LPJmL_dt[, Date := NULL]

# for(year in 2000:2014){
#   this_plot <- ggplot(GPP_combi[ Year == year,]) + geom_raster(aes(x = Lon, y = Lat, fill = rel_GPP_dev)) + scale_fill_distiller(palette = "RdBu", type = "div", limits = c(-1,1))
#   this_plot <- this_plot + labs(title = year)
#    print(this_plot) 
# }


# all years 
master_full_dt <- merge.data.table(x = master_full_dt, y = GPP_LPJmL_dt, by = c("Lon", "Lat", "Year", "Month"))

message(paste0("*** Completed LPJml GPP ***"))
current_row_size <- check_row_count(full_dt = master_full_dt, previous_row_count = current_row_size)
rm(GPP_LPJmL_dt, GPP_LPJmL_terra)


#### FINAL POST-PROCESSING ####

# calculate GDP per capita
# this catch cases where
master_full_dt[, GDP_capita := pmin(Inf, GDP_gridcell/PopDens)]
master_full_dt$GDP_capita[master_full_dt$PopDens == 0] <- 0


#### SAVE FILES ####
saveRDS(master_full_dt, file.path(base_dir, paste0("master_full_dt_", version, ".rds")))

print(names(master_full_dt))

toc()



