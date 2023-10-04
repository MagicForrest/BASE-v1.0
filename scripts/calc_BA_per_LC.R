library(terra)
library(tictoc)


# define root path with here package and 
here::i_am("scripts/calc_BA_per_LC.R")
library(here)

firecci_area <- 3

# FireCCI51 pixel data dir
fire_dir <- here("external_files/firecci51_pixel_data")

# pre-processed  land cover data
landcover_dir <- here("external_files/landcover_cci_raw")
  
# FireEUrisk target grid and the output directory
base_dir <- here("external_files", "gridded_9km")
target_grid <- rast(file.path(base_dir, "FirEUrisk_ref_grid.nc"))
output_dir <-   file.path(base_dir, "FireCCI51")



# years to process
first_year <- 2001
last_year <- 2020

# FireCCI grid (to regrid the landcover data too) and grid of 1s to get total number of gridcells
firecci_grid <- rast(file.path(fire_dir, paste(first_year),  paste0(first_year, "01", "01-ESACCI-L3S_FIRE-BA-MODIS-AREA_", firecci_area ,"-fv5.1-LC.tif")))

all_1s <- firecci_grid
all_1s[] <- 1
num_pixels_rast <- resample(all_1s, target_grid, method = "sum", threads = TRUE)


months <- c(paste0(0,1:9), "10", "11", "12" )

# land cover classes
all_LC_classes <- list(
  
  list(name = "Natural", subclasses = c(50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 
                                     100, 160, 170, 120, 121, 122, 140, 180, 150, 151, 152, 153)),
  list(name = "Cropland", subclasses =c(10, 11, 12, 20, 30, 40)),
  list(name = "Pasture", subclasses =c(100, 130)),
  list(name = "Urban", subclasses = c(190)),
  list(name = "PureCropland", subclasses = c(10, 11, 20))
  
)

# temporary check

for(this_LC_class in all_LC_classes) {
  
  
  # classification matrix for this LC type (classify them all to 1, the rest are set to 0 with the "others" argument)
  reclass_matrix <- as.matrix(data.frame(from = this_LC_class$subclasses, to = rep(1, length(this_LC_class$subclasses))))
  
  
  this_lc_burnt_fraction <- rast()
  this_lc_area_fraction <- rast()
  
  # process each year separately
  
  for(year in first_year:last_year){ 
    
    message(paste("************", year, "************" ))

    # read the LC data and NN aggregated match it to the FireCII
    if(year %in% 1992:2015) {
      landcover_orignal_res <- rast(file.path(landcover_dir, paste0("ESACCI-LC-L4-LCCS-Map-300m-P1Y-", year, "-v2.0.7cds.nc")))
    } else {
      landcover_orignal_res <- rast(file.path(landcover_dir, paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-", year ,"-v2.1.1.nc")))
    }
    tic()
    landcover_firecci_res <- resample(landcover_orignal_res[["lccs_class"]], firecci_grid, method = "near", threads = TRUE  )
    message("Resampled (NN) LC to FireCCI grid")
    toc()
    
    # reclassify pixels (only LC type we want)
    tic()
    landcover_firecci_res_reclassified <- classify(landcover_firecci_res, reclass_matrix, others = 0) # reclassifying land cover data
    message("Reclassified LC data")
    toc()
    
    # resample sum to get number of pixels of that LC type in that FirEUrisk 9km grid
    tic()
    landcover_FirEUrisk_sum <- resample(landcover_firecci_res_reclassified, target_grid, method = "sum", threads = TRUE) # reclassifying land cover data
    message("Resample (sum) LC data to FirEUrisk grid")
    toc()
    
    # set values with less that 5 pixels to NA and save
    landcover_FirEUrisk_sum[landcover_FirEUrisk_sum < 5] <- NA
    add(this_lc_area_fraction) <- landcover_FirEUrisk_sum
    
    
    # read each month o LC_classified burnt pixels
    for(month in months) {
      
      message(paste("*******", month, "*******" ))
      
      # read this month
      this_filename <- file.path(fire_dir, year,  paste0(year, month,"01-ESACCI-L3S_FIRE-BA-MODIS-AREA_", firecci_area ,"-fv5.1-LC.tif"))
      this_fire_rast <- rast(this_filename)
      
      # reclassify fire data
      tic()
      this_fire_rast_reclass <- classify(this_fire_rast, reclass_matrix, others = 0) 
      message("Reclassified fire data")
      toc()
      
      # sum burnt pixels
      tic()
      this_fire_FirEUrisk_sum <- resample(this_fire_rast_reclass, target_grid, method = "sum", threads = TRUE) # reclassifying land cover data
      message("Resamples (sum) this months to FirEUrisk grid")
      toc()
      
      # divide pixel counts to get burnt fraction of landcover type
      tic()
      this_burnt_fraction <- this_fire_FirEUrisk_sum/landcover_FirEUrisk_sum
      message("Division to get burnt fraction done.")
      toc()
      
      print(this_burnt_fraction)
      #plot(this_burnt_fraction)    
      
      
      add(this_lc_burnt_fraction) <- this_burnt_fraction
      
      rm(this_fire_rast, 
         this_fire_rast_reclass, 
         this_fire_FirEUrisk_sum, 
         this_burnt_fraction)
      gc()
      
      
    }
    
    
  }
  
  landcover_FirEUrisk_area_fraction <- this_lc_area_fraction / num_pixels_rast
  
  # add times and names
  time(this_lc_burnt_fraction) <- seq(as.Date(paste0(first_year, "-01-01")), as.Date(paste0(last_year, "-12-01")), by = "month")
  time(landcover_FirEUrisk_area_fraction) <- seq(as.Date(paste0(first_year, "-01-01")), as.Date(paste0(last_year, "-12-01")), by = "year")
  
  # save everything
  writeRaster(x = this_lc_burnt_fraction, filename = file.path(output_dir, paste0("ESA_CCI_51_9km_", tolower(this_LC_class$name), "_frac.v2.0.tif")), overwrite=TRUE)
  writeRaster(x = landcover_FirEUrisk_area_fraction, filename = file.path(output_dir, paste0("ESA_CCI_LC_9km_", tolower(this_LC_class$name), "_frac.v2.0.tif")), overwrite=TRUE)
  
  
  print(this_lc_burnt_fraction)

  message(paste("Finished LC class", this_LC_class$name))
  toc()
  
  rm(landcover_FirEUrisk_area_fraction, this_lc_area_fraction, this_lc_burnt_fraction)
  gc()
 
}
