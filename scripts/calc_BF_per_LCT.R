library(terra)
library(tictoc)


# define root path with here package and 
here::i_am("scripts/calc_BF_per_LCT.R")
library(here)

firecci_area <- 3

analysis_version <- "BASE_v1.0"


# FireCCI51 pixel data dir
fire_dir <- here("external_files/firecci51_pixel_data")

# pre-processed land cover data
landcover_dir <- here("external_files/links/landcover_cci_raw")

# FireEUrisk target grid and the output directory
target_grid <- rast(here("external_files", "links", "gridded_9km", "FirEUrisk_ref_grid.nc"))
output_dir <-   here("bf_per_lct", analysis_version)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


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
  
  
  # # Non-cropland vegetation (NCV)
  # NCV = list(name = "NCV", subclasses = c(50, # broadleaved evergreen tree
  #                                         60, 61, 62, # broadleaved deciduous tree
  #                                         70, 71, 72, # needleaved evergreen tree
  #                                         80, 81, 82, # needleaved deciduous tree
  #                                         90, # mixed leaftype tree
  #                                         100, # mosaic woody-herbaceous 
  #                                         110, # mosaic herbaceous-woody
  #                                         120, 121, 122, # shrublands
  #                                         130, # grassland
  #                                         140, # lichens and mosses
  #                                         150, 151, 152, 153,  # sparse vegetation
  #                                         160, 170, 180)), # flooded vegetation types
  # 
  # Non-burnable (ie fragmentors)
  # NonFlammable = list(name = "NonFlammable", subclasses = c(200, 201, 202, # bare areas
  #                                                           210, # water bodies
  #                                                           220)), # permanent snow and ice
  # 
  # 
  # Woodlands and Shrublands (WaS) - AKA - "Natural"
  WaS = list(name = "WaS", subclasses = c(50, # broadleaved evergreen tree
                                          60, 61, 62, # broadleaved deciduous tree
                                          70, 71, 72, # needleaved evergreen tree
                                          80, 81, 82, # needleaved deciduous tree
                                          90, # mixed leaftype tree
                                          120, 121, 122, # shrublands
                                          140, # lichens and mosses
                                          150, 151, 152, 153,  # sparse vegetation
                                          160, 170, 180)), # flooded vegetation types
  
  # 
  # Grassland
  # AllGrassland = list(name = "AllGrassland", subclasses =c(100, 110, 130)), # grassland and mosaic herbaceous-woody
  
  # 
  # # Mosaics grassland
  # MosaicGrasslandDominated= list(name = "MosaicGrasslandDominated", subclasses =c(110)), # grassland dominated mosiac 
  # 
  # # Mosaics grassland
  # MosaicWoodyDominated= list(name = "MosaicWoodyDominated", subclasses =c(100)) #  woody dominated mosiac
  
  # # Shrublands
  # Shrublands= list(name = "Shrublands", subclasses =c(120, 121, 122)), #  shrublands
  # 
  # # Woodlands 
  # Woodlands = list(name = "Woodlands", subclasses = c(50, # broadleaved evergreen tree
  #                                         60, 61, 62, # broadleaved deciduous tree
  #                                         70, 71, 72, # needleaved evergreen tree
  #                                         80, 81, 82, # needleaved deciduous tree
  #                                         90)), # mixed leaftype tree
  # 
  # Grassland
  Grasslands = list(name = "Grasslands", subclasses =c(130)), # pure grassland only
  
  # Natural mosaics 
  NaturalMosaics= list(name = "NaturalMosaics", subclasses =c(100, 110)) #  both woody and grass dominated mosaics
  
  # # Sparse vegetation types
  # Sparse = list(name = "Sparse", subclasses = c(150, 151, 152, 153)), # sparse vegetation
  
  
  
  # 
  # # Fully inclsuive cropland
  # Cropland = list(name = "Cropland", subclasses =c(10, 11, 12, # rainfed crops
  #                                                  20, # crops (irrigated/post flooding)
  #                                                  30, 40)), # crop/natural mosaics
  # # - without mosiacs or woody
  # PureCropland = list(name = "PureCropland", subclasses = c(10, 11, # non-woody rainfed crops
  #                                                           20)), # crops (irrigated/post flooding),
  # # - mosiacs only
  # MosaicCropland = list(name = "MosaicCropland", subclasses = c(30, 40)), # crop/natural mosaics
  # # - woody only
  # WoodyCropland = list(name = "WoodyCropland", subclasses = c(12)), # woody
  # 
  # # Urban
  # Urban = list(name = "Urban", subclasses = c(190)), 
  # 

  # 
  # # Everything burnable
  # All = list(name = "All", subclasses = c(10, 11, 12, # rainfed crops
  #                                         20, # crops (irrigated/post flooding)
  #                                         30, 40, # crop/natural mosaics
  #                                         50, # broadleaved evergreen tree
  #                                         60, 61, 62, # broadleaved deciduous tree
  #                                         70, 71, 72, # needleaved evergreen tree
  #                                         80, 81, 82, # needleaved deciduous tree
  #                                         90, # mixed leaftype tree
  #                                         100, # mosaic woody-herbaceous 
  #                                         110, # mosaic herbacious-woody
  #                                         120, 121, 122, # shrublands
  #                                         130, # grassland
  #                                         140, # lichens and mosses
  #                                         150, 151, 152, 153,  # sparse vegetation
  #                                         160, 170, 180, # flooded vegetation types
  #                                         190)) # urban
  
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
  writeRaster(x = this_lc_burnt_fraction, filename = file.path(output_dir, paste0("ESA_CCI_51_9km_", tolower(this_LC_class$name), "_frac.tif")), overwrite=TRUE)
  writeRaster(x = landcover_FirEUrisk_area_fraction, filename = file.path(output_dir, paste0("ESA_CCI_LC_9km_", tolower(this_LC_class$name), "_frac.tif")), overwrite=TRUE)
  
  
  print(this_lc_burnt_fraction)
  
  message(paste("Finished LC class", this_LC_class$name))
  toc()
  
  rm(landcover_FirEUrisk_area_fraction, this_lc_area_fraction, this_lc_burnt_fraction)
  gc()
  
}
