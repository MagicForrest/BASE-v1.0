library(terra)
library(tictoc)

# 
firecci_area <- 3

# Tree cover pixel data dir
tree_cover_dir <- "/data/shared/VegetationCover/Landsat"

# pre-processed land cover data
landcover_dir <- "/data/mforrest/LandUseLandCover/ESACCI/raw"

# FireEUrisk target grid
target_grid <- rast("/storage/shared/FirEUrisk/FirEUrisk_ref_grid.nc")

# output directory
output_dir <- "/storage/shared/FirEUrisk/Gridded_9km/LandSat"


# years to process
years_to_process <- c(2000, 2005, 2010, 2015)

# FireCCI grid (to regrid the landcover data too) and gris of 1s to get total number of gridcells
firecci_grid <- rast(file.path("/data/shared/Fire/FireCCI51/pixel/", "2007", "20071201-ESACCI-L3S_FIRE-BA-MODIS-AREA_3-fv5.1-LC.tif"))



# land cover classes
all_LC_classes <- list(
  
  Natural = list(name = "Natural", subclasses = c(50, # broadleaved evergreen tree
                                                  60, 61, 62, # broadleaved deciduous tree
                                                  70, 71, 72, # needleaved evergreen tree
                                                  80, 81, 82, # needleaved deciduous tree
                                                  90, # mixed leaftype tree
                                                  100, # mosaic woody-herbaceous 
                                                  120, 121, 122, # shrublands
                                                  140, # lichens and mosses
                                                  150, 151, 152, 153,  # sparse vegetation
                                                  160, 170, 180)), # flooded vegetation types
  All = list(name = "All", subclasses = c(10, 11, 12, # rainfed crops
                                          20, # crops (irrigated/post flooding)
                                          30, 40, # crop/natural mosaics
                                          50, # broadleaved evergreen tree
                                          60, 61, 62, # broadleaved deciduous tree
                                          70, 71, 72, # needleaved evergreen tree
                                          80, 81, 82, # needleaved deciduous tree
                                          90, # mixed leaftype tree
                                          100, # mosaic woody-herbaceous 
                                          110, # mosaic herbacious-woody
                                          120, 121, 122, # shrublands
                                          130, # grassland
                                          140, # lichens and mosses
                                          150, 151, 152, 153,  # sparse vegetation
                                          160, 170, 180, # flooded vegetation types
                                          190)), # urban
  list(name = "Cropland", subclasses =c(10, 11, 12, 20, 30, 40)),
  list(name = "Pasture", subclasses =c(100, 110, 130)),
  list(name = "PureCropland", subclasses = c(10, 11, 20))
  
)

# temporary check

for(this_LC_class in all_LC_classes) {
  
  
  # classification matrix for this LC type (classify them all to 1, the rest are set to 0 with the "others" argument)
  reclass_matrix <- as.matrix(data.frame(from = this_LC_class$subclasses, to = rep(1, length(this_LC_class$subclasses))))
  
  
  this_lc_tree_cover <- rast()

  # process each year separately
  
  for(year in years_to_process){ 
    
    message(paste("************", year, "************" ))
    
    # read the LC data and crop it to the FirEUrisk extent
    if(year %in% 1992:2015) {
      landcover_orignal_res <- rast(file.path(landcover_dir, paste0("ESACCI-LC-L4-LCCS-Map-300m-P1Y-", year, "-v2.0.7cds.nc")))
    } else {
      landcover_orignal_res <- rast(file.path(landcover_dir, paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-", year ,"-v2.1.1.nc")))
    }
    tic()
    landcover_cropped <- crop(landcover_orignal_res[["lccs_class"]], target_grid)
    message("Cropped LC to FireCCI grid")
    toc()
    
  
    # reclassify pixels (only LC type we want)
    tic()
    landcover_cropped_reclassified <- classify(landcover_cropped, reclass_matrix, others = NA) # reclassifying land cover data
    message("Reclassified LC data")
    toc()
    
    # read this year of tree cover data
    this_filename <- file.path(tree_cover_dir, paste0("Tree_cover_global_", year, "_250m.tif"))
    this_tree_cover_rast <- rast(this_filename)
    
    # regrid it to the 300m land cover grid
    # TODO Get this directly at 300m resolution from GEE
    tic()
    this_tree_cover_rast <- resample(this_tree_cover_rast, landcover_cropped_reclassified, method = "average", threads = TRUE)
    toc()
    message("Tree cover data read and resampled to 300m grid LC grid.")
    
    
    # set tree cover pixels which are not of the land cover class to NA 
    tic()
    this_tree_cover_rast_masked <- mask(this_tree_cover_rast, landcover_cropped_reclassified)
    toc()
    message("Masked out other land cover types")
    
    print(this_tree_cover_rast_masked)
    print(landcover_cropped_reclassified)
    
    
    # average tree cover pixels from 300m to 9km
    tic()
    this_tree_cover_FirEUrisk_mean <- resample(this_tree_cover_rast_masked, target_grid, method = "average", threads = TRUE) # reclassifying land cover data
    message("Resamples (mean) this years tree over to FirEUrisk grid")
    toc()
    
    # sum the number of land cover pixels and mask out tre cover where there are less than 5 pixels per gridcell
    tic()
    landcover_count <- resample(landcover_cropped_reclassified, target_grid, method = "sum")
    landcover_count[landcover_count < 6] <- NA
    this_tree_cover_FirEUrisk_mean <- mask(this_tree_cover_FirEUrisk_mean, landcover_count)
    message("Masked out tree cover where less than 6 pixels of the landcover type.")
    toc()
 
   
    rm(this_tree_cover_rast)
    gc()
   
    
    # write each year
    # save everything
    writeRaster(x = this_tree_cover_FirEUrisk_mean, filename = file.path(output_dir, paste("LandSat_tree_cover", year,  tolower(this_LC_class$name), "frac.v2.0.tif", sep = "_")), overwrite=TRUE)
    #writeRaster(x = landcover_FirEUrisk_area_fraction, filename = file.path(output_dir, paste0("ESA_CCI_LC_9km_", tolower(this_LC_class$name), "_frac.with_landsat_treecover.tif")), overwrite=TRUE)
    
    
    rm(this_lc_tree_cover, this_tree_cover_FirEUrisk_mean, landcover_count)
    gc()
    
  }
  
 
  

  message(paste("Finished LC class", this_LC_class$name))
  toc()
  
 
}
