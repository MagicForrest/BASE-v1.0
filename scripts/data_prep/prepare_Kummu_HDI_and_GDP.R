library(terra)

base_dir <- "/storage/shared/FirEUrisk/Gridded_9km"
ref_grid_terra <- rast(file.path(base_dir, "FirEUrisk_ref_grid.nc"))

first_year <- 1990
last_year <- 2015

### HDI
this_file <- file.path("/data/mforrest/GDP/Kummu2018/", "HDI_1990_2015_v2.nc")
this_HDI_rast <- rast(this_file)
this_HDI_rast <- resample(this_HDI_rast, ref_grid_terra, method = "average")
time(this_HDI_rast) <- as.Date(paste(first_year:last_year, "07", "15", sep = "-"))
writeCDF(x = this_HDI_rast, filename =  file.path("/storage/shared/FirEUrisk/Gridded_9km/HDI_GDP", "Kummu_HDI_FirEUrisk.nc"))

### GDP
this_file <- file.path("/data/mforrest/GDP/Kummu2018/", "GDP_per_capita_PPP_1990_2015_v2.nc")
this_HDI_rast <- rast(this_file)
this_HDI_rast <- resample(this_HDI_rast, ref_grid_terra, method = "average")
time(this_HDI_rast) <- as.Date(paste(first_year:last_year, "07", "15", sep = "-"))
writeCDF(x = this_HDI_rast, filename =  file.path("/storage/shared/FirEUrisk/Gridded_9km/HDI_GDP", "Kummu_GDP_FirEUrisk.nc"))