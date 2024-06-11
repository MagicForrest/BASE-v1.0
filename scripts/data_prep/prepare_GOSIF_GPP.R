library(terra)


base_dir <- "/storage/shared/FirEUrisk/Gridded_9km"
ref_grid_terra <- rast(file.path(base_dir, "FirEUrisk_ref_grid.nc"))

first_year <- 2001
last_year <- 2022


all_GPP_months <- rast()
all_dates <- c()
for(this_year in first_year:last_year){
  
  for(this_month in c(paste0(0, 1:9), "10", "11", "12")) {
    
    this_file <- file.path("/data/mforrest/Productivity/GOSIF", paste0("GOSIF_GPP_", this_year, ".M", this_month, "_Mean.tif"))
    this_month_GPP <- rast(this_file) * 0.01
    this_month_GPP[this_month_GPP > 800] <- NA
    this_month_GPP_ET <- resample(this_month_GPP, ref_grid_terra, method = "average")
    add(all_GPP_months) <- this_month_GPP_ET
    all_dates <- append(all_dates, as.Date(paste(this_year, this_month, "15", sep = "-")))
  }
  
}
time(all_GPP_months) <- all_dates

writeCDF(x = all_GPP_months, filename =  file.path("/storage/shared/FirEUrisk/Gridded_9km/GPP", "GOSIF_GPP_FirEUrisk.nc"))
