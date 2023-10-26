#### HELPER FUNCTIONS

check_row_count <- function(full_dt, previous_row_count){
  
  new_row_count <- nrow(full_dt)
  
  message(paste0("Full points before = ", previous_row_count, 
                 ", rows after = ", new_row_count,
                 ", %loss = ", 100 * (previous_row_count - new_row_count)/ previous_row_count, "%"))
  
  previous_row_count <- new_row_count
  
  return(previous_row_count)
  
} 


add_long_term_means_and_deviations <- function(dt, gs_dt, agg_fun = mean){
  
  # calculate annual values (all months, JJA and growing season)
  annual_means_dt <- dt[, lapply(.SD, FUN=agg_fun, na.rm = TRUE), by = c("Lon", "Lat", "Year"), .SDcols = c("value")]
  setnames(annual_means_dt, "value", "Annual_value")
  JJA_means_dt <- dt[ Month %in% c(6,7,8) , lapply(.SD, FUN=agg_fun, na.rm = TRUE), by = c("Lon", "Lat", "Year"), .SDcols = c("value")]
  setnames(JJA_means_dt, "value", "JJA_value")
  # do growing season means
  gs_subset_dt <- merge.data.table(dt, gs_dt, by =c("Lon", "Lat", "Month"))
  gs_means_dt <- gs_subset_dt[, lapply(.SD, FUN=agg_fun, na.rm = TRUE), by = c("Lon", "Lat", "Year"), .SDcols = c("value")]
  setnames(gs_means_dt, "value", "GS_value")
  annual_values_dt <- merge.data.table(annual_means_dt, JJA_means_dt, c("Lon", "Lat", "Year"))
  annual_values_dt <- merge.data.table(annual_values_dt, gs_means_dt, c("Lon", "Lat", "Year"))
  
  

  # calculate the long term mean of the annual values
  annual_longterm_values_dt <- annual_values_dt[ , lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat")]
  annual_longterm_values_dt[ , Year := NULL]
  setnames(annual_longterm_values_dt, c("Annual_value", "JJA_value", "GS_value"), c("Mean_annual_value", "Mean_JJA_value", "Mean_GS_value"))
  
  # merge the annual values with the long term values and then calculate the deviation
  deviations_dt <- merge.data.table(annual_values_dt, annual_longterm_values_dt, by = c("Lon", "Lat"))
  deviations_dt[ , Annual_value_deviation := Annual_value - Mean_annual_value]
  deviations_dt[ , JJA_value_deviation := JJA_value - Mean_JJA_value]
  deviations_dt[ , GS_value_deviation := GS_value - Mean_GS_value]
  deviations_dt[ , Annual_value_rel_deviation := Annual_value_deviation/  Mean_annual_value]
  deviations_dt[ , JJA_value_rel_deviation := JJA_value_deviation / Mean_JJA_value]
  deviations_dt[ , GS_value_rel_deviation := GS_value_deviation / Mean_GS_value]
  
  # calculate the mean deviations of previous 1, 2 and 3 years  
  setkey(deviations_dt, "Year")
  deviations_dt[, Annual_value_deviation_1 := data.table::shift(Annual_value_deviation, n=1), by=c("Lon", "Lat")]
  deviations_dt[, Annual_value_deviation_2 := data.table::shift(data.table::frollmean(Annual_value_deviation, align = "right", n = 2)), by=c("Lon", "Lat")]
  deviations_dt[, Annual_value_deviation_3 := data.table::shift(data.table::frollmean(Annual_value_deviation, align = "right", n = 3)), by=c("Lon", "Lat")]
  deviations_dt[, JJA_value_deviation_1 := data.table::shift(JJA_value_deviation, n=1), by=c("Lon", "Lat")]
  deviations_dt[, JJA_value_deviation_2 := data.table::shift(data.table::frollmean(JJA_value_deviation, align = "right", n = 2)), by=c("Lon", "Lat")]
  deviations_dt[, JJA_value_deviation_3 := data.table::shift(data.table::frollmean(JJA_value_deviation, align = "right", n = 3)), by=c("Lon", "Lat")]
  deviations_dt[, GS_value_deviation_1 := data.table::shift(GS_value_deviation, n=1), by=c("Lon", "Lat")]
  deviations_dt[, GS_value_deviation_2 := data.table::shift(data.table::frollmean(GS_value_deviation, align = "right", n = 2)), by=c("Lon", "Lat")]
  deviations_dt[, GS_value_deviation_3 := data.table::shift(data.table::frollmean(GS_value_deviation, align = "right", n = 3)), by=c("Lon", "Lat")]
  deviations_dt[, Annual_value_rel_deviation_1 := data.table::shift(Annual_value_rel_deviation, n=1), by=c("Lon", "Lat")]
  deviations_dt[, Annual_value_rel_deviation_2 := data.table::shift(data.table::frollmean(Annual_value_rel_deviation, align = "right", n = 2)), by=c("Lon", "Lat")]
  deviations_dt[, Annual_value_rel_deviation_3 := data.table::shift(data.table::frollmean(Annual_value_rel_deviation, align = "right", n = 3)), by=c("Lon", "Lat")]
  deviations_dt[, JJA_value_rel_deviation_1 := data.table::shift(JJA_value_rel_deviation, n=1), by=c("Lon", "Lat")]
  deviations_dt[, JJA_value_rel_deviation_2 := data.table::shift(data.table::frollmean(JJA_value_rel_deviation, align = "right", n = 2)), by=c("Lon", "Lat")]
  deviations_dt[, JJA_value_rel_deviation_3 := data.table::shift(data.table::frollmean(JJA_value_rel_deviation, align = "right", n = 3)), by=c("Lon", "Lat")]
  deviations_dt[, GS_value_rel_deviation_1 := data.table::shift(GS_value_rel_deviation, n=1), by=c("Lon", "Lat")]
  deviations_dt[, GS_value_rel_deviation_2 := data.table::shift(data.table::frollmean(GS_value_rel_deviation, align = "right", n = 2)), by=c("Lon", "Lat")]
  deviations_dt[, GS_value_rel_deviation_3 := data.table::shift(data.table::frollmean(GS_value_rel_deviation, align = "right", n = 3)), by=c("Lon", "Lat")]
  
  # merge the deviations to the original data.table and return
  dt <- merge.data.table(dt, deviations_dt, by = c("Lon", "Lat", "Year"))
  return(dt)
  
}


# TODO add measure of growing season
add_antecedent_values <- function(dt, FUN = sum){
  
  # calculate 1,2,3,6 and 12 month antecedent valuesums
  dt[, value1 := data.table::shift(value, n=1), by=c("Lon", "Lat")]
  dt[, value2 := data.table::shift(data.table::frollapply(value, FUN = FUN, align = "right", n = 2)), by=c("Lon", "Lat")]
  dt[, value3 := data.table::shift(data.table::frollapply(value, FUN = FUN, align = "right", n = 3)), by=c("Lon", "Lat")]
  dt[, value6 := data.table::shift(data.table::frollapply(value, FUN = FUN, align = "right", n = 6)), by=c("Lon", "Lat")]
  dt[, value12 := data.table::shift(data.table::frollapply(value, FUN = FUN, align = "right", n = 12)), by=c("Lon", "Lat")]
  
  # also the max of the last 13 months (including this month)
  dt[, Max_value13 := data.table::frollapply(value, FUN = max, align = "right", n = 13), by=c("Lon", "Lat")]
  
  return(dt)
  
}

# remap the Lons and Lats.  Gawd I love how fast data.table is.
remapDataTableToReferenceGrid <- function(dt, ref_grid_dt, new_digits = 7) {
  
  # lons
  old_lons <- sort(unique(dt[["Lon"]]))
  new_lons <- round(sort(unique(ref_grid_dt[["Lon"]])), new_digits)
  if(length(old_lons) != length(new_lons)) stop("In remapDataTableToReferenceGrid, number of Lons don't match")
  Lon_map_dat <- data.table("Lon" = old_lons , 
                            "New_Lon" = new_lons)
  setkey(Lon_map_dat, "Lon")
  setkey(dt, "Lon")
  lons_matched <- dt[Lon_map_dat]
  
  # lats 
  old_lats <- sort(unique(dt[["Lat"]]))
  new_lats <- round(sort(unique(ref_grid_dt[["Lat"]])), new_digits)
  if(length(old_lats) != length(new_lats)) stop("In remapDataTableToReferenceGrid, number of Lats don't match")
  Lat_map_dat <- data.table("Lat" = old_lats , 
                            "New_Lat" = new_lats)
  setkey(Lat_map_dat, "Lat")
  setkey(lons_matched, "Lat")
  dt <- lons_matched[Lat_map_dat]
  
  # tidy the table and return
  dt[ , Lon := New_Lon]
  dt[ , Lat := New_Lat]
  dt[ , New_Lon := NULL]
  dt[ , New_Lat := NULL]
  
}