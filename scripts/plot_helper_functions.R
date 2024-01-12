

makeOverLay <- function(spatial_dt) {
  
  overlay <- rnaturalearth::ne_countries(returnclass = "sf", scale = 50)
  sf::sf_use_s2(FALSE)
  
  # make an appropriate overlay
  all_lons <- sort(unique(spatial_dt[["Lon"]]))
  all_lats <- sort(unique(spatial_dt[["Lat"]]))
  plot_region <- c(xmin = min(all_lons), xmax = max(all_lons), ymin = min(all_lats), ymax = max(all_lats))
  this_overlay <- st_crop(overlay, plot_region)
  
  # drop countries not in the FirEUrisk targer area
  this_overlay <- this_overlay[-which(this_overlay$admin %in%  c("Russia", "Ukraine", "Tunisia", "Algeria", "Belarus",  "Turkey", "Morocco", "Moldova")),,drop =FALSE]
  
  return(this_overlay)
}


spatialPlot <- function(spatial_dt, overlay = NULL, ...){
  
  ba_cuts <- c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000,10000)
  ba_cols <- turbo(length(ba_cuts)-1)
  
  spatial_dt_for_plotting <- melt(spatial_dt, id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area")
  spatial_dt_for_plotting[ , value := cut(`Burnt Area`, ba_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
  spatial_plot <- ggplot(spatial_dt_for_plotting) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + scale_fill_viridis(option = "H", name = "Burnt area (ha)", discrete = TRUE) + facet_wrap(~Source) 
  spatial_plot <- spatial_plot + coord_cartesian() 
  if(!is.null(overlay)) {
    spatial_plot <- spatial_plot +  geom_sf(data=overlay, 
                                            fill = "transparent", 
                                            linewidth = 0.1,
                                            colour= "black")
  }
  spatial_plot <- spatial_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  magicPlot(p = spatial_plot, ...)
  
}

spatialPlotDifferenceFromBaseline <- function(spatial_dt, baseline, obs = "FireCCI51", overlay = NULL, ...){
  
  ba_diff_cuts <- c(-100, -50, -20, -10, -5, -2, -1, 1, 2, 5, 10, 20, 50, 100)
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(length(ba_diff_cuts))
  
  # prepare data
  columns_to_diff <- names(spatial_dt)[-which(names(spatial_dt) %in% c("Lon", "Lat", baseline, obs))]
  new_names <- paste0(columns_to_diff,"-", "Baseline")
  new_dt <- copy(spatial_dt)[ , (new_names) := pmin((.SD - get(baseline)) / get(baseline) * 100, 100) , .SDcols = columns_to_diff]
  new_names <- c("Lon", "Lat", new_names)
  spatial_dt_for_plotting <- melt(new_dt[ , ..new_names], id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area")
  spatial_dt_for_plotting[ , value := cut(`Burnt Area`, ba_diff_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
  spatial_plot <- ggplot(spatial_dt_for_plotting) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + scale_fill_manual(values = cols, name = "delta Burnt area (%)") + facet_wrap(~Source) 
  spatial_plot <- spatial_plot + coord_cartesian() 
  if(!is.null(overlay)) {
    spatial_plot <- spatial_plot +  geom_sf(data=overlay, 
                                            fill = "transparent", 
                                            linewidth = 0.1,
                                            colour= "black")
  }
  spatial_plot <- spatial_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  magicPlot(p = spatial_plot, ...)
  
}


spatialDeltaError <- function(spatial_dt, baseline, obs = "FireCCI51",  overlay = NULL, ...){
  
  ba_diff_cuts <- c(-200, -100, -50, -20, -10, -5, -2, 2, 5, 10, 20, 50, 100, 200)
  cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdBu"))(length(ba_diff_cuts))
  
  
  #### CHANGES IN PERFORMANCES RELATIVE TO DATA ####
  delta_error_cols <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "BrBG"))(length(ba_diff_cuts)))
  
  improvement_dt <- copy(spatial_dt)[ , Baseline_Error := get(baseline) - get(obs)]
  columns_to_diff <- names(spatial_dt)[-which(names(spatial_dt) %in% c("Lon", "Lat", baseline, obs))]
  error_names <- paste0(columns_to_diff, "_Error")
  improvement_dt[ , (error_names) := .SD - get(obs) , .SDcols = columns_to_diff]
  error_improvement_names <- paste0(columns_to_diff, "_Improvement")
  improvement_dt[ , (error_improvement_names) := pmin( (abs(.SD) - abs(Baseline_Error))/Baseline_Error * 100, 200) , .SDcols = error_names]
  
  new_names <- c("Lon", "Lat", error_improvement_names)
  spatial_dt_for_plotting <- improvement_dt[ , ..new_names]
  setnames(spatial_dt_for_plotting, gsub(pattern = "_Improvement", "", names(spatial_dt_for_plotting)))
  spatial_dt_for_plotting <- melt(spatial_dt_for_plotting, id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area")
  spatial_dt_for_plotting[ , value := cut(`Burnt Area`, ba_diff_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
  spatial_plot <- ggplot(spatial_dt_for_plotting) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + scale_fill_manual(values = delta_error_cols, name = "delta Error (%)") + facet_wrap(~Source) 
  spatial_plot <- spatial_plot + coord_cartesian() 
  spatial_plot <- spatial_plot + labs(caption = "Plot shows the percentage change in error (normalised to baseline error).")
  
  if(!is.null(overlay)) {
    spatial_plot <- spatial_plot +  geom_sf(data=overlay, 
                                            fill = "transparent", 
                                            linewidth = 0.1,
                                            colour= "black")
  }
  spatial_plot <- spatial_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  magicPlot(p = spatial_plot, ...)
  
  
}




IAVPlot <- function(iav_dt, cols = NULL, linewidths = NULL, linetypes = NULL, ...) {
  
  this_iav_for_plotting <- melt(iav_dt, id.vars = c("Year"), variable.name = "Source", value = "Burnt Area")
  iav_plot <- ggplot(data = this_iav_for_plotting, aes(x = Year, y = `Burnt Area`, col = Source, linewidth = Source, linetype = Source)) + geom_line()
  iav_plot <- iav_plot + theme_bw() 
  iav_plot <- iav_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  if(!is.null(cols))  iav_plot <- iav_plot + scale_color_manual(values = cols)
  if(!is.null(linewidths)) iav_plot <- iav_plot + scale_linewidth_manual(values = linewidths)
  if(!is.null(linetypes))  iav_plot <- iav_plot + scale_linetype_manual(values = linetypes)
  magicPlot(p = iav_plot, ...)
  return(iav_plot)
}



seasonalPlot <- function(seasonal_dt, cols = NULL, linewidths = NULL, linetypes = NULL, ...) {
  
  seasonal_dt_for_plotting <- melt(seasonal_dt, id.vars = c("Month"), variable.name = "Source", value = "Burnt Area")
  seasonal_plot <- ggplot(seasonal_dt_for_plotting) + geom_line(aes(x = Month, y = `Burnt Area`, col = Source, linewidth = Source, linetype = Source))
  seasonal_plot <- seasonal_plot + theme_bw() 
  seasonal_plot <- seasonal_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  if(!is.null(cols))  seasonal_plot <- seasonal_plot + scale_color_manual(values = cols)
  if(!is.null(linewidths)) seasonal_plot <- seasonal_plot + scale_linewidth_manual(values = linewidths)
  if(!is.null(linetypes))  seasonal_plot <- seasonal_plot + scale_linetype_manual(values = linetypes)
  magicPlot(p = seasonal_plot, ...)
  return(seasonalPlot)
}



