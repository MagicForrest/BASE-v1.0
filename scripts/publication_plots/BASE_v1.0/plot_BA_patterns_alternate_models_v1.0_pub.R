#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)

# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_BA_patterns_alternate_models_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))



# this is the fit batch and analysis verisons
fit_batch_version <- "BASE_v1.0"
analysis_version <- "BASE_v1.0"

# dataset name
obs_name <- "FireCCI51"
obs_linetype <- "solid"

cropland_col <-"orchid3"
  
ncv_col <- "springgreen4"
  


# for plotting
ba_cuts <- c(0,5,10,20,50,100,200,500,1000,2000,5000,10000)
ba_cols <- inferno(length(ba_cuts)-1)
text.multiplier <- 2.8

#  Directories for reading data and saving plots
sensitivity_dir <- here("plots", analysis_version, "sensitivity")
manuscript_dir <- here("plots", analysis_version, "manuscript")
models_dir <- here("fitted_models", analysis_version, "GLMs")


#### DEFINE BASELINES ####
baselines_list <- list(
  
  "NCV" = list(name = "NCV",
               colour = ncv_col,
               linetype = "22",
               baseline_model_id ="BASE_v1.0",
               baseline_model_name ="BASE v1.0"),
  
  "Cropland" = list(name = "PureCropland",
                    colour = cropland_col,
                    linetype = "22",
                    baseline_model_id ="BASE_v1.0",
                    baseline_model_name ="BASE v1.0")     
  
)


#### DEFINE SENSITIVITY PLOTS ####

sensitivity_list <- list(
  
  
  "Cropland_socioeconomics" = list(name = "Cropland_socioeconomics",
                                   lcc = "PureCropland",
                                   baseline_str = baselines_list[["Cropland"]]$baseline_model_id,
                                   simulations = list(list(id = "Omit_GDP",
                                                           col = "royalblue4",
                                                           linetype = "44"),
                                                      list(id = "Replace_GDP_with_HDI",
                                                           col = "sienna2",
                                                           linetype = "33"))),
  
  "MEPI_and_FWI_not_interacting" = list(name = "MEPI_and_FWI_not_interacting",
                                        lcc = "NCV",
                                        baseline_str = baselines_list[["NCV"]]$baseline_model_id,
                                        simulations = list(list(id = "MEPI_and_FWI_not_interacting",
                                                                col = "sienna2",
                                                                linetype = "33"))),
  
  
  "Omit_HDI_NCV" = list(name = "Omit_HDI_NCV",
                        lcc = "NCV",
                        baseline_str = baselines_list[["NCV"]]$baseline_model_id,
                        simulations = list(list(id = "Omit_HDI",
                                                col = "royalblue4",
                                                linetype = "44"))),
  
  
  
  "Wind_speed_quadratic" = list(name = "Wind_speed_quadratic",
                                lcc = "PureCropland",
                                baseline_str = baselines_list[["Cropland"]]$baseline_model_id,
                                simulations = list(list(id = "Wind_speed_quadratic",
                                                        col = "red4",
                                                        linetype = "44")))
  
)




#### READ AND PROCESS THE DATA FOR THE MAIN MODEL RUNS (BASELINES) ####

# baselines in a list for lookup for a string
spatial_baselines <- list()
seasonal_baselines <- list()
iav_baselines <- list()
cols_baselines <- list()
linetypes_baselines <- list()

for(this_baseline in baselines_list) {
  
  source_names <- c(obs_name, this_baseline$baseline_model_name) 
  
  # read data.tables and set names
  this_dt <- readRDS(file.path(models_dir, paste("BurntFraction", this_baseline$name, sep = "_"),  fit_batch_version, this_baseline$baseline_model_id, paste("DT", this_baseline$baseline_model_id, "rds", sep = ".")))
  setnames(this_dt, c("Observed_burnt_area", "Predicted_burnt_area_raw"), source_names)
  
  this_yearmean <- this_dt[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = source_names]
  
  this_spatial <- this_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = source_names]
  spatial_baselines[[paste(this_baseline$baseline_model_id, this_baseline$name, sep = "_")]] <- melt(this_spatial, id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area")
  
  this_seasonal <-  this_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = source_names]
  seasonal_baselines[[paste(this_baseline$baseline_model_id, this_baseline$name, sep = "_")]] <- melt(this_seasonal, id.vars = c("Month"), variable.name = "Source", value = "Burnt Area")
  
  this_iav <-   this_dt[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = source_names]
  iav_baselines[[paste(this_baseline$baseline_model_id, this_baseline$name, sep = "_")]] <- melt(this_iav, id.vars = c("Year"), variable.name = "Source", value = "Burnt Area")
  
  # assign colours, linetypes, alpha etc
  col_list <- list()
  col_list[[obs_name]] <- this_baseline$colour
  col_list[[this_baseline$baseline_model_name]] <- this_baseline$colour
  cols_baselines[[paste(this_baseline$baseline_model_id, this_baseline$name, sep = "_")]] <- col_list
  
  linetype_list <- list()
  linetype_list[[obs_name]] <- obs_linetype
  linetype_list[[this_baseline$baseline_model_name]] <- this_baseline$linetype
  linetypes_baselines[[paste(this_baseline$baseline_model_id, this_baseline$name, sep = "_")]] <-  linetype_list
  
}


#### DO THE SENSITIVITY PLOT ####
for(this_sens in sensitivity_list) {
  
  
  print(paste("** Doing sensitiviy plots: ", this_sens$name ))
  
  # get the baselines
  sens_spatial_dt <- spatial_baselines[[paste(this_sens$baseline_str, this_sens$lcc, sep = "_")]]
  sens_seasonal_dt <- seasonal_baselines[[paste(this_sens$baseline_str, this_sens$lcc, sep = "_")]]
  sens_iav_dt <- iav_baselines[[paste(this_sens$baseline_str, this_sens$lcc, sep = "_")]]
  these_cols <- cols_baselines[[paste(this_sens$baseline_str, this_sens$lcc, sep = "_")]]
  these_linetypes <- linetypes_baselines[[paste(this_sens$baseline_str, this_sens$lcc, sep = "_")]]
  
  # read each sensitivity and append it to the data.tables for plotting
  for(this_alt_list in this_sens$simulations) {
    
    this_alt <- this_alt_list$id
    print(paste(" **** Reading sensitivity simulation: ", this_alt))
    
    this_alt_pretty <- gsub(pattern = "_", replacement = " ", x = this_alt)
    
    # read and format the alt data.table
    alt_dt <- readRDS(file.path(models_dir, paste("BurntFraction", this_sens$lcc, sep = "_"), fit_batch_version, this_alt , paste("DT", this_alt, "rds", sep = ".")))
    subset_columns <- c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw")
    alt_dt[ , ..subset_columns ]
    setnames(alt_dt, "Predicted_burnt_area_raw", this_alt_pretty)
    
    alt_yearmean <- alt_dt[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = this_alt_pretty]
    
    alt_spatial <- alt_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = this_alt_pretty]
    sens_spatial_dt <- rbind(sens_spatial_dt,
                             melt(alt_spatial, id.vars = c("Lon", "Lat"), variable.name = "Source", value = "Burnt Area"))
    
    alt_seasonal <-  alt_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = this_alt_pretty]
    sens_seasonal_dt <- rbind(sens_seasonal_dt,
                              melt(alt_seasonal, id.vars = c("Month"), variable.name = "Source", value = "Burnt Area"))
    
    alt_iav <-   alt_dt[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = this_alt_pretty]
    sens_iav_dt <- rbind(sens_iav_dt,
                         melt(alt_iav, id.vars = c("Year"), variable.name = "Source", value = "Burnt Area"))
    
    these_cols[[this_alt_pretty]] <- this_alt_list$col
    these_linetypes[[this_alt_pretty]] <- this_alt_list$linetype
  }
  
  
  #### MAKE SPATIAL PLOT ####  
  sens_spatial_dt[ , value := cut(`Burnt Area`, ba_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE, dig.lab =4)]
  suppressWarnings(this_overlay <- makeOverLay(sens_spatial_dt))
  
  spatial_plot <- ggplot(sens_spatial_dt) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + scale_fill_viridis(option = "B", name = "Burnt area (ha)", discrete = TRUE)
  spatial_plot <- spatial_plot + coord_cartesian() + facet_wrap( ~ Source) + theme_bw()
  if(!is.null(this_overlay)) {
      suppressMessages(
        spatial_plot <- spatial_plot +  geom_sf(data=this_overlay, 
                                                fill = "transparent", 
                                                linewidth = 0.1,
                                                colour= "cyan")
      )
  }
  spatial_plot <- spatial_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                                       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  print(spatial_plot)
  
  magicPlot(p = spatial_plot, filename = file.path(sensitivity_dir, paste("Sensitivity_Spatial", this_sens$name, sep = "_")), width = 1800, height = 1200)
  
  
  # paper plots
  
  # Fig F02
  if(this_sens$name == "Cropland_socioeconomics"){
    
    magicPlot(p = spatial_plot, filename = file.path(manuscript_dir, paste0("Figure_F02_", this_sens$name)), width = 1500, height = 1800)
    
    pdf(file = file.path(manuscript_dir, paste0("Figure_F02_", this_sens$name, ".pdf")), width = 15, height = 18)
    print(spatial_plot)
    dev.off()
    pdf(file = file.path(manuscript_dir, paste0("fig_F02.pdf")), width = 15, height = 18)
    print(spatial_plot)
    dev.off()
    
  }
  
  
  
  #### SEASONAL PLOT ####
  
  sens_seasonal_dt[ , `Burnt Area`:= `Burnt Area`/1000000]
  seasonal_plot <- ggplot(data = sens_seasonal_dt, aes(x = Month, y = `Burnt Area`, col = Source,  linetype = Source)) 
  seasonal_plot <- seasonal_plot + geom_line(linewidth = 2)
  seasonal_plot <- seasonal_plot + scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  seasonal_plot <- seasonal_plot + theme_bw() 
  seasonal_plot <- seasonal_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  seasonal_plot <- seasonal_plot + scale_colour_manual(values = unlist(these_cols))
  seasonal_plot <- seasonal_plot + scale_linetype_manual(values = unlist(these_linetypes))
  seasonal_plot <- seasonal_plot + labs(y = "Burnt area (Mha)")
  
  print(seasonal_plot)
  magicPlot(p = seasonal_plot, filename = file.path(sensitivity_dir, paste("Sensitivity_Seasonal", this_sens$name, sep = "_")), width = 1600, height = 900)
  
  # Fig D4
  if(this_sens$name == "MEPI_and_FWI_not_interacting"){
    
    magicPlot(p = seasonal_plot, filename = file.path(manuscript_dir, paste("Figure_E01", "Seasonal", this_sens$name, sep = "_")), width = 1600, height = 900)
    
    pdf(file = file.path(manuscript_dir, paste0("Figure_E01_Seasonal_", this_sens$name, ".pdf")), width = 16, height = 9)
    print(seasonal_plot)
    dev.off()
    pdf(file = file.path(manuscript_dir, paste0("fig_E01.pdf")), width = 16, height = 9)
    print(seasonal_plot)
    dev.off()
    
  }
  
  
  #### IAV PLOT ####
  sens_iav_dt[ , `Burnt Area`:= `Burnt Area`/1000000]
  iav_plot <- ggplot(data = sens_iav_dt, aes(x = Year, y = `Burnt Area`, col = Source,  linetype = Source)) 
  iav_plot <- iav_plot + geom_smooth(aes( col = Source,  linetype = Source, fill = Source), method=lm, formula = y ~ x, linewidth = 2, alpha = 0.3) 
  iav_plot <- iav_plot + geom_line(linewidth = 2)
  iav_plot <- iav_plot + theme_bw() 
  iav_plot <- iav_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  iav_plot <- iav_plot + scale_colour_manual(values = unlist(these_cols))
  iav_plot <- iav_plot + scale_fill_manual(values = unlist(these_cols))
  iav_plot <- iav_plot + scale_linetype_manual(values = unlist(these_linetypes))
  iav_plot <- iav_plot + labs(y = "Burnt area (Mha)")
  
  print(iav_plot)
  magicPlot(p = iav_plot, filename = file.path(sensitivity_dir, paste("Sensitivity_IAV", this_sens$name, sep = "_")), width = 1200, height = 1000)
  
  
  # paper plots
  
  # Fig D1
  if(this_sens$name == "Cropland_socioeconomics"){
    
    magicPlot(p = iav_plot, filename = file.path(manuscript_dir, paste("Figure_F01", "IAV", this_sens$name, sep = "_")), width = 1600, height = 900)
    
    pdf(file = file.path(manuscript_dir, paste0("Figure_F01_IAV_", this_sens$name, ".pdf")), width = 16, height = 9)
    print(iav_plot)
    dev.off()
    pdf(file = file.path(manuscript_dir, paste0("fig_F01.pdf")), width = 16, height = 9)
    print(iav_plot)
    dev.off()
    
  }
  
  # Fig D3
  if(this_sens$name == "Omit_HDI_NCV"){
    
    magicPlot(p = iav_plot, filename = file.path(manuscript_dir, paste("Figure_F03", "IAV", this_sens$name, sep = "_")), width = 1600, height = 900)
    
    pdf(file = file.path(manuscript_dir, paste0("Figure_F03_IAV_", this_sens$name, ".pdf")), width = 16, height = 9)
    print(iav_plot)
    dev.off()
    pdf(file = file.path(manuscript_dir, paste0("fig_F03.pdf")), width = 16, height = 9)
    print(iav_plot)
    dev.off()
    
  }
  
}
