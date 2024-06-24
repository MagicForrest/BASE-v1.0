#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)

# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_BA_patterns_main_models_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))


# define the models to plot
ncv_model <- "BASE_v1.0"
cropland_model <- "BASE_v1.0"
analysis_version <- "BASE_v1.0"

# dataset names
obs_name <- "FireCCI51"
sim_name <- "BASE"
lcc_names <- c(obs_name, sim_name)


ba_cuts <- c(0,5,10,20,50,100,200,500,1000,2000,5000,10000)
ba_cols <- turbo(length(ba_cuts)-1)
text.multiplier <- 2.8



#  Directories for reading data and saving plots
manuscript_fig_dir <- here("plots", analysis_version, "manuscript")
models_dir <- here("fitted_models", analysis_version, "GLMs")


#### READ AND PROCESS THE DATA ####

# read data.tables and set names
ncv_dt <- readRDS(file.path(models_dir, "BurntFraction_NCV",  ncv_model, paste("DT", ncv_model, "rds", sep = ".")))
cropland_dt <- readRDS(file.path(models_dir, "BurntFraction_PureCropland",  cropland_model, paste("DT", cropland_model, "rds", sep = ".")))
setnames(ncv_dt, c("Observed_burnt_area", "Predicted_burnt_area_raw"), lcc_names) 
setnames(cropland_dt, c("Observed_burnt_area", "Predicted_burnt_area_raw"), lcc_names) 

# NCV
ncv_yearmean <- ncv_dt[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = lcc_names]
ncv_spatial <- ncv_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = lcc_names]
ncv_seasonal <-  ncv_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = lcc_names]
ncv_iav <-   ncv_dt[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = lcc_names]

# Cropland
cropland_yearmean <- cropland_dt[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = lcc_names]
cropland_spatial <- cropland_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = lcc_names]
cropland_seasonal <-  cropland_yearmean[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = lcc_names]
cropland_iav <-   cropland_dt[, lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = lcc_names]



#### MAKE SPATIAL FIGURE ####

this_overlay <- makeOverLay(ncv_spatial)

# combine into single data.frame for plotting
ncv_spatial[ , LCC := "NCV"]
cropland_spatial[ , LCC := "Cropland"]
spatial_dt <- rbind(ncv_spatial, cropland_spatial)
spatial_dt_for_plotting <- melt(spatial_dt, id.vars = c("Lon", "Lat", "LCC"), variable.name = "Source", value = "Burnt Area")
spatial_dt_for_plotting[ , value := cut(`Burnt Area`, ba_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE, dig.lab =4)]

spatial_plot <- ggplot(spatial_dt_for_plotting) + geom_tile(aes(x = Lon, y = Lat, fill = value)) + scale_fill_viridis(option = "H", name = "Burnt area (ha)", discrete = TRUE) 
spatial_plot <- spatial_plot + coord_cartesian(expand = FALSE) + facet_grid(LCC ~ Source, switch = "y" ) + theme_bw()
spatial_plot <- spatial_plot + scale_x_continuous(expand = c(0, 0))
spatial_plot <- spatial_plot +scale_y_continuous(expand = c(0, 0))
if(!is.null(this_overlay)) {
  spatial_plot <- spatial_plot +  geom_sf(data=this_overlay, 
                                          fill = "transparent", 
                                          linewidth = 0.2,
                                          colour= "magenta")
}
spatial_plot <- spatial_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                                     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
print(spatial_plot)

magicPlot(p = spatial_plot, filename = file.path(manuscript_fig_dir, "Figure_04_Spatial_BA"), width = 1100, height = 1200, type = "png")

pdf(file = file.path(manuscript_fig_dir, paste0("Figure_04_Spatial_BA.pdf")), width = 11, height = 12)
print(spatial_plot)
dev.off()

pdf(file = file.path(manuscript_fig_dir, paste0("fig_04.pdf")), width = 11, height = 12)
print(spatial_plot)
dev.off()



#### MAKE IAV FIGURE ####

ncv_iav[ , LCC := "NCV"]
cropland_iav[ , LCC := "Cropland"]
iav_dt <- rbind(ncv_iav, cropland_iav)

this_iav_for_plotting <- melt(iav_dt, id.vars = c("Year", "LCC"), variable.name = "Source", value = "Burnt Area")
this_iav_for_plotting[ , `Burnt Area`:= `Burnt Area`/1000000]
iav_plot <- ggplot(data = this_iav_for_plotting, aes(x = Year, y = `Burnt Area`, col = LCC,  linetype = Source)) 
iav_plot <- iav_plot + geom_smooth(aes( col = LCC,  linetype = Source), method=lm, linewidth = 2, alpha = 0.6) 
iav_plot <- iav_plot + geom_line(linewidth = 2)
iav_plot <- iav_plot + theme_bw() 
iav_plot <- iav_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))

iav_plot <- iav_plot + facet_wrap( ~LCC, ncol = 1, scales = "free")
iav_plot <- iav_plot + scale_colour_manual(values = c("Cropland" = "orchid4", 
                                                            "NCV" ="springgreen4"), guide = "none")
iav_plot <- iav_plot + labs(y = "Burnt area (Mha)")

print(iav_plot)
magicPlot(p = iav_plot, filename = file.path(manuscript_fig_dir, "Figure_05_IAV_BA"), width = 1200, height = 1000)

plot_dim_units = "px"
pdf(file = file.path(manuscript_fig_dir, paste0("Figure_05_IAV_BA.pdf")), width = 12, height = 10)
print(iav_plot)
dev.off()

pdf(file = file.path(manuscript_fig_dir, paste0("fig_05.pdf")), width = 12, height = 10)
print(iav_plot)
dev.off()



#### MAKE SEASONAL FIGURE ####

ncv_seasonal[ , LCC := "NCV"]
cropland_seasonal[ , LCC := "Cropland"]
seasonal_dt <- rbind(ncv_seasonal, cropland_seasonal)

this_seasonal_for_plotting <- melt(seasonal_dt, id.vars = c("Month", "LCC"), variable.name = "Source", value = "Burnt Area")
this_seasonal_for_plotting[ , `Burnt Area`:= `Burnt Area`/1000000]
seasonal_plot <- ggplot(data = this_seasonal_for_plotting, aes(x = Month, y = `Burnt Area`, col = LCC,  linetype = Source)) 
seasonal_plot <- seasonal_plot + geom_line(linewidth = 2)
seasonal_plot <- seasonal_plot + scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
seasonal_plot <- seasonal_plot + theme_bw() 
seasonal_plot <- seasonal_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
seasonal_plot <- seasonal_plot + facet_wrap( ~LCC, ncol = 1, scales = "free")
seasonal_plot <- seasonal_plot + scale_colour_manual(values = c("Cropland" = "orchid4", 
                                                      "NCV" ="springgreen4"), guide = "none")
seasonal_plot <- seasonal_plot + labs(y = "Burnt area (Mha)")

print(seasonal_plot)
magicPlot(p = seasonal_plot, filename = file.path(manuscript_fig_dir, "Figure_06_Seasonal_BA"), width = 1200, height = 1000)

pdf(file = file.path(manuscript_fig_dir, paste0("Figure_06_Seasonal_BA.pdf")), width = 12, height = 10)
print(seasonal_plot)
dev.off()

pdf(file = file.path(manuscript_fig_dir, paste0("fig_06.pdf")), width = 12, height = 10)
print(seasonal_plot)
dev.off()





#### CONSIDER COMBINING PLOTS??? ####