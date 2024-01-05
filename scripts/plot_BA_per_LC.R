library(terra)
library(data.table)
library(tictoc)
library(ggplot2)
library(viridis)
library(sf)
library(ggpubr)

# define root path with here package and 
here::i_am("scripts/plot_BA_per_LC.R")
library(here)

source(here("scripts", "plot_helper_functions.R"))
source(here("scripts", "plot_utils.R"))


# to make the plots labels nices
lcc_lookup <- list(
  
  # all    
  "all" = list(name = "Total", level = "Total", type = "Total"),
  
  # main categories
  "cropland" = list( name = "All croplands", level = "Category", type = "Cropland"),
  "ncv" = list( name = "NCV", level = "Category", type = "NCV"),
  "nonflammable" = list( name = "Non-flammable", level = "Category", type = "Non-flammable"),
  "urban" = list( name = "Urban", level = "Category", type = "Non-flammable"),
  "allgrassland" = list( name = "All grasslands",level = "Category", type = "Grassland"),
  "was" = list( name = "Woodlands and shrublands", level = "Category", type = "NCV"),
  
  # cropland sub-types
  "purecropland" = list( name = "Herb. croplands", level = "Subcategory", type = "Cropland"),
  "woodycropland" = list(name = "Woody croplands", level = "Subcategory", type = "Cropland"),
  "mosaiccropland" = list( name = "Mosaic croplands", level = "Subcategory", type = "Cropland"),
  
  # grassland sub-types
  "mosaicgrasslanddominated" = list( name = "Mosaic grasslands (grass dom.)", level = "Subcategory", type = "Grassland"),
  "mosaicwoodydominated" = list( name = "Mosaic grasslands (woody dom.)", level = "Subcategory", type = "Grassland"),
  
  
  # ncv subtypes
  "sparse" = list( name = "Sparse vegetation", level = "Subcategory", type = "NCV"),
  "shrublands" = list( name = "Shrublands", level = "Subcategory", type = "NCV"),
  "mosaicgrassland" = list( name = "Natural mosaics", level = "Subcategory", type = "NCV"),
  "puregrassland" = list( name = "Grasslands", level = "Subcategory", type = "NCV")
)



version_label <- "v2.0"
base_dir <- here("external_files", "gridded_9km")
input_dir <- file.path(base_dir, "FireCCI51")
results_dir <- here("results/Burnt_Fraction_per_LCC")
plot_dir <- here("plots/manuscript_BASE_v1.0")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)


# read file for FireEUrisk countries only
refgrid_rast <- rast("/storage/shared/FirEUrisk/Gridded_9km/LandUse/Lu_6bands_Refgrid.nc")


text.multiplier <- 2.8

# first get a list of all the gridcell
refgrid_dt <- as.data.table(refgrid_rast[[1]], xy = TRUE)
refgrid_dt[ , Lu_6bands_Refgrid_1 := NULL]
setnames(refgrid_dt, c("Lon", "Lat"))
setkey(refgrid_dt, "Lon", "Lat")

# for comparing
all_maps_dt <- data.table()
all_seasonal_dt <- data.table()
all_yearly_dt <- data.table()
all_yearly_normalised_dt <- data.table()
all_total_dt <- data.table()
all_lc_frac_maps_dt <- data.table()

# loop over LC types
for(this_bf_file in list.files(path = input_dir, pattern = "^ESA_CCI_51.*tif$")){
  
  
  # read data (both burnt fraction and land cover) and extract the name of the land cover type
  this_bf_rast <- rast(file.path(input_dir, this_bf_file))
  this_bf_name <- strsplit(this_bf_file, "_")[[1]][[5]]
  this_lc_rast <- rast(file.path(input_dir, gsub(pattern = "51", replacement = "LC", this_bf_file)))
  print(this_bf_name)
  this_bf_pretty_name <- lcc_lookup[[this_bf_name]]$name
  print(this_bf_pretty_name)
  
  # get gridcell size for weighting is not already done
  if(!exists("gridcell_areas_dt")){
    gridcell_sizes_rast <- cellSize(this_bf_rast[[1]], unit="ha")
    gridcell_sizes_dt <- as.data.table(gridcell_sizes_rast, xy = TRUE)
    setnames(gridcell_sizes_dt, c("Lon", "Lat", "GridcellArea"))
  }
  
  # convert burnt fraction to a data.table
  this_bf_dt <- as.data.table(this_bf_rast, xy = TRUE)
  setnames(this_bf_dt, c("Lon", "Lat", format(time(this_bf_rast), "%Y-%m")))
  setkey(this_bf_dt, "Lon", "Lat")
  this_bf_dt <- this_bf_dt[refgrid_dt]
  this_bf_dt_melted <- melt.data.table(this_bf_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE)
  setnames(this_bf_dt_melted, c("Lon", "Lat", "Date", "BurntFraction"))
  this_bf_dt_melted[ , c("Year", "Month") := tstrsplit(Date, "-")]
  this_bf_dt_melted[ , Month := as.integer(Month)]
  this_bf_dt_melted[ , Year := as.integer(Year)]
  this_bf_dt_melted[ , Date := NULL]
  
  # convert land cover class fraction to a data.table for weighting
  this_lc_dt <- as.data.table(this_lc_rast, xy = TRUE)
  setnames(this_lc_dt, c("Lon", "Lat", format(time(this_lc_rast), "%Y")))
  setkey(this_lc_dt, "Lon", "Lat")
  this_lc_dt <- this_lc_dt[refgrid_dt]
  this_lc_dt_melted <- melt.data.table(this_lc_dt, id.vars = c("Lon", "Lat"), variable.factor = FALSE, value.name = "LandCoverFraction", variable.name = "Year")
  this_lc_dt_melted[ , Year := as.integer(Year)]
  
  # calculate the actual area covered by each land cover class in the gridcell
  this_lc_area_dt <- merge.data.table(x = this_lc_dt_melted,  gridcell_sizes_dt, by = c("Lon", "Lat"))
  this_lc_area_dt[ , LandCoverArea := LandCoverFraction * GridcellArea]
  
  
  # merge the actual area of the LCC to the BF table
  this_bf_dt_melted <- merge.data.table(this_lc_area_dt, this_bf_dt_melted, by = c("Lon", "Lat", "Year"))
  
  # aggregate on level of burnt fraction
  this_bf_dt_meanyear <- this_bf_dt_melted[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Month"), .SDcols = c("BurntFraction", "LandCoverArea")]
  this_bf_dt_map <- this_bf_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = c("BurntFraction")] 
  this_bf_dt_seasonal <- this_bf_dt_meanyear[ , lapply(.SD, FUN=stats::weighted.mean, w = LandCoverArea, na.rm = TRUE), by = c("Month"), .SDcols = c("BurntFraction")] 
  this_bf_dt_yearsum <- this_bf_dt_melted[ , lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat", "Year"), .SDcols = c("BurntFraction", "LandCoverArea")] 
  this_bf_dt_yearly <- this_bf_dt_yearsum[ , lapply(.SD, FUN=stats::weighted.mean, w = LandCoverArea, na.rm = TRUE), by = c("Year"), .SDcols = c("BurntFraction")] 
  this_bf_dt_yearly_normalised <- copy(this_bf_dt_yearly)[ , BurntFraction := BurntFraction/sum(BurntFraction)]
  
  
  
  # also the landcover fraction
  this_lc_dt_meanyear <- this_lc_dt_melted[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon", "Lat"), .SDcols = c("LandCoverFraction")]
  
  # calculate absolute burnt area
  this_bf_dt_melted[ , BurntArea := LandCoverArea * BurntFraction]
  this_ba_total <- sum(this_bf_dt_melted[["BurntArea"]], na.rm =TRUE)/length(unique((this_bf_dt_melted[["Year"]])))/1000000
  print(this_ba_total)
  
  # add to big plotting data.tables
  all_maps_dt <- rbind(all_maps_dt, this_bf_dt_map[ , `Land cover class` := this_bf_pretty_name])
  all_seasonal_dt <- rbind(all_seasonal_dt, this_bf_dt_seasonal[ , `Land cover class` := this_bf_pretty_name])
  all_yearly_dt <- rbind(all_yearly_dt, this_bf_dt_yearly[ , `Land cover class` := this_bf_pretty_name])
  all_yearly_normalised_dt <- rbind(all_yearly_normalised_dt, this_bf_dt_yearly_normalised[ , `Land cover class` := this_bf_pretty_name])
  all_total_dt <- rbind(all_total_dt, data.table("Land cover class" = this_bf_pretty_name, 
                                                 "Level" = lcc_lookup[[this_bf_name]]$level, 
                                                 "Type" = lcc_lookup[[this_bf_name]]$type, 
                                                 "Total BA (Mha)" = this_ba_total))
  all_lc_frac_maps_dt <- rbind(all_lc_frac_maps_dt, this_lc_dt_meanyear[ , `Land cover class` := this_bf_pretty_name])
  print(all_lc_frac_maps_dt)
  
}

# convert spatial fraction to percentage for more concise plotting
all_maps_dt[ , BurntFraction := BurntFraction * 100]


#### PLOTS ####




# plot total burnt area
ba_histo <- ggplot(data = all_total_dt) + geom_col(aes(x = `Land cover class`, y = `Total BA (Mha)`))
ba_histo <- ba_histo + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                             axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
magicPlot(p = ba_histo, filename = file.path(results_dir, "All_BurntAreas_histo"), width = 1400, height = 900)
print(ba_histo)



### Paper plot
ba_histo <- ggplot(data = all_total_dt) + geom_col(aes(x = `Land cover class`, y = `Total BA (Mha)`, fill = Type,  alpha = Level))
ba_histo <- ba_histo + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                             axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
magicPlot(p = ba_histo, filename = file.path(results_dir, "All_BurntAreas_histo"), width = 1400, height = 900)
print(ba_histo)

positions <- c("Total", "All croplands", "Herb. croplands", "Mosaic croplands", "Woody croplands", "NCV", "Woodlands and shrublands", "Natural mosaics", "Grasslands", "Shrublands", "Sparse vegetation")
ba_histo <- ba_histo + scale_x_discrete(limits = positions)
ba_histo <- ba_histo + scale_alpha_manual(values = c("Total" = 1, "Category" = 1, "Subcategory" = 0.5),
                                          guide="none")
ba_histo <- ba_histo + scale_fill_manual(values = c("Total" = "midnightblue", 
                                                    "Cropland" = "orchid4", 
                                                    "NCV" ="springgreen4"), 
                                         guide="none")


fig1_histo <- ba_histo

print(ba_histo)



# plot different comparisons
all_groups <- list(
  
  
  # list(name = "Cropland types", subclasses = c("All croplands", 
  #                                              "Herb. croplands", 
  #                                              "Mosaic croplands", 
  #                                              "Woody croplands")),
  # list(name = "Grassland types", subclasses = c("All grasslands", 
  #                                               "Grasslands",
  #                                               "Natural mosaics", 
  #                                               "Mosaic grasslands (grass dom.)", 
  #                                               "Mosaic grasslands (woody dom.)",
  #                                               "Woodlands and shrublands")),
  # list(name = "Grassland-woody gradient", subclasses = c("Grasslands",
  #                                                        "Natural mosaics",
  #                                                        "Woodlands and shrublands")),
  # list(name = "Non-vegetation types", subclasses = c("Urban", 
  #                                                    "Non-flammable")),
  # list(name = "Non-cropland vegetation types", subclasses = c("Total",
  #                                                             "NCV", 
  #                                                             "Woodlands and shrublands",
  #                                                             "Sparse vegetation")),
  # list(name = "Candidate categories", subclasses = c("NCV", 
  #                                                    "Woodlands and shrublands",
  #                                                    "Herb. croplands"))
  
  list(name = "Main Figures", subclasses = c("Herb. croplands",
                                             "NCV")),
  
  list(name = "Supp Figures NCV", subclasses = c("NCV",
                                                 "Woodlands and shrublands",
                                                 "Natural mosaics", 
                                                 "Grasslands", 
                                                 "Shrublands", 
                                                 "Sparse vegetation"))
  
  
  
)

for(this_group in all_groups) {
  
  # subset hist
  ba_histo <- ggplot(data = all_total_dt[ `Land cover class` %in% this_group$subclasses, ]) + geom_col(aes(x = `Land cover class`, y = `Total BA (Mha)`))
  ba_histo <- ba_histo + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                               axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  magicPlot(p = ba_histo, filename = file.path(results_dir, paste0(gsub(" ", "_", this_group$name), "_", "Histo")), width = 1400, height = 900)
  print(ba_histo)
  
  
  # plot seasonal cycle
  seasonal_plot <- ggplot(all_seasonal_dt[ `Land cover class` %in% this_group$subclasses, ]) + geom_line(aes(x = Month, y = BurntFraction, col = `Land cover class` , linetype = `Land cover class` ), linewidth = 2)
  if(this_group$name == "Main Figures") {
    seasonal_plot <- ggplot(all_seasonal_dt[ `Land cover class` %in% this_group$subclasses, ]) + geom_line(aes(x = Month, y = BurntFraction, col = `Land cover class`), linewidth = 2)
    seasonal_plot <- seasonal_plot + scale_color_manual(values = c("Herb. croplands" = "orchid4", 
                                                                   "NCV" ="springgreen4"))
    fig1_seasonal <- seasonal_plot
  }
  seasonal_plot <- seasonal_plot + scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  seasonal_plot <- seasonal_plot + labs(y = "Mean burnt fraction")
  seasonal_plot <- seasonal_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  magicPlot(p = seasonal_plot, filename = file.path(results_dir, paste0(gsub(" ", "_", this_group$name), "_", "Seasonal")), width = 1400, height = 900)
  if(this_group$name == "Main Figures") {
    fig1_seasonal <- seasonal_plot
  }
  else if(this_group$name == "Supp Figures NCV") {
    magicPlot(p = seasonal_plot, filename = file.path(plot_dir, paste0("FigureS5_NCV_Seasonal")),  width = 1800, height = 1500)
  }
  
  # plot annual TS
  annual_plot <- ggplot(all_yearly_dt[  `Land cover class`  %in% this_group$subclasses, ]) + geom_line(aes(x = Year, y = BurntFraction, col = `Land cover class` , linetype = `Land cover class`), linewidth = 2)
  if(this_group$name == "Main Figures") {
    annual_plot <- ggplot(all_yearly_dt[  `Land cover class`  %in% this_group$subclasses, ]) + geom_line(aes(x = Year, y = BurntFraction, col = `Land cover class`), linewidth = 2)
    annual_plot <- annual_plot + scale_colour_manual(values = c("Herb. croplands" = "orchid4", 
                                                                "NCV" ="springgreen4"))
  }
  annual_plot <- annual_plot + labs(y = "Mean burnt fraction")
  annual_plot <- annual_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  magicPlot(p = annual_plot, filename = file.path(results_dir, paste0(gsub(" ", "_", this_group$name), "_", "Yearly")), width = 1400, height = 900)
  
  
  # plot normalised annual TS
  annual_plot_norm <- ggplot(all_yearly_normalised_dt[  `Land cover class`  %in% this_group$subclasses, ]) + geom_line(aes(x = Year, y = BurntFraction, col = `Land cover class` , linetype = `Land cover class`), linewidth = 2)
  if(this_group$name == "Main Figures") {
    annual_plot_norm <- ggplot(all_yearly_normalised_dt[  `Land cover class`  %in% this_group$subclasses, ]) + geom_line(aes(x = Year, y = BurntFraction, col = `Land cover class`), linewidth = 2)
    annual_plot_norm <- annual_plot_norm + scale_colour_manual(values = c("Herb. croplands" = "orchid4", 
                                                                          "NCV" ="springgreen4"))
  }
  annual_plot_norm <- annual_plot_norm + labs(y = "Mean burnt fraction (normalised)")
  annual_plot_norm <- annual_plot_norm + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  magicPlot(p = annual_plot_norm, filename = file.path(results_dir, paste0(gsub(" ", "_", this_group$name), "_", "NormalisedYearly")), width = 1400, height = 900)
  if(this_group$name == "Main Figures") {
    annual_plot_norm <- annual_plot_norm + scale_colour_manual(values = c("Herb. croplands" = "orchid4", 
                                                                          "NCV" ="springgreen4"), 
                                                               guide="none")
    fig1_annual <- annual_plot_norm
  }
  else if(this_group$name == "Supp Figures NCV") {
    magicPlot(p = annual_plot_norm, filename = file.path(plot_dir, paste0("FigureS4_NCV_Annual")),  width = 1800, height = 1500)
  }
  
  # plot maps
  overlay <- makeOverLay(all_maps_dt[  `Land cover class`  %in% this_group$subclasses, ])
  
  bf_cuts <- c(0.00, 0.001, 0.002, 0.005, 0.01,0.02,0.05,0.10,0.20,0.50,1)
  bf_cuts <- bf_cuts * 100
  bf_cols <- turbo(length(bf_cuts)-1)
  all_maps_dt[ , BurntFraction_cut := cut(BurntFraction, bf_cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
  spatial_plot <- ggplot(all_maps_dt[  `Land cover class`  %in% this_group$subclasses, ]) + geom_tile(aes(x = Lon, y = Lat, fill = BurntFraction_cut)) + scale_fill_viridis(option = "H", name = "%", discrete = TRUE) + facet_wrap(~`Land cover class` ) 
  spatial_plot <- spatial_plot + coord_cartesian() 
  #spatial_plot <- spatial_plot + labs(title = this_group$name)
  spatial_plot <- spatial_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                                       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  spatial_plot <- spatial_plot +  geom_sf(data=overlay, 
                                          fill = "transparent", 
                                          linewidth = 0.2,
                                          colour= "magenta")
  magicPlot(p = spatial_plot, filename = file.path(results_dir, paste0(gsub(" ", "_", this_group$name), "_", "Maps")),  width = 1400, height = 1200)
  if(this_group$name == "Main Figures") {
    fig1_spatial <- spatial_plot
  }
  else if(this_group$name == "Supp Figures NCV") {
    magicPlot(p = spatial_plot, filename = file.path(plot_dir, paste0("FigureS3_NCV_BF_Map")),  width = 1800, height = 1500)
  }
  
  
  lc_maps <- ggplot(all_lc_frac_maps_dt[  `Land cover class`  %in% this_group$subclasses, ]) + geom_tile(aes(x = Lon, y = Lat, fill = LandCoverFraction)) + scale_fill_viridis(option = "A", name = "Cover fraction (-)") + facet_wrap(~`Land cover class` ) 
  lc_maps <- lc_maps + coord_cartesian() 
  #lc_maps <- lc_maps + labs(title = this_group$name)
  lc_maps <- lc_maps + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                                       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  lc_maps <- lc_maps +  geom_sf(data=overlay, 
                                          fill = "transparent", 
                                          linewidth = 0.2,
                                          colour="green")
  magicPlot(p = lc_maps, filename = file.path(results_dir, paste0(gsub(" ", "_", this_group$name), "_", "LandCoverFraction_Maps")),  width = 1400, height = 1200)
  if(this_group$name == "Main Figures") {
    magicPlot(p = lc_maps, filename = file.path(plot_dir, paste0("FigureS1_NCV_and_Cropland_LC_Fractions")),  width = 1800, height = 1500)
  }
  else if(this_group$name == "Supp Figures NCV") {
    magicPlot(p = lc_maps, filename = file.path(plot_dir, paste0("FigureS2_NCV_LC_Fractions")),  width = 1800, height = 1500)
  }
  
  
}

# make Fig 1
#fig1_upper <- ggarrange(plotlist = list(fig1_histo, fig1_spatial), labels = c("a", "b"), ncol = 2, nrow = 1 )
#fig1_lower <- ggarrange(plotlist = list( fig1_annual, fig1_seasonal), labels = c("c", "d"), common.legend = TRUE, ncol = 2, nrow = 1 )
#fig1 <- ggarrange(plotlist = list(fig1_upper, fig1_lower), ncol = 1, nrow = 2 )

fig1 <- ggarrange(plotlist = list(fig1_histo, fig1_spatial, fig1_annual, fig1_seasonal), 
                  labels = "auto",
                  font.label = list(size = 30, face = "bold"))

print(fig1)
magicPlot(p = fig1, filename = file.path(plot_dir, paste0("Figure1")),  width = 1800, height = 1500)

