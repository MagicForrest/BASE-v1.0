library(ggplot2)
library(ggcorrplot)
library(ggpubr)
# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_correlations_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))

min_landcover_frac <- 0.15

prefix_string <- "BASE_v1.0"

text_multiplier_pdf <- 1.2

#  Directories for reading data and saving plots
pub_results_dir <- here("publication_results/manuscript_BASE_v1.0")
intermediates_dir <- here("intermediates", "GLMs",  prefix_string)
models_dir <- here("results", "GLMs",  prefix_string)

# data to use
# input data directory containing large data.tables already constructed with script 
data_dir <- here("external_files/input_datatables/")
data_version <- "v1.01_publication_with_Kummu_GDP"

#### READ THE MASTER DATATABLE ####
master_dt <- readRDS(file.path(data_dir, paste0("master_full_dt_", data_version, ".rds")))

#### GET ALL THE PREDICTORS ####
source(here("scripts/v1.0_sensitivity_models.R"))


baselines_list <- list(
  
  "NCV" = list(name = "NCV",
               nice_name = "NCV",
               all_model_spec = all_ncv_sensitivity_models(),
               baseline_model_id ="BASE_v1.0",
               baseline_model_name ="BASE v1.0"),
  
  "Cropland" = list(name = "PureCropland",
                    nice_name = "Cropland",
                    all_model_spec = all_cropland_sensitivity_models(),
                    baseline_model_id ="BASE_v1.0",
                    baseline_model_name ="BASE v1.0")     
)



all_corrplots_pdf  <- list()
all_corrplots_png  <- list()

for(this_lcc_class in baselines_list) {
  
  all_predictors <- c()
  for(this_model in this_lcc_class$all_model_spec) {
    all_predictors <- c(all_predictors, this_model$linear_terms, this_model$quadratic_terms, this_model$interaction_terms)
  }
  #all_predictors <- unique(all_predictors)
  all_predictors <- unique(unlist(strsplit(all_predictors, split = "*", fixed = TRUE)))
  
  # add cropland fraction
  if(this_lcc_class$name == "PureCropland") all_predictors <- append(all_predictors, "LandcoverFraction_PureCropland")
  
  # drop predictors that weren't included in the publication
  unused_predictors <- c("deltaNatural10", "deltaNatural20", "deltaNatural30", "deltaNatural40",  "MEPI_LPJmL",
                         "GPP3_2_index", "GPP4_2_index", "GPP5_2_index", "GPP6_2_index", "GPP4_index", "GPP5_index", 
                         "MEPI2", "GPP6_index", "StdDevWindSpeed", "MaxWindSpeed", "Crop_ratio",
                         "Pop_dens_static", "GDP_capita_Wang", "GDP_gridcell_Wang") 
  all_predictors <- all_predictors[! all_predictors %in% unused_predictors]
  
  # calculate extra ones
  if("MEPI" %in% all_predictors) master_dt[ , MEPI := GPP/Max_GPP13]
  if("MEPI2" %in% all_predictors) master_dt[ , MEPI2 := ((GPP+GPP1)/2) /Max_GPP13]
  if("MEPI_LPJmL" %in% all_predictors) master_dt[ , MEPI_LPJmL := GPP_LPJmL/Max_GPP_LPJmL13]
  if("GPP3_index" %in% all_predictors) master_dt[ , GPP3_index := (GPP3/3)/Max_GPP13]
  if("GPP4_index" %in% all_predictors) master_dt[ , GPP4_index := (GPP4/4)/Max_GPP13]
  if("GPP5_index" %in% all_predictors) master_dt[ , GPP5_index := (GPP5/5)/Max_GPP13]
  if("GPP6_index" %in% all_predictors) master_dt[ , GPP6_index := (GPP6/6)/Max_GPP13]
  if("GPP3_2_index" %in% all_predictors) master_dt[ , GPP3_2_index := (GPP3_2/3)/Max_GPP13]
  if("GPP4_2_index" %in% all_predictors) master_dt[ , GPP4_2_index := (GPP4_2/4)/Max_GPP13]
  if("GPP5_2_index" %in% all_predictors) master_dt[ , GPP5_2_index := (GPP5_2/5)/Max_GPP13]
  if("GPP6_2_index" %in% all_predictors) master_dt[ , GPP6_2_index := (GPP6_2/6)/Max_GPP13]
  if("FAPAR_index" %in% all_predictors) master_dt[ , FAPAR_index := FAPAR/Max_FAPAR13]
  if("Crop_ratio" %in% all_predictors) master_dt[ , Crop_ratio := LandcoverFraction_PureCropland/LandcoverFraction_NCV]
  if("PHI" %in% all_predictors) master_dt[ , PHI := (GPP3/3)/Max_GPP13]
  
  
  # do transforms (log)
  for(this_predictor in all_predictors){
    if(substr(this_predictor, 1, 4) == "log_") {
      master_dt[ , (this_predictor):= log(get(substr(this_predictor, 5, nchar(this_predictor))) + 1)]
    }
  }
  
  
  # select those predictors from the big dt
  this_dt <- master_dt[ , ..all_predictors]
  
  # select only the gridcells across the land cover threshold
  # if(landcover == "NCV") 
  this_dt <- this_dt[ get(paste("LandcoverFraction",  this_lcc_class$name, sep = "_")) > min_landcover_frac, ] 
  # if(landcover == "PureCropland") master_dt <- master_dt[ LandcoverFraction_PureCropland > min_landcover_frac, ] 
  
  # drop the land cover fractions because we don't use them in the final predictors
  this_dt[ , LandcoverFraction_NCV := NULL ]
  this_dt[ , LandcoverFraction_PureCropland := NULL ]
  
   all_cor <- cor(na.omit(this_dt), method = "pearson")
  
  # pdf
  pearsons_corrplot_pdf <- ggcorrplot(all_cor, hc.order = TRUE, type = "lower",
                                  outline.col = "white",
                                  ggtheme = ggplot2::theme_bw,
                                  colors = c("#6D9EC1", "white", "#E46726"),
                                  lab = TRUE,
                                  lab_size = 6,
                                  tl.cex = 16,
                                  show.legend = FALSE)
  pearsons_corrplot_pdf  <- pearsons_corrplot_pdf  + labs(title = this_lcc_class$nice_name) + theme(plot.title = element_text(size=26))
  pearsons_corrplot_pdf  <- pearsons_corrplot_pdf  + theme(plot.title = element_text(hjust = 0.5))
  all_corrplots_pdf[[this_lcc_class$nice_name]] <- pearsons_corrplot_pdf 
  
  # png
  pearsons_corrplot_png <- ggcorrplot(all_cor, hc.order = TRUE, type = "lower",
                                      outline.col = "white",
                                      ggtheme = ggplot2::theme_bw,
                                      colors = c("#6D9EC1", "white", "#E46726"),
                                      lab = TRUE,
                                      lab_size = 8,
                                      tl.cex = 20,
                                      show.legend = FALSE)
  pearsons_corrplot_png  <- pearsons_corrplot_png  + labs(title = this_lcc_class$nice_name) + theme(plot.title = element_text(size=26))
  pearsons_corrplot_png  <- pearsons_corrplot_png  + theme(plot.title = element_text(hjust = 0.5))
  all_corrplots_png[[this_lcc_class$nice_name]] <- pearsons_corrplot_png 

}

#### AND COMBINE ####

figb1_png <- ggarrange(plotlist = all_corrplots_png,
                   ncol = 1,
                   labels = "auto",
                   font.label = list(size = 24, face = "bold"), 
                   align = "hv"
)

magicPlot(p = figb1_png, filename = file.path(pub_results_dir, paste("Figure_B01_Predictor_Correlation")), width= 1000, height  = 1800)

figb1_pdf <- ggarrange(plotlist = all_corrplots_pdf,
                       ncol = 1,
                       labels = "auto",
                       font.label = list(size = 24, face = "bold"), 
                       align = "hv"
)


pdf(file = file.path(pub_results_dir, paste0("Figure_B01_Predictor_Correlation.pdf")), width = 10, height = 18)
print(figb1_pdf)
dev.off()

pdf(file = file.path(pub_results_dir, paste0("fig_B01.pdf")), width = 10, height = 18)
print(figb1_pdf)
dev.off()


