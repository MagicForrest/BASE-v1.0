# NOTE: For this script we are reading a table of the SHAP-derived variable important valuesthat we saved when we fitted the model.
# This is because the vi() function used to get the variable importance doesn't seem to work well if it the fitted model object is 
# no longer in it's original environment

library(ggplot2)
# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_variable_importance_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))


prefix_string <- "BASE_v1.0"

#  Directories for reading data and saving plots
pub_results_dir <- here("publication_results/manuscript_BASE_v1.0")
models_dir <- here("results", "GLMs",  prefix_string)


#### DEFINE BASELINES ####

cropland_col <- "orchid4"
ncv_col <- "springgreen4"
  
baselines_list <- list(
  
  "NCV" = list(name = "NCV",
               nice_name = "NCV",
               colour = ncv_col,
               linetype = "dashed",
               baseline_model_id ="BASE_v1.0",
               baseline_model_name ="BASE v1.0"),
  
  "Cropland" = list(name = "PureCropland",
                    nice_name = "Cropland",
                    colour = cropland_col,
                    linetype = "dashed",
                    baseline_model_id ="BASE_v1.0",
                    baseline_model_name ="BASE v1.0")     
  
)




# loop across models to plot
for(this_baseline in baselines_list) {
  

  #### READ AND PLOT THE SHAP STATS ####
  this_coeffs_df <- read.table(file.path(models_dir, paste("BurntFraction", this_baseline$name, sep = "_"),  this_baseline$baseline_model_id, "coeffs_table.txt"), header = TRUE)
  print(this_coeffs_df)
  
}
