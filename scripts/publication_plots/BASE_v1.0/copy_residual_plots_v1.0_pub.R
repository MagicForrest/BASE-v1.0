# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/copy_residual_plots_v1.0_pub.R")
library(here)


# define the models to plot
ncv_model <- "BASE_v1.0"
cropland_model <- "BASE_v1.0"

# 
analysis_version <- "BASE_v1.0"
fit_batch_version <- "BASE_v1.0"


# copy the partial residual and response plots from the fitted model directory to the manuscript plot dir 
file.copy(here("fitted_models", analysis_version, "GLMs", "BurntFraction_NCV", fit_batch_version, ncv_model, "All_Single_Predictors.png"), 
          here("plots", analysis_version, "manuscript/Figure_D01_NCV_Partial_Residuals.png"), overwrite = TRUE )
file.copy(here("fitted_models", analysis_version, "GLMs", "BurntFraction_NCV", fit_batch_version, ncv_model, "All_Single_Predictors.pdf"), 
          here("plots", analysis_version, "manuscript/Figure_D01_NCV_Partial_Residuals.pdf"), overwrite = TRUE )
file.copy(here("fitted_models", analysis_version, "GLMs", "BurntFraction_NCV", fit_batch_version, ncv_model, "All_Single_Predictors.pdf"), 
          here("plots", analysis_version, "manuscript/fig_D01.pdf"), overwrite = TRUE) 
file.copy(here("fitted_models", analysis_version, "GLMs", "BurntFraction_PureCropland", fit_batch_version, cropland_model, "All_Single_Predictors.png"), 
          here("plots", analysis_version, "manuscript/Figure_D02_PureCropland_Partial_Residuals.png"), overwrite = TRUE )
file.copy(here("fitted_models", analysis_version, "GLMs", "BurntFraction_PureCropland", fit_batch_version, cropland_model, "All_Single_Predictors.pdf"), 
          here("plots", analysis_version, "manuscript/Figure_D02_PureCropland_Partial_Residuals.pdf"), overwrite = TRUE )
file.copy(here("fitted_models", analysis_version, "GLMs", "BurntFraction_PureCropland", fit_batch_version, cropland_model, "All_Single_Predictors.pdf"), 
          here("plots", analysis_version, "manuscript/fig_D02.png"), overwrite = TRUE )