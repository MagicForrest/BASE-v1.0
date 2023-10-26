#### LOAD PACKAGES AND DEFINE RELATIVE PATHS ####

if("DGVMTools" %in% (.packages())){  detach("package:DGVMTools", unload=TRUE) }
library(mgcv)
library(mgcViz)
library(ggeffects)
library(voxel)
library(GGally)
library(corrplot)
library(DGVMTools)
library(raster)
library(viridis)
library(caret)
library(tictoc)
library(rnaturalearth)
library(stars)
library(viridis)
library(ggfortify)
library(car)
library(sjPlot)
library(vip)
library(ggcorrplot)


# define root path with here package and 
here::i_am("scripts/fit_BASE.R")
library(here)
source(here("scripts", "glm_fitting_helper_functions.R"))
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))

# input data directory containing large data.tables already constructed with script 
data_dir <- here("external_files/input_datatables/")


#### USER SETTINGS ####

# define the target for the model fitting
landcover = "Natural"
target = "BurntFraction"
dependent_var <- paste(target, landcover, sep = "_")

# do the fitting (if FALSE try to read a previously fitted model)
do_fit <- TRUE

# wether to plot the predictor terms (can sometimes be slow)
plot_terms <- TRUE

# data to use
data_version <- "8.0_treecover"

# version
fit_batch_version <- "v6"
summary_table_name <- paste("summary_metrics", fit_batch_version, sep = "_")


# years for fitting
first_year_available <- 2002
last_year_available <- 2014
# take every even year
fit_years <- (first_year_available:last_year_available)[which(first_year_available:last_year_available %% 2 == 1)]
fit_years <-  first_year_available:(first_year_available+ceiling(length(first_year_available:last_year_available)/2))

# fire size threshold - 0.1 seems to work quite reasonably (see v0.10_newdata plots for Natural)
min_fire_size_ha <- 0.1

# applying scaling or not
apply_scaling <- TRUE

# balance dataset
balance_dataset <- FALSE

# read/write intermediate results to files
quick.read.autodelete <- FALSE

# Only for smooth terms (i.e. testing new predictors)
k <- 8
basis_splines <- "cr"


#### SPECIFY MODELS TO FIT ####

model_specifications_list <- list()

# # A straightforward test with all types of terms
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = "TEST",
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("HDI"),
#                                                                            quadratic_terms = c("PopDens"),
#                                                                            interaction_terms = c("Mean_annual_GPP*FWI"),
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# 
# #### DEFINE PREVIOUS - The previous baseline model ####
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0("PREVIOUS"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FWI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# #### DEFINE 0 - The baseline model ####
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".0_baseline"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# #### DEFINE 1 - Fuel dryness ####
# # Try models with the other components of the FWI and Tmax
# # TODO consider Nesterov index, maybe even VPD?
# all_alt_dryness_predictors <- c("Tmax", "FFMC", "DMC", "DC", "ISI", "BUI", "DSR")
# for(alt_predictor in all_alt_dryness_predictors) {
# 
#   model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                              version_id = paste0(fit_batch_version, ".1_fuel_drying_", alt_predictor),
#                                                                              target = "BurntFraction",
#                                                                              family = quasibinomial(link=logit),
#                                                                              linear_terms = c("Mean_annual_GPP", "FAPAR", alt_predictor, "PopDens", "HDI"),
#                                                                              quadratic_terms = c(),
#                                                                              interaction_terms = c(),  # Form "Var*Var2"
#                                                                              fixed_effect_terms = c("Dominant_type"),
#                                                                              random_effect_terms = c(),
#                                                                              smooth_terms = c(),
#                                                                              select = TRUE)
# }
# # TODO - consider multiple/interactions??
# 
# 
# #### DEFINE 2 - Fuel availability ####
# # TODO add long term Mean_annual_FAPAR, Mean_annual_FAPA, Tree_cover
# all_alt_fuel_availability_predictors <- c("log_Mean_annual_GPP", "FAPAR12", "Mean_annual_FAPAR", "GPP12", "log_GPP12","AGB_Natural", "AGB_Gridcell",  "log_AGB_Natural", "log_AGB_Gridcell", "Treecover_Natural", "Treecover_Gridcell")
# for(alt_predictor in all_alt_fuel_availability_predictors) {
# 
#   model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                              version_id = paste0(fit_batch_version, ".2_fuel_availability_", alt_predictor),
#                                                                              target = "BurntFraction",
#                                                                              family = quasibinomial(link=logit),
#                                                                              linear_terms = c(alt_predictor, "FAPAR", "FWI", "PopDens", "HDI"),
#                                                                              quadratic_terms = c(),
#                                                                              interaction_terms = c(),  # Form "Var*Var2"
#                                                                              fixed_effect_terms = c("Dominant_type"),
#                                                                              random_effect_terms = c(),
#                                                                              smooth_terms = c(),
#                                                                              select = TRUE)
# }
# 
# #### DEFINE 3 - Fuel curing ####
# # TODO indices relative to the max?
# all_alt_fuel_curing_predictors <- c("FAPAR_index", "GPP", "GPP_index")
# for(alt_predictor in all_alt_fuel_curing_predictors) {
# 
#   model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                              version_id = paste0(fit_batch_version, ".3_fuel_curing_", alt_predictor),
#                                                                              target = "BurntFraction",
#                                                                              family = quasibinomial(link=logit),
#                                                                              linear_terms = c("Mean_annual_GPP", alt_predictor, "FWI", "PopDens", "HDI"),
#                                                                              quadratic_terms = c(),
#                                                                              interaction_terms = c(),  # Form "Var*Var2"
#                                                                              fixed_effect_terms = c("Dominant_type"),
#                                                                              random_effect_terms = c(),
#                                                                              smooth_terms = c(),
#                                                                              select = TRUE)
# }
# 
# #### DEFINE 4 - Human Effects ####
# 
# # swap urban for population density
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".4_human_Urban"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "Urban", "HDI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# # swap GDP_capita for HDI
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".4_human_HDI_GDP_capita"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "HDI", "GDP_capita"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# # no human effects
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".4_human_None"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# # interactions
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".4_human_PopDensxHDI"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c( "PopDens*HDI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".4_human_UrbanxHDI"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c( "Urban*HDI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".4_human_PopDens_quadratic_HDI"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "HDI"),
#                                                                            quadratic_terms = c("PopDens"),
#                                                                            interaction_terms = c( ),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# 
# 
# 
# #### DEFINE 5 - Differing tree species flammability ####
# 
# # remove tree types
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".5_tree_flammability_none"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c(),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# # use species types
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_species"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# #### DEFINE 6 - Landscape continuity ####
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".6_continuity_Natural"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI", "Natural"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".6_continuity_Natural_quadratic"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c("Natural"),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# 
# #### DEFINE 7 - Topography ####
# 
# # Slope, TPI and interaction
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".7_topography_Slope"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI", "Slope"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".7_topography_TPI"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI", "TPI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".7_topography_Slope_and_TPI"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".7_topography_SlopexTPI"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c("Slope*TPI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# 
# 
# #### DEFINE 8 - Wind enhancement of RoS ####
# # simply also include wind speed
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version,".8_windspeed_WindSpeed"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI", "WindSpeed"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# 
# #### DEFINE 9 - Spotting ####
# 
# all_alt_woody_predictors <- c("AGB_Natural", "AGB_Gridcell", "Treecover_Natural", "Treecover_Gridcell")
# for(alt_predictor in all_alt_woody_predictors) {
# 
#   model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                              version_id = paste0(fit_batch_version, ".9_spotting_Windspeedx", alt_predictor),
#                                                                              target = "BurntFraction",
#                                                                              family = quasibinomial(link=logit),
#                                                                              linear_terms = c("FAPAR", "FWI", "PopDens", "HDI"),
#                                                                              quadratic_terms = c(),
#                                                                              interaction_terms = c(paste0(alt_predictor,"*WindSpeed")),  # Form "Var*Var2"
#                                                                              fixed_effect_terms = c("Dominant_type"),
#                                                                              random_effect_terms = c(),
#                                                                              smooth_terms = c(),
#                                                                              select = TRUE)
# }
# 
# #### DEFINE 10 - Land abandonment ####
# all_land_abandonment_predictors <- c("deltaNatural10", "deltaNatural20", "deltaNatural30", "deltaNatural40")
# for(alt_predictor in all_land_abandonment_predictors) {
# 
#   model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                              version_id = paste0(fit_batch_version, ".10_land_abandonment_", alt_predictor),
#                                                                              target = "BurntFraction",
#                                                                              family = quasibinomial(link=logit),
#                                                                              linear_terms = c("Mean_annual_GPP", "FAPAR", "FWI", "PopDens", "HDI", alt_predictor),
#                                                                              quadratic_terms = c(),
#                                                                              interaction_terms = c(),  # Form "Var*Var2"
#                                                                              fixed_effect_terms = c("Dominant_type"),
#                                                                              random_effect_terms = c(),
#                                                                              smooth_terms = c(),
#                                                                              select = TRUE)
# }


#### DEFINE CANDIDATES ####
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                              version_id = paste0(fit_batch_version, ".Candidate2"),
#                                                                              target = "BurntFraction",
#                                                                              family = quasibinomial(link=logit),
#                                                                              linear_terms = c("Mean_annual_FAPAR", "FAPAR12", "GPP", "FWI", "PopDens", "HDI", "Slope", "TPI"),
#                                                                              quadratic_terms = c(),
#                                                                              interaction_terms = c(),  # Form "Var*Var2"
#                                                                              fixed_effect_terms = c("Dominant_type"),
#                                                                              random_effect_terms = c(),
#                                                                              smooth_terms = c(),
#                                                                              select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate3"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_FAPAR", "GPP_index", "FWI", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate4"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Treecover_Gridcell", "FAPAR12", "GPP_index", "FWI", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate5"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Treecover_Gridcell", "FAPAR12", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate6"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "GPP_index", "FWI", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate7"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Treecover_Gridcell", "GPP12", "GPP_index", "FWI", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c(),
#                                                                            interaction_terms = c(),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate8"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate8_raw"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)


# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate9"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Mean_annual_GPP", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate10"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "Mean_annual_GPP", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)


# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate11"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("log_Mean_annual_GPP", "PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate12"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("PopDens", "HDI", "Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", "FAPAR12*Mean_annual_GPP"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate13"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", "FAPAR12*Mean_annual_GPP", "PopDens*HDI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate14"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("PopDens", "HDI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", "FAPAR12*Mean_annual_GPP", "Slope*TPI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)


# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate15"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "PopDens", "HDI",  "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell", "Slope"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Check1_smooth_PopDens"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "HDI",  "TPI", "Slope"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c("PopDens"),
#                                                                            select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Check2_smooth_Urban"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "HDI",  "TPI", "Slope"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c("Urban"),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Check3_smooth_Wind"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12", "HDI",  "TPI", "Slope", "PopDens"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c("WindSpeed"),
#                                                                            select = TRUE)




# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, "_VERSION_2"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", ),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, "Candidate16"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("FAPAR12","Slope", "TPI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", "PopDens*HDI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate17"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Slope", "TPI", "HDI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", "PopDens*FAPAR12"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate18"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Slope", "TPI", "PopDens"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI", "HDI*FAPAR12"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)



# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Check3_smooth_roads"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Slope", "TPI","PopDens", "FAPAR12", "HDI"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c("RoadDens"),
#                                                                            select = TRUE)
# 
# model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
#                                                                            version_id = paste0(fit_batch_version, ".Candidate19_plus_RoadDens"),
#                                                                            target = "BurntFraction",
#                                                                            family = quasibinomial(link=logit),
#                                                                            linear_terms = c("Slope", "TPI","PopDens", "FAPAR12", "HDI", "RoadDens"),
#                                                                            quadratic_terms = c("Treecover_Gridcell"),
#                                                                            interaction_terms = c("GPP_index*FWI"),  # Form "Var*Var2"
#                                                                            fixed_effect_terms = c("Dominant_type"),
#                                                                            random_effect_terms = c(),
#                                                                            smooth_terms = c(),
#                                                                            select = TRUE)

model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
                                                                           version_id = paste0(fit_batch_version, ".Candidate20_HDI_x_RoadDens"),
                                                                           target = "BurntFraction",
                                                                           family = quasibinomial(link=logit),
                                                                           linear_terms = c("Slope", "TPI","PopDens", "FAPAR12"),
                                                                           quadratic_terms = c("Treecover_Gridcell"),
                                                                           interaction_terms = c("GPP_index*FWI", "HDI*RoadDens"),  # Form "Var*Var2"
                                                                           fixed_effect_terms = c("Dominant_type"),
                                                                           random_effect_terms = c(),
                                                                           smooth_terms = c(),
                                                                           select = TRUE)

model_specifications_list[[length(model_specifications_list) + 1]] <- list(landcover = "Natural",
                                                                           version_id = paste0(fit_batch_version, ".Candidate21_PopDens_x_RoadDens"),
                                                                           target = "BurntFraction",
                                                                           family = quasibinomial(link=logit),
                                                                           linear_terms = c("Slope", "TPI","HDI", "FAPAR12"),
                                                                           quadratic_terms = c("Treecover_Gridcell"),
                                                                           interaction_terms = c("GPP_index*FWI", "RoadDens*PopDens"),  # Form "Var*Var2"
                                                                           fixed_effect_terms = c("Dominant_type"),
                                                                           random_effect_terms = c(),
                                                                           smooth_terms = c(),
                                                                           select = TRUE)



#### DEFINE DIRECTORIES

# define paths for output (results and intermediate data)
top.results.dir <- here("results", "GLMs", paste(dependent_var, fit_batch_version, sep = "_"))
dir.create(top.results.dir, showWarnings = FALSE, recursive = TRUE)
top.intermediates.dir <- here("intermediates", "GLMs", paste(dependent_var, fit_batch_version, sep = "_"))
dir.create(top.intermediates.dir, showWarnings = FALSE, recursive = TRUE)


not_predictors <- c("Lon", "Lat", "Year", "Month", "BurntFraction", "LandcoverFraction", "N_patches_firecci_CUTOFF12", "N_patches_firecci_CUTOFF6",  "N_patches_MODIS_CUTOFF12",   "N_patches_MODIS_CUTOFF6", "GDP_gridcell", "FuelClass")


# minimum data points
MIN_DATA_POINTS <- 100

month_labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


# fuel_class
all_fuel_classes <- list("Nonfuel" = 7, 
                         "Low shrubland" =  21, 
                         "Medium shrubland" = 22, 
                         "High shrubland" = 23, 
                         "Low grassland" = 31, 
                         "Medium grassland" = 32, 
                         "High grassland" = 33, 
                         "Herbaceous cropland" = 41, 
                         "Woody cropland" = 42, 
                         "Tree wet and peat/semi-peat land" = 51, 
                         "Shrubland wet and peat/semi-peat land" = 52, 
                         "Grassland wet and peat/semi-peat land" = 53, 
                         "Urban continuous fabric" = 61, 
                         "Urban discontinuous fabric" = 62, 
                         "Open broadleaf evergreen forest" = 1111,
                         "Closed broadleaf evergreen forest" = 1112, 
                         "Open broadleaf deciduous forest" = 1121, 
                         "Closed broadleaf deciduous forest " = 1122, 
                         "Open needleleaf evergreen forest" = 1211, 
                         "Closed needleleaf evergreen forest" = 1212, 
                         "Open needleleaf deciduous forest" = 1221,
                         "Closed needleleaf deciduous forest" = 1222, 
                         "Open mixed forest" = 1301, 
                         "Closed mixed forest" = 1302)

combined_fuel_classes <- list("Cropland" = c(41,42),
                              "Forest" = c(1111,1112,1121,1122,1211,1212,1221,1222,1301,1302),
                              "Shrubland" = c(21,22,23),
                              "Grassland" = c(31,32,33),
                              "Urban" = c(61,62),
                              "Peatland" = c(51,52,53),
                              "All" = as.numeric(all_fuel_classes[!all_fuel_classes %in% 7]))

forest_fuel_classes <- list("Natural" = c(1111,1112,1121,1122,1211,1212,1221,1222,1301,1302,21,22,23),
                            "Open forest" = c(1111,1121,1211,1221,1301),
                            "Closed forest" = c(1112,1122,1212,1222,1302),
                            "Evergreen forest" = c(1111,1112,1211,1212),
                            "Deciduous forest" = c(1121,1122,1221,1222),
                            "Mixed forest" = c(1301,1302),
                            "Needleleaf forest" = c(1211,1212,1221,1222),
                            "Broadleaf forest" = c(1111,1112,1121,1122))


#### PLOTTING AND SAVING OPTIONS AND SETTINGS ####

text.multiplier <- 3

# cuts and colours
bf.cuts <- c(0,0.002,0.005,0.01,0.02,0.05,0.10,0.2,0.50,1.0)
bf.cols <- turbo(length(bf.cuts)-1)
ba.cuts <- c(1,2,5,10,20)
ba.cols <- turbo(length(ba.cuts)-1)
percentage.total.ba.cuts <- c(0,0.5,1,2,5,10,20,50,100)


fraction_or_area <- list(
  Fraction = list(name = "Fraction", 
                  title = "Burnt Fraction (%)", 
                  columns = c("Predicted_burnt_fraction","Observed_burnt_fraction"),
                  pretty_columns = c("Predicted burnt fraction","Observed burnt fraction"),
                  cuts =  c(0,0.1,0.2,0.5,1,2,5,10,20,50,100) ),
  Area = list(name = "Area", 
              title = "Burnt Area (ha)", 
              columns = c("Predicted_burnt_area","Observed_burnt_area"),
              pretty_columns = c("Predicted burnt area","Observed burnt area"),
              cuts = c(0,1,2,5,10,20,50,100,200,500,1000,2000,5000,10000))
)


overlay <- rnaturalearth::ne_countries(returnclass = "sf")
sf::sf_use_s2(FALSE)

# make a text file to output the summary metrics
summary_table_name_try <- file.path(top.results.dir, paste0(summary_table_name, ".txt"))
iter <- 1
while(file.exists(summary_table_name_try)) {
  iter <- iter + 1
  summary_table_name_try <- file.path(top.results.dir, paste0(summary_table_name,".r", iter,".txt"))
}
summary_table_name_final <- summary_table_name_try
summary_metrics_table <- data.table()


#### READ DATA.TABLE AND PREPARE ####
master_dt <- readRDS(file.path(data_dir, paste0("master_full_dt_", data_version, ".rds")))

# some data prep
master_dt[ , PopDens := sqrt(PopDens)]
master_dt[ , GDP_capita := sqrt(GDP_capita)]



# get a list of all predictors and do transforms as necessary
all_predictors <- c()
for(model_spec in model_specifications_list) {
  all_predictors <- c(all_predictors, 
                      model_spec$linear_terms, 
                      model_spec$quadratic_terms,
                      model_spec$fixed_effect_terms)
  if(!is.null(model_spec$interaction_terms)) all_predictors <- c(all_predictors, unlist(strsplit(model_spec$interaction_terms, split = "*", fixed = TRUE)))
}
all_predictors <- unique(all_predictors)

# derisome indicees
if("GPP_index" %in% all_predictors) master_dt[ , GPP_index := GPP/Max_GPP12]
if("FAPAR_index" %in% all_predictors) master_dt[ , FAPAR_index := FAPAR/Max_FAPAR12]

# do transforms (log)
for(this_predictor in all_predictors){
  
  if(substr(this_predictor, 1, 4) == "log_") {
    print(substr(this_predictor, 5, nchar(this_predictor)))
    master_dt[ , (this_predictor):= log(get(substr(this_predictor, 5, nchar(this_predictor))) + 1)]
  }
  
}


#### MAIN LOOP FOR EACH MODEL ####
first_model_fit <- TRUE
for(model_spec in model_specifications_list) {
  
  tic()
  
  # make an empty list for the summary metrics (will be written as text at the end)
  summary_metrics <- list()
  
  # read the model specifications for later convenience
  #dependent_var <-  model_spec$target
  version_id <- model_spec$version_id
  #landcover <- model_spec$landcover
  linear_terms <- model_spec$linear_terms
  quadratic_terms <- model_spec$quadratic_terms
  interaction_terms <- model_spec$interaction_terms
  fixed_effect_terms <- model_spec$fixed_effect_terms
  random_effect_terms <- model_spec$random_effect_terms
  smooth_terms <- model_spec$smooth_terms
  
  
  # analysis and plot directories and text info
  plot.dir <- file.path(top.results.dir, version_id)
  dir.create(plot.dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(plot.dir, "GLM"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(plot.dir, "Scaled"), showWarnings = FALSE, recursive = TRUE)
  intermediates.dir <- file.path(top.intermediates.dir, version_id)
  dir.create(intermediates.dir, showWarnings = FALSE, recursive = TRUE)
  this_model_textfile <- file(file.path(plot.dir, "log.txt"), open = "wt")
  writeLines(paste("Input data version:", data_version), this_model_textfile)
  summary_metrics[["Model"]] <- version_id
  
  
  # get predictor vars for subsetting the data.table (splitting the interaction terms to their components where necessary)
  predictor_vars <- c(linear_terms, quadratic_terms, fixed_effect_terms, random_effect_terms, smooth_terms)
  if(!is.null(interaction_terms))  predictor_vars <- c(predictor_vars, unlist(strsplit(interaction_terms, split = "*", fixed = TRUE)) )
  
  # get continuous predictors for the correlation matrix plot (by excluding the fixed and random effects)
  continuous_predictor_vars <- predictor_vars[ !predictor_vars %in% c(fixed_effect_terms, random_effect_terms)] 
  
  
  # build the formula  
  this_formula_str <- paste(dependent_var, "~")
  if(length(linear_terms) > 0) this_formula_str <- paste(this_formula_str, paste(linear_terms, collapse =  " + "))
  if(length(quadratic_terms) > 0) this_formula_str <- paste(this_formula_str, paste(paste0("poly(", quadratic_terms, ",2,raw=TRUE)"), collapse =  " + "), sep = " + ")
  if(length(interaction_terms) > 0) this_formula_str <- paste(this_formula_str, paste(interaction_terms, collapse =  " + "), sep = " + ")
  if(length(fixed_effect_terms) > 0) this_formula_str <- paste(this_formula_str, paste(paste0(fixed_effect_terms), collapse =  " + "), sep = " + ")
  if(length(smooth_terms) > 0) this_formula_str <- paste(this_formula_str, paste(paste0("s(", smooth_terms, ", k=", k, ", bs='", basis_splines, "')"), collapse =  " + "), sep = " + ")
  writeLines(c("Model formula:", this_formula_str), this_model_textfile)
  # convert to formula
  this_formula <- as.formula(this_formula_str)
  
  
  # remove the previous GLM and clean up 
  suppressWarnings(rm(this_m))
  gc()
  
  head_text <- paste("******** Running model", model_spec$version_id, "********")
  message("")
  message(strrep("*", nchar(head_text)))
  message(head_text)
  message(strrep("*", nchar(head_text)))
  message("")
  
  
  
  
  # simply don't run if not enough data points
  if(nrow(master_dt) > MIN_DATA_POINTS) {
    
    #### CORRELATION PLOT ####  
    
    # make and save correlation plot
    all_cor <- cor(na.omit(master_dt[ , ..continuous_predictor_vars]), method = "pearson")
    pearsons_corrplot <- ggcorrplot(all_cor, hc.order = TRUE, type = "lower",
                                    outline.col = "white",
                                    ggtheme = ggplot2::theme_gray,
                                    colors = c("#6D9EC1", "white", "#E46726"),
                                    lab = TRUE,
                                    lab_size = 8,
                                    tl.cex = 24,
                                    show.legend = FALSE)
    magicPlot(p = pearsons_corrplot, filename = file.path(plot.dir, paste("PearsonsCorrelation", sep = "_")), height = 800, width =800)
    #print(pearsons_corrplot)
    
    
    
    #### SUBSET THE DATA ####
    
    # select the columns we need for this particular model
    if("Year" %in% names(master_dt))  { 
      col_subset <- c("Lon", "Lat", "Year","Month",  dependent_var, paste("LandcoverFraction", landcover, sep = "_"), "FuelClass", predictor_vars)
    } else {
      col_subset <- c("Lon", "Lat", "Month",  dependent_var, paste("LandcoverFraction", landcover, sep = "_"), "FuelClass", predictor_vars)
    }
    
    # select columns (variables) and rows (only the fitting years) and require at least 0.1% of the area covered by the landcover type
    valid_subset_dt <- na.omit(master_dt[get(paste("LandcoverFraction", landcover, sep = "_")) > 0.01, ..col_subset])
    fitting_dt <- valid_subset_dt[ Year %in% fit_years,]
    
    
    # clean some rich/highly urban gridcells
    if("GDP_capita" %in% predictor_vars)  fitting_dt <- fitting_dt[GDP_capita < sqrt(200000),]
    if("Urban" %in% predictor_vars) fitting_dt <- fitting_dt[Urban < 0.5,] 
    # limit the predictor to 1.0 in case of very rare numerical artefacts
    if(target == "BurntFraction") fitting_dt <- fitting_dt[ , c(dependent_var) := cap_at_1(.SD), .SDcols = dependent_var] 
    # if Natural
    if(landcover == "Natural") fitting_dt <- fitting_dt[ LandcoverFraction_Natural > 0.15, ] 
    
    
    
    # if("Urban" %in% predictor_vars) fitting_dt <- fitting_dt[Urban < 0.5,] 
    
    
    # balance dataset
    if(balance_dataset) {
      # for gricells selection, average across years 
      fitting_dt_annual <- fitting_dt[ ,  lapply(.SD, FUN=sum), by = c("Lon", "Lat")]
      
      
      fitting_dt_annual[ , Lon1 := floor(Lon)]
      fitting_dt_annual[ , Lat1 := floor(Lat)]
      set.seed(1234)
      
      burnt_dt <- fitting_dt_annual[ get(dependent_var) > 0, .(burnt = .N), by = c("Lon1", "Lat1")]
      unburnt_dt <- fitting_dt_annual[ get(dependent_var) == 0, .(unburnt = .N), by = c("Lon1", "Lat1")]
      
      totals_dt <- merge(burnt_dt, unburnt_dt, all = TRUE)
      totals_dt$burnt[is.na( totals_dt$burnt)] <- 0
      totals_dt$unburnt[is.na( totals_dt$unburnt)] <- 0
      totals_dt[, total := burnt+unburnt]
      totals_dt[, frac_burnt := burnt / (burnt+unburnt)]
      
      mean(totals_dt[["burnt"]], na.rm = TRUE)
      
      
      burnt_frac_plot <- ggplot(totals_dt) + geom_raster(aes(x = Lon1, y = Lat1, fill = frac_burnt)) + coord_equal()
      total_gc_plot <- ggplot(totals_dt) + geom_raster(aes(x = Lon1, y = Lat1, fill = total)) + coord_equal()
      
      selected_gridcells <- data.table()
      for(irow in 1:nrow(totals_dt)){
        
        this_Lat1 = totals_dt[irow, Lat1]
        this_Lon1 = totals_dt[irow, Lon1]
        this_dt <- fitting_dt_annual[Lon1 == this_Lon1 & Lat1 == this_Lat1 , ]
        
        # if more burnt than unburnt, take all unburnt gridcells
        if(totals_dt[irow, burnt] >= totals_dt[irow, unburnt]) {
          selected_gridcells <- rbind(selected_gridcells, this_dt[ , c("Lon", "Lat")])
        }
        else {
          # take all burnt
          this_burnt_dt <- this_dt[ get(dependent_var) > 0, c("Lon", "Lat")]
          selected_gridcells <- rbind(selected_gridcells, this_burnt_dt)
          # take a matching amount of unburnt as burnt, with a minimum of 15% gridcells (or failing that a single gricell)
          this_unburnt_dt <- this_dt[ get(dependent_var) == 0, c("Lon", "Lat")]
          num_unburnt_sample <- max(nrow(this_burnt_dt), ceiling(0.15 * nrow(this_dt)))
          this_unburnt_dt <- this_unburnt_dt[ , c("Lon", "Lat")][sample(x = 1:nrow(this_unburnt_dt), size = num_unburnt_sample, replace = FALSE),]
          selected_gridcells <- rbind(selected_gridcells, this_unburnt_dt)
        }
        
      }
      
      message(paste("Subsetting", nrow(selected_gridcells), "out of", nrow(fitting_dt_annual), "gridcells"))
      #fitting_dt <- fitting_dt[selected_gridcells]
    }
    
    
    #### START FIT ####
    if(do_fit) {
      message(" ** Starting fit with formula:")
      print(this_formula_str)
      tic()
      this_m <-  mgcv::gam(this_formula,
                           data = fitting_dt, 
                           method = "REML",
                           select = model_spec$select,
                           family = model_spec$family)
      fit_timing <- capture.output(toc())
      writeLines(c("Total fit time", fit_timing) , con = this_model_textfile)
      print(fit_timing)
      
      print(" ** Summarising model")
      this_summary <- summary(this_m)
      print(this_summary)
      writeLines(capture.output(print(this_summary)), con = this_model_textfile)
      
      this_deviance_explained <- 1 - (this_m$deviance/this_m$null.deviance)
      summary_metrics[["Deviance explained"]] <- round(this_deviance_explained, 3)
      print(paste0(" ** Finished fit.  Deviance explained = ", signif(this_deviance_explained, 3) *100, "%"))
      
      
      ####  CALCULATE PREDICTED VALUES #### 
      print(" ** Making full prediction")
      # note the requirement add that the prediction is only done in gridcells where there is at least 1% of the landcover type present
      predicted_dt <- copy(valid_subset_dt)[ , "Predicted_burnt_fraction" := predict(object = this_m, newdata = valid_subset_dt, type = "response")]
      setnames(predicted_dt, dependent_var, "Observed_burnt_fraction")
      print(" ** Done.")
      # calculate burnt area from burnt fraction, landcover fraction and gridcell area
      print(" ** Calculating gridecell area")
      predicted_dt <- addArea(predicted_dt, "ha", tolerance = 0.0000001)
      predicted_dt[, Landcover_area_ha :=  get(paste("LandcoverFraction", landcover, sep = "_")) * Area]
      predicted_dt[, Predicted_burnt_area_raw := Predicted_burnt_fraction * Landcover_area_ha]
      predicted_dt[, Observed_burnt_area := Observed_burnt_fraction * Landcover_area_ha]
      predicted_dt[, Predicted_burnt_fraction := Predicted_burnt_fraction * 100]
      predicted_dt[, Observed_burnt_fraction := Observed_burnt_fraction * 100]
      print(" ** Done.")
      
      
      #### APPLY THRESHOLD ####
      
      ### Remove small monthly burnt areas from the prediction    
      unscaled_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_raw"]])
      summary_metrics[["Raw full NME"]] <- round(unscaled_NME, 3)
      print(unscaled_NME)
      predicted_dt[, Predicted_burnt_area_threshold := fifelse(Predicted_burnt_area_raw > min_fire_size_ha, Predicted_burnt_area_raw, 0)]
      threshold_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_threshold"]])
      print(threshold_NME)
      
      #### APPLY SCALING ####
      if(apply_scaling) {
        
        print(" ** Applying scaling")
        tic()
        # apply an exponent and then scale the burnt area
        observed_total_ba <- sum(predicted_dt[["Observed_burnt_area"]])
        min_NME <- 100
        best_exp <- best_scaling <- NULL
        for(this_exponent in seq(0.1, 5, 0.1)) {
          
          this_exp_predictions <- predicted_dt$Predicted_burnt_area_threshold^this_exponent
          this_scaling <- observed_total_ba / sum(this_exp_predictions)
          this_final <- this_exp_predictions *  this_scaling
          this_NME <-  calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = this_final)
          
          if(this_NME < min_NME) {
            min_NME <- this_NME
            best_exp <- this_exponent
            best_scaling <- this_scaling
          }
          
        }
        print(" ** Done.")
        toc()
        print(paste("Final NME:", min_NME))
        print(paste("Scaling Factor:", best_scaling))
        print(paste("Exponent:", best_exp))
        
        
        summary_metrics[["Scaled full NME"]] <- round(min_NME, 3)
        summary_metrics[["Scaling Factor"]] <- round(best_scaling, 3)
        summary_metrics[["Exponent"]] <- round(best_exp, 3)
        
        # select the fial predicted burnt area
        predicted_dt[, Predicted_burnt_area_scaled := best_scaling * Predicted_burnt_area_threshold^best_exp]
        subset_to_save <- predicted_dt[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw", "Predicted_burnt_area_scaled", "Observed_burnt_area", "Predicted_burnt_fraction", "Observed_burnt_fraction")]
        
      } else {
        subset_to_save <- predicted_dt[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw", "Observed_burnt_area", "Predicted_burnt_fraction", "Observed_burnt_fraction")]
      }
      
      
      # save the GLM and the data to the intermediates directory
      print(" ** Saving model")
      tic()
      saveRDS(object = this_m, file = file.path(intermediates.dir, paste("GLM", version_id, "rds", sep = ".")))
      toc()
      print(" ** Saving results data.frame")
      tic()
      saveRDS(object = predicted_dt, file = file.path(intermediates.dir, paste("DT", version_id, "rds", sep = ".")))
      toc()
      
    } else {
      print(" ** Reading GLM from disk")
      this_m <- readRDS(file = file.path(intermediates.dir, paste("GLM", version_id, "rds", sep = ".")))
      predicted_dt <- readRDS(file = file.path(intermediates.dir, paste("DT", version_id, "rds", sep = ".")))
      #predictor_vars <- names(this_m$model)
    }
    
  }
  
  # additional plotting and analysis
  if(exists("this_m")) {
    
    # if only only linear terms, calculate the variable inflation factor (VIF)
    if(length(quadratic_terms) == 0 & length(interaction_terms) == 0 & length(fixed_effect_terms) == 0){
      print(" ** Calculating variable inflation factor")
      tic()
      this_vif <- vif(this_m)
      print(this_vif)
      writeLines(capture.output(print(this_vif)), con = this_model_textfile)
      toc()
    }
    
    # # subtitle of variance explained and number of data points
    subtitle_text <- paste0("Dev. expl. = ",  round(this_deviance_explained * 100, 1), "%")
    caption_text <- paste(dependent_var, version_id, paste0("Dev. expl. = ",  round(this_deviance_explained * 100, 1), "%"), sep = ", ")
    
    
    # Variable Importance through Permutation (VIP)
    # try catch
    
    
    
    ########################### PLOT PREDICTORS ##############################
    print(" ** Plotting predictors")
    
    # all plots for maybe combining in different figures
    all_ggplot_list <- list()
    
    # generate a template data frame for prediction
    print("  ***** Calculating fixed predictor values.")
    tic()
    template_dt <- calculatePredictionDT(dt = master_dt, model = this_m, method = mean, npoints = 100) 
    toc()
    
    # PLOT SINGLE TERMS
    for(this_var in c(linear_terms, quadratic_terms, smooth_terms)){
      print(paste0("   **** Plotting simple predictor:", this_var))
      
      all_ggplot_list[[this_var]] <- plotSimpleTerm (this_var, this_m, template_dt, range(master_dt[[this_var]], na.rm = TRUE))
      effect_plot <- all_ggplot_list[[this_var]] + theme(text = element_text(size = theme_get()$text$size * text.multiplier))#,
      effect_plot <- effect_plot + labs(caption = caption_text, y = paste("Burnt Fracttion [-] (other covariates fixed at mean)"))
      
      ### save it
      magicPlot(p = effect_plot, filename = file.path(plot.dir, paste("Predictor_Single", this_var, sep = "_")), height = 1300, width =1800)
    }
    
    
    # PLOT INTERACTION TERMS
    for(this_interaction in interaction_terms){
      
      print(paste0("   **** Plotting interation:", this_interaction))
      
      # determine the variables
      these_vars <-  unlist(strsplit(x =this_interaction, split = "*", fixed = TRUE))
      
      # if it is a two-way interaction  
      if(length(these_vars) == 2) {
        these_ranges <- list()
        these_ranges[[these_vars[1]]] <- range(master_dt[[these_vars[1]]], na.rm = TRUE)
        these_ranges[[these_vars[2]]] <- range(master_dt[[these_vars[2]]], na.rm = TRUE)
        all_ggplot_list[[this_interaction]] <- plotInteractionTerm(vars = these_vars, 
                                                                   model =  this_m,
                                                                   dt = template_dt, 
                                                                   ranges = these_ranges)
        
        these_ranges[[these_vars[2]]] <- range(master_dt[[these_vars[2]]], na.rm = TRUE)
        test_plot <- plotInteractionTerm(vars = these_vars, model =  this_m, dt = template_dt, ranges = these_ranges)
      }
      
      # tweak and save plot
      test_plot <- test_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
      test_plot <- test_plot + labs(caption = caption_text)
      magicPlot(p = test_plot, filename = file.path(plot.dir, paste("Predictor_Interaction", paste0(these_vars[1], "x", these_vars[2]), sep = "_")), height = 1300, width =1800)
    }
    
    
    
    
    ############ PREDICTIONS IN TIME AND SPACE ################
    
    #### SPATIAL PREDICTIONS ####
    
    # make an appropriate overlay
    all_lons <- sort(unique(predicted_dt[["Lon"]]))
    all_lats <- sort(unique(predicted_dt[["Lat"]]))
    plot_region <- c(xmin = min(all_lons), xmax = max(all_lons), ymin = min(all_lats), ymax = max(all_lats))
    this_overlay <- st_crop(overlay, plot_region)
    
    for(scaled_or_unscaled in c("GLM", "Scaled")) {
      
      if(scaled_or_unscaled == "GLM" | (scaled_or_unscaled == "Scaled" & apply_scaling)) {
        
        # set either scaled or not
        if(scaled_or_unscaled == "GLM") predicted_dt[ , Predicted_burnt_area := Predicted_burnt_area_raw]    
        else predicted_dt[ , Predicted_burnt_area := Predicted_burnt_area_scaled]    
        
        # monthly
        print(" ** Plotting monthly predictions")
        
        # average across years
        predicted_dt_meanyear <- predicted_dt[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon","Lat", "Month"), .SDcols = c("Predicted_burnt_fraction", "Observed_burnt_fraction", "Predicted_burnt_area", "Observed_burnt_area")]
        
        # make monthly plots
        for(this_month in unique(predicted_dt_meanyear[["Month"]])) {
          
          this_prediction <- predicted_dt_meanyear[Month == this_month, ]
          this_prediction[, Month := NULL]
          
          setnames(this_prediction,  gsub(pattern = "_", replacement = " ", names(this_prediction), ))
          this_prediction <- melt(this_prediction, id.vars = c("Lon", "Lat"))
          
          for(this_var in fraction_or_area) {
            
            this_prediction_fraction_or_area <- this_prediction[ variable %in% this_var$pretty_columns,]
            this_prediction_fraction_or_area[ , value := cut(value, this_var$cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
            
            
            this_prediction_plot <- ggplot(this_prediction_fraction_or_area[ variable %in% this_var$pretty_columns,]) +  geom_raster(aes(x = Lon, y = Lat, fill = value)) + scale_fill_viridis(option = "H", name = this_var$title, discrete = TRUE) + facet_wrap(~variable)
            this_prediction_plot <- this_prediction_plot + labs(title = paste("Burnt", this_var$name, "in", landcover,":",  month_labels[this_month]),
                                                                caption = caption_text)
            this_prediction_plot <- this_prediction_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
            suppressWarnings({
              this_prediction_plot <- this_prediction_plot + coord_cartesian() + geom_sf(data=this_overlay, 
                                                                                         fill = "transparent", 
                                                                                         linewidth = 0.1,
                                                                                         colour= "black")
              magicPlot(p = this_prediction_plot, filename = file.path(plot.dir, scaled_or_unscaled, paste("MonthlyPrediction",  paste0("Burnt", this_var$name),  paste(this_month, month_labels[this_month], sep = "-"), sep = "_")), height = 900, width =1300)
            })
          }
        }
        
        # annual
        print(" ** Plotting annual predictions")
        
        this_prediction <- predicted_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat")] 
        this_prediction[, Month := NULL]
        
        setnames(this_prediction,  gsub(pattern = "_", replacement = " ", names(this_prediction), ))
        this_prediction <- melt(this_prediction, id.vars = c("Lon", "Lat"))
        
        spatial_NME <- calcNME(this_prediction[variable == "Observed burnt area", value], this_prediction[variable == "Predicted burnt area", value])
        summary_metrics[[paste(scaled_or_unscaled, "Spatial NME")]] <- round(spatial_NME, 3)
        print(paste("Final spatial burnt area NME =",  round(spatial_NME, 3)))
        
        for(this_var in fraction_or_area) {
          
          this_prediction_fraction_or_area <- this_prediction[ variable %in% this_var$pretty_columns,]
          this_prediction_fraction_or_area[ , value := cut(value, this_var$cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
          
          
          this_prediction_plot <- ggplot(this_prediction_fraction_or_area[ variable %in% gsub("_", " ", this_var$pretty_columns, fixed = TRUE),]) +  geom_raster(aes(x = Lon, y = Lat, fill = value)) + scale_fill_viridis(option = "H", name = this_var$title, discrete = TRUE) + facet_wrap(~variable)
          this_prediction_plot <- this_prediction_plot + labs(title = paste("Burnt", this_var$name, "in", landcover,": Annual"),
                                                              caption = caption_text)
          this_prediction_plot <- this_prediction_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
          this_prediction_plot <- this_prediction_plot + coord_cartesian() + geom_sf(data=this_overlay, 
                                                                                     fill = "transparent", 
                                                                                     linewidth = 0.1,
                                                                                     colour= "black")
          magicPlot(p = this_prediction_plot, filename = file.path(plot.dir, scaled_or_unscaled, paste("AnnualPrediction",  paste0("Burnt", this_var$name), sep = "_")), height = 900, width =1300)
          
        }
        
        #### SUBANNUAL PREDICTIONS ####
        print(" ** Plotting climatology prediction")
        
        # sum burnt areas for each year 
        prediction_dt_subannual <- predicted_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
        prediction_dt_subannual <- melt(prediction_dt_subannual, id.vars = c("Month"), variable.name = "Type", value.name = "Burnt_area")
        this_prediction_ts_plot <- ggplot(prediction_dt_subannual) +  geom_line(data = prediction_dt_subannual, aes(x = Month, y = Burnt_area, col = Type)) + ylab("Burnt area (ha)")
        this_prediction_ts_plot <- this_prediction_ts_plot + scale_x_continuous(breaks = 1:12, labels = month_labels) + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
        #print(this_prediction_ts_plot)
        this_prediction_ts_plot <- this_prediction_ts_plot + labs(title = paste("Burnt Area in landcover:", landcover),
                                                                  caption = caption_text)
        
        magicPlot(p = this_prediction_ts_plot, filename = file.path(plot.dir, scaled_or_unscaled, paste("SeasonalPrediction", sep = "_")), height = 1000, width =1500)
        
        seasonal_NME <- calcNME(prediction_dt_subannual[Type == "Observed_burnt_area", Burnt_area], prediction_dt_subannual[Type == "Predicted_burnt_area", Burnt_area])
        summary_metrics[[paste(scaled_or_unscaled, "Seasonal NME")]] <- round(seasonal_NME, 3)
        print(paste("Final seasonal burnt area NME =",  round(seasonal_NME, 3)))
        
        
        #### ANNUAL TS (IAV) PREDICTIONS ####
        
        #  sum all month in the years
        predicted_dt_yearly <- predicted_dt[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon","Lat","Year"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
        
        # sum across all gridcells melt
        predicted_dt_yearly <- predicted_dt_yearly[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
        
        # plot
        predicted_dt_yearly_melted <- melt(predicted_dt_yearly, id.vars = c("Year"), variable.name = "Type", value.name = "Burnt_area")
        
        #setnames(predicted_dt_yearly, c("FireCCI51", "BASE"), c("Observed_burnt_area", "Predicted_burnt_area"))
        
        this_prediction_yearly_plot <- ggplot(predicted_dt_yearly_melted) +  geom_line(data = predicted_dt_yearly_melted, aes(x = Year, y = Burnt_area, col = Type)) + ylab("Burnt area (ha)")
        this_prediction_yearly_plot <- this_prediction_yearly_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
        #print(this_prediction_ts_plot)
        this_prediction_yearly_plot <- this_prediction_yearly_plot + labs(title = paste("Burnt Area in landcover:", landcover),
                                                                          caption = caption_text)
        
        magicPlot(p = this_prediction_yearly_plot, filename = file.path(plot.dir, scaled_or_unscaled, paste("YearlyTSPrediction", sep = "_")), height = 1000, width =1500)
        
        iav_NME <- calcNME(predicted_dt_yearly_melted[Type == "Observed_burnt_area", Burnt_area], predicted_dt_yearly_melted[Type == "Predicted_burnt_area", Burnt_area])
        summary_metrics[[paste(scaled_or_unscaled, "IAV NME")]] <- round(iav_NME, 3)
        print(paste("Final IAV burnt area NME =",  round(iav_NME, 3)))
        

      iav_plot <- IAVPlot(predicted_dt_yearly, #linewidths = c(2,2), linetypes = c("solid", "solid"),
                  filename = file.path(plot.dir, scaled_or_unscaled, paste("YearlyTSPrediction", "2", sep = "_")), width = 1400, height = 900)
     # iav_plot <- iav_plot + scale_color_viridis_b(labels = as_labeller(c("Predicted_burnt_area" = "BASE", "Observed_burnt_area" = "FireCCI51")))
      iav_plot <- iav_plot + scale_color_viridis_d(labels = c("FireCCI51", "BASE"), option = "viridis")
      iav_plot <- iav_plot + scale_linewidth_manual(values = c(2,2), labels = c("FireCCI51", "BASE"))
      iav_plot <- iav_plot + scale_linetype_manual(values = c("solid", "solid"), labels = c("FireCCI51", "BASE"))
      iav_plot <- iav_plot + geom_smooth(method=lm) 
      iav_plot <- iav_plot + geom_line() 
      
      print(iav_plot)
      
      iav_NME <- calcNME(predicted_dt_yearly, predicted_dt_yearly_melted[Type == "Predicted_burnt_area", Burnt_area])
      
        
        
      } # catch if no scaling applied
      
    } # for scaled and unscaled
    
    ### END PREDICTION CODE
    
  } else {
    print(" ** No GLM fitted or found on disk")
  }
  
  
  full_timing <- capture.output(toc())
  writeLines(c("Total run time:", full_timing) , con = this_model_textfile)
  close(this_model_textfile)
  fwrite(x = summary_metrics_table,
         file = file.path(plot.dir, "log.txt"),
         sep = ",",
         col.names=T,
         append=T)
  
  # (over-)write the summury metric file every time (so we have some results even if run is incomplete)
  summary_metrics_table <- rbind(summary_metrics_table, summary_metrics )
  write.csv(x = summary_metrics_table, file = summary_table_name_final, row.names = FALSE)
  
} # for specified model


