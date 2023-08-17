#### PREAMBLE ####

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

# define root path with here package and 
here::i_am("scripts/fit_BASE.R")
library(here)
source(here("scripts", "glm_fitting_helper_functions.R"))
source(here("scripts", "plot_utils.R"))

# input data dir containing large data.tables already constructed with script 
data_dir <- here("external_files/input_datatables/")

# define paths for output (plots and intermediate data)
top.plot.dir <- here("plots")
dir.create(top.plot.dir, showWarnings = FALSE, recursive = TRUE)
top.intermediates.dir <- here("intermediates")
dir.create(top.intermediates.dir, showWarnings = FALSE, recursive = TRUE)



#### USER SETTINGS ####

# do the fitting (if FALSE try to read a previously fitted model)
do_fit <- TRUE

# wether to plot the predictor terms (can sometimes be slow)
plot_terms <- TRUE

# data to use
data_version <- "6.0_agb"

# years for fitting
first_year_available <- 2002
last_year_available <- 2014
# take every even year
fit_years <- (first_year_available:last_year_available)[which(first_year_available:last_year_available %% 2 == 1)]
fit_years <-  first_year_available:(first_year_available+ceiling(length(first_year_available:last_year_available)/2))

# fire size threshold - 0.1 seems to work quite reasonably (see v0.10_newdata plots for Natural)
min_fire_size_ha <- 0.0

# applying scaling or not
apply_scaling <- FALSE

# balance dataset

# read/write intermediate results to files
quick.read.autodelete <- FALSE

# Only for smooth terms (i.e. testing new predictors)
k <- 8
basis_splines <- "cr"


#### SPECIFY MODELS TO FIT ####


# define models for different land cover classes 
model_specifications_list <- list(
  
  list(landcover = "Natural",
                 version_id = "TEST",
                 target = "BurntFraction",
                 family = quasibinomial(link=logit),
                 #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
                 linear_terms = c("PopDens", "HDI"),
                 quadratic_terms = c(),#c("GPP12"),
                 interaction_terms = c("mean_annual_GPP*FWI"),
                 fixed_effect_terms = c("Dominant_type"),
                 random_effect_terms = c(),
                 smooth_terms = c(),
                 select = TRUE

  )
  
)
  # 
  # list(landcover = "Natural",
  #                version_id = "v2.0.0_base",
  #                target = "BurntFraction",
  #                family = quasibinomial(link=logit),
  #                #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #                linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI"),
  #                quadratic_terms = c(),#c("GPP12"),
  #                interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #                fixed_effect_terms = c("Dominant_type"),
  #                random_effect_terms = c(),
  #                smooth_terms = c(),
  #                select = TRUE
  #               
  # ),
  # 
  # 
  # list(landcover = "Natural",
  #             version_id = "v2.1.0_wind",
  #             target = "BurntFraction",
  #             family = quasibinomial(link=logit),
  #             #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #             linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI", "WindSpeed"),
  #             quadratic_terms = c(),#c("GPP12"),
  #             interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #             fixed_effect_terms = c("Dominant_type"),
  #             random_effect_terms = c(),
  #             smooth_terms = c(),
  #             select = TRUE
  #             
  # ),
  # 
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.2.0_topography",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI", "Slope", "TPI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  #      
  # ),
  # 
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.3.0_swap_FAPAR12",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("FAPAR12", "FWI", "PopDens", "HDI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # 
  # ),
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.3.1_include_FAPAR",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("FAPAR", "mean_annual_GPP", "FWI", "PopDens", "HDI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # 
  # ),
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.3.2_include_FAPAR_swap_FAPAR12",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("FAPAR", "FAPAR12", "FWI", "PopDens", "HDI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.4.1_devGPP_dev_Prec1_dev_GPP1",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI", "annual_Prec_rel_deviation_1", "annual_GPP_rel_deviation_1"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.4.2_devGPP_dev_Prec2_dev_GPP2",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI", "annual_Prec_rel_deviation_2", "annual_GPP_rel_deviation_2"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  # 
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.4.3_devGPP_dev_Prec3_dev_GPP3",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI", "annual_Prec_rel_deviation_3", "annual_GPP_rel_deviation_3"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  # 
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.4.4_devGPP_dev_Prec1_x_dev_GPP1",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c("annual_Prec_rel_deviation_1*annual_GPP_rel_deviation_1"),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  # 
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.4.5_devGPP_dev_Prec2_x_dev_GPP2",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c("annual_Prec_rel_deviation_2*annual_GPP_rel_deviation_2"),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  # 
  # list(landcover = "Natural",
  #      version_id = "v2.4.6_devGPP_dev_Prec3_x_dev_GPP3",
  #      target = "BurntFraction",
  #      family = quasibinomial(link=logit),
  #      #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
  #      linear_terms = c("mean_annual_GPP", "FWI", "PopDens", "HDI"),
  #      quadratic_terms = c(),#c("GPP12"),
  #      interaction_terms = c("annual_Prec_rel_deviation_3*annual_GPP_rel_deviation_3"),  #c("FWI*log_GPP12") # Form "Var*Var2"
  #      fixed_effect_terms = c("Dominant_type"),
  #      random_effect_terms = c(),
  #      smooth_terms = c(),
  #      select = TRUE
  # ),
  
#   list(landcover = "Natural",
#        version_id = "v2.4.7_devGPP_dev_Prec1_dev_GPP1",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_JJA_GPP", "FWI", "PopDens", "HDI", "JJA_Prec_rel_deviation_1", "JJA_GPP_rel_deviation_1"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.8_devGPP_dev_Prec2_dev_GPP2",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_JJA_GPP", "FWI", "PopDens", "HDI", "JJA_Prec_rel_deviation_2", "JJA_GPP_rel_deviation_2"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.9_devGPP_dev_Prec3_dev_GPP3",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_JJA_GPP", "FWI", "PopDens", "HDI", "JJA_Prec_rel_deviation_3", "JJA_GPP_rel_deviation_3"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.10_devGPP_dev_Prec1_x_dev_GPP1",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_JJA_GPP", "FWI", "PopDens", "HDI"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c("JJA_Prec_rel_deviation_1*JJA_GPP_rel_deviation_1"),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.11_devGPP_dev_Prec2_x_dev_GPP2",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_JJA_GPP", "FWI", "PopDens", "HDI"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c("JJA_Prec_rel_deviation_2*JJA_GPP_rel_deviation_2"),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.12_devGPP_dev_Prec3_x_dev_GPP3",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_JJA_GPP", "FWI", "PopDens", "HDI"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c("JJA_Prec_rel_deviation_3*JJA_GPP_rel_deviation_3"),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.13_devGPP_dev_Prec1_dev_GPP1",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_GS_GPP", "FWI", "PopDens", "HDI", "GS_Prec_rel_deviation_1", "GS_GPP_rel_deviation_1"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.14_devGPP_dev_Prec2_dev_GPP2",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_GS_GPP", "FWI", "PopDens", "HDI", "GS_Prec_rel_deviation_2", "GS_GPP_rel_deviation_2"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.15_devGPP_dev_Prec3_dev_GPP3",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_GS_GPP", "FWI", "PopDens", "HDI", "GS_Prec_rel_deviation_3", "GS_GPP_rel_deviation_3"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c(),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.16_devGPP_dev_Prec1_x_dev_GPP1",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_GS_GPP", "FWI", "PopDens", "HDI"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c("GS_Prec_rel_deviation_1*GS_GPP_rel_deviation_1"),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.17_devGPP_dev_Prec2_x_dev_GPP2",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_GS_GPP", "FWI", "PopDens", "HDI"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c("GS_Prec_rel_deviation_2*GS_GPP_rel_deviation_2"),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   ),
#   
#   list(landcover = "Natural",
#        version_id = "v2.4.18_devGPP_dev_Prec3_x_dev_GPP3",
#        target = "BurntFraction",
#        family = quasibinomial(link=logit),
#        #linear_terms = c("FWI", "GPP12", "GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#        linear_terms = c("mean_GS_GPP", "FWI", "PopDens", "HDI"),
#        quadratic_terms = c(),#c("GPP12"),
#        interaction_terms = c("GS_Prec_rel_deviation_3*GS_GPP_rel_deviation_3"),  #c("FWI*log_GPP12") # Form "Var*Var2"
#        fixed_effect_terms = c("Dominant_type"),
#        random_effect_terms = c(),
#        smooth_terms = c(),
#        select = TRUE
#   )
#   
#   
#   # ,
#   # cropland = list(landcover = "Cropland",
#   #                  version_id = "v0.11.0_properGPP_3wayint",
#   #target = "BurntFraction",
#   #family = quasibinomial(link=logit),
#   #                 linear_terms = c("GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural"),
#   #                 quadratic_terms = c("FWI", "GPP12"),
#   #                 interaction_terms = c("PopDens*HDI"),
#   #select = TRUE
#   #  fixed_terms = c),
#   # 
#   # cropland_pure = list(landcover = "PureCropland",
#   #                       version_id = "v0.11.0_properGPP_3wayint",
#   #target = "BurntFraction",
#   #family = quasibinomial(link=logit),
#   #select = TRUE
#   #                      linear_terms = c("GPP", "GPP_index", "RoadDens", "Slope", "TPI", "GDP_capita", "Urban", "Natural"),
#   #                      quadratic_terms = c("FWI", "GPP12"),
#   #                      interaction_terms = c("PopDens*HDI"))
#   # 
#   #,
#   # pasture = list(landcover = "Pasture",
#   #                  version_id = "v0.11.0_properGPP_3wayint",
#   #target = "BurntFraction",
#   #family = quasibinomial(link=logit),
#   #select = TRUE
#   #                 linear_terms = c("GPP", "GPP_index", "RoadDens", "Slope", "GDP_capita", "Urban", "Natural", "PopDens", "HDI"),
#   #                 quadratic_terms = c( "FWI", "GPP12"),
#   #                 interaction_terms = c())
#   
#   
#   
# )


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




#### READ DATA.TABLE AND PREPARE ####
master_dt <- readRDS(file.path(data_dir, paste0("master_full_dt_", data_version, ".rds")))

# some data prep
master_dt[ , PopDens := sqrt(PopDens)]
master_dt[ , GDP_capita := sqrt(GDP_capita)]
master_dt[ , GPP_index := (GPP3/3)/maxGPP12]
master_dt[ , capGPP12 := cap_at_1000(GPP12)]
master_dt[ , log_GPP12 := log(capGPP12 + 1)]
master_dt[ , AGB_natural := log(AGB_natural + 1)]


#### MAIN LOOP FOR EACH MODEL ####

for(model_spec in model_specifications_list) {
  
  tic()
  
  # read the model specifications for later convenience
  dependent_var <-  model_spec$target
  analysis_version <- model_spec$version_id
  landcover <- model_spec$landcover
  linear_terms <- model_spec$linear_terms
  quadratic_terms <- model_spec$quadratic_terms
  interaction_terms <- model_spec$interaction_terms
  fixed_effect_terms <- model_spec$fixed_effect_terms
  random_effect_terms <- model_spec$random_effect_terms
  smooth_terms <- model_spec$smooth_terms
  
  this_dependent_var <- paste(dependent_var, landcover, sep = "_")
  
  
  # analysis directories and text info
  plot.dir <- file.path(top.plot.dir, this_dependent_var, analysis_version)
  dir.create(plot.dir, showWarnings = FALSE, recursive = TRUE)
  intermediates.dir <- file.path(top.intermediates.dir, this_dependent_var, analysis_version)
  dir.create(intermediates.dir, showWarnings = FALSE, recursive = TRUE)
  this_model_textfile <- file(file.path(plot.dir, "log.txt"), open = "wt")
  writeLines(paste("Input data version:", data_version), this_model_textfile)

  
  
 
  
  # get predictor vars for subsetting the data.table (splitting the interaction terms to their components where necessary)
  predictor_vars <- c(linear_terms, quadratic_terms, fixed_effect_terms, random_effect_terms, smooth_terms)
  if(!is.null(interaction_terms))  predictor_vars <- c(predictor_vars, unlist(strsplit(interaction_terms, split = "*", fixed = TRUE)) )
  
  # get continuous predictors for the correlation matrix plot (by excluding the fixed and random effects)
  continuous_predictor_vars <- predictor_vars[ !predictor_vars %in% c(fixed_effect_terms, random_effect_terms)] 
  
  
  # build the formula  
  this_formula_str <- paste(this_dependent_var, "~")
  if(length(linear_terms) > 0) this_formula_str <- paste(this_formula_str, paste(linear_terms, collapse =  " + "))
  if(length(quadratic_terms) > 0) this_formula_str <- paste(this_formula_str, paste(paste0("poly(", quadratic_terms, ",2)"), collapse =  " + "), sep = " + ")
  if(length(interaction_terms) > 0) this_formula_str <- paste(this_formula_str, paste(interaction_terms, collapse =  " + "), sep = " + ")
  if(length(fixed_effect_terms) > 0) this_formula_str <- paste(this_formula_str, paste(paste0(fixed_effect_terms), collapse =  " + "), sep = " + ")
  if(length(smooth_terms) > 0) this_formula_str <- paste(this_formula_str, paste(paste0("s(", smooth_terms, ", k=", k, ", bs='", basis_splines, "')"), collapse =  " + "), sep = " + ")
  writeLines(c("Model formula:", this_formula_str), this_model_textfile)
  # convert to formula
  this_formula <- as.formula(this_formula_str)
  
  
  # remove the previous GLM and clean up 
  suppressWarnings(rm(this_m))
  gc()
  
  head_text <- paste("******** Running land cover class:", landcover, "********")
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
    pearsons_corrplot <- {
      corrplot(corr = all_cor,
               method = "number",
               type = "upper",
               number.cex = 0.8)
      recordPlot()
    }
    magicPlot(p = pearsons_corrplot, filename = file.path(plot.dir, paste("PearsonsCorrelation", sep = "_")), height = 800, width =800)
    print(pearsons_corrplot)
    
    
    
    #### SUBSET THE DATA ####
    
    # select the columns we need for this particular model
    if("Year" %in% names(master_dt))  { 
      col_subset <- c("Lon", "Lat", "Year","Month",  this_dependent_var, paste("LandcoverFraction", landcover, sep = "_"), "FuelClass", predictor_vars)
    } else {
      col_subset <- c("Lon", "Lat", "Month",  this_dependent_var, paste("LandcoverFraction", landcover, sep = "_"), "FuelClass", predictor_vars)
    }
    
    # select columns (variables) and rows (only the fitting years) and require at least 0.1% of the area covered by the landcover type
    fitting_dt <- master_dt[ Year %in% fit_years & get(paste("LandcoverFraction", landcover, sep = "_")) > 0.01, ..col_subset]
    fitting_dt <- na.omit(fitting_dt)
    
    # clean some rich/highly urban gridcells
    if("GDP_capita" %in% predictor_vars)  fitting_dt <- fitting_dt[GDP_capita < sqrt(200000),]
    if("Urban" %in% predictor_vars) fitting_dt <- fitting_dt[Urban < 0.5,] 
    if(dependent_var == "BurntFraction") fitting_dt <- fitting_dt[ , c(this_dependent_var) := cap_at_1(.SD), .SDcols = this_dependent_var] 
    # if Natural
    if(landcover == "Natural") fitting_dt <- fitting_dt[ LandcoverFraction_Natural > 0.15, ] 
    
    
    
    # limit the predictor to 1.0 in case of very rare numerical artefacts
    if("Urban" %in% predictor_vars) fitting_dt <- fitting_dt[Urban < 0.5,] 
    
    
    # balance dataset
    if(FALSE) {
      # for gricells selection, average across years 
      fitting_dt_annual <-   fitting_dt[ ,  lapply(.SD, FUN=sum), by = c("Lon", "Lat")]
      
      
      fitting_dt_annual[ , Lon1 := floor(Lon)]
      fitting_dt_annual[ , Lat1 := floor(Lat)]
      set.seed(1234)
      
      burnt_dt <- fitting_dt_annual[ get(this_dependent_var) > 0, .(burnt = .N), by = c("Lon1", "Lat1")]
      unburnt_dt <- fitting_dt_annual[ get(this_dependent_var) == 0, .(unburnt = .N), by = c("Lon1", "Lat1")]
      
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
          this_burnt_dt <- this_dt[ get(this_dependent_var) > 0, c("Lon", "Lat")]
          selected_gridcells <- rbind(selected_gridcells, this_burnt_dt)
          # take a matching amount of unburnt as burnt, with a minimum of 15% gridcells (or failing that a single gricell)
          this_unburnt_dt <- this_dt[ get(this_dependent_var) == 0, c("Lon", "Lat")]
          num_unburnt_sample <- max(nrow(this_burnt_dt), ceiling(0.15 * nrow(this_dt)))
          this_unburnt_dt <- this_unburnt_dt[ , c("Lon", "Lat")][sample(x = 1:nrow(this_unburnt_dt), size = num_unburnt_sample, replace = FALSE),]
          selected_gridcells <- rbind(selected_gridcells, this_unburnt_dt)
        }
        
      }
      
      message(paste("Subsetting", nrow(selected_gridcells), "out of", nrow(fitting_dt_annual), "gridcells"))
      #fitting_dt <- fitting_dt[selected_gridcells]
    }
    
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
                 
      
      this_summary <- summary(this_m)
      this_deviance_explained <- 1 - (this_m$deviance/this_m$null.deviance)
      print(paste0(" ** Finished fit.  Deviance explained = ", signif(this_deviance_explained, 3) *100, "%"))
      
      
      # save the GLM and the data to the intermediates directory
      saveRDS(object = this_m, file = file.path(intermediates.dir, paste("GLM", analysis_version, "rds", sep = ".")))
      saveRDS(object = fitting_dt, file = file.path(intermediates.dir, paste("DT", analysis_version, "rds", sep = ".")))
      
    } else {
      print(" ** Reading GLM from disk")
      this_m <- readRDS(file = file.path(intermediates.dir, paste("GLM", analysis_version, "rds", sep = ".")))
      fitting_dt <- readRDS(file = file.path(intermediates.dir, paste("DT", analysis_version, "rds", sep = ".")))
      #predictor_vars <- names(this_m$model)
    }
    
  }
  
  
  if(exists("this_m")) {
    
    print(" ** Summarising model")
    this_summary <- summary(this_m)
    this_deviance_explained <- 1 - (this_m$deviance/this_m$null.deviance)
    print(this_summary)
    writeLines(capture.output(this_summary), con = this_model_textfile)
    
    # if only only linear terms, calculate the variable inflation factor (VIF)
    if(length(quadratic_terms) == 0 & length(interaction_terms) == 0){
      this_vif <- vif(this_m)
      print(this_vif)
    }
    
    # # subtitle of variance explained and number of data points
    subtitle_text <- paste0("Dev. expl. = ",  round(this_deviance_explained * 100, 1), "%")
    caption_text <- paste(this_dependent_var, analysis_version, paste0("Dev. expl. = ",  round(this_deviance_explained * 100, 1), "%"), sep = ", ")
    
    
    # Variable Importance through Permutation (VIP)
    # try catch
    
    
    
    ########################### PLOT PREDICTORS ##############################


    # all plots for maybe combining in different figures
    all_ggplot_list <- list()
    
    # generate a template data frame for prediction
    template_dt <- calculatePredictionDT(dt = master_dt, model = this_m, method = mean, npoints = 100) 
    
    # PLOT SINGLE TERMS
    for(this_var in c(linear_terms, quadratic_terms, smooth_terms)){
      all_ggplot_list[[this_var]] <- plotSimpleTerm (this_var, this_m, template_dt, range(master_dt[[this_var]], na.rm = TRUE))
      effect_plot <- all_ggplot_list[[this_var]] + theme(text = element_text(size = theme_get()$text$size * text.multiplier))#,
      effect_plot <- effect_plot + labs(caption = caption_text, y = paste("Burnt Fracttion [-] (other covariates fixed at mean)"))
      
      ### save it
      magicPlot(p = effect_plot, filename = file.path(plot.dir, paste("Predictor_Single", this_var, sep = "_")), height = 1300, width =1800)
    }
    
    
    # PLOT INTERACTION TERMS
    for(this_interaction in interaction_terms){
      
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
    
    
    

    ########################### PREDICTIONS IN TIME AND SPACE ##############################

    
    ### now predict 
    print(" ** Making prediction")
    # note the requirement add that the prediction is only done in gridcells where there is at least 1% of the landcover type present
    predicted_dt <- na.omit(copy(master_dt[get(paste("LandcoverFraction", landcover, sep = "_")) > 0.01,] ))
    predicted_dt[ , "Predicted_burnt_fraction" := predict(object = this_m, newdata = predicted_dt, type = "response")]
    setnames(predicted_dt, this_dependent_var, "Observed_burnt_fraction")
    print(" ** Done.")
    
    # calculate burnt area from burnt fraction, landcover fraction and gridcell area
    print(" ** Calculating gridecell area")
    predicted_dt <- addArea(predicted_dt, "ha", tolerance = 0.0000001)
    predicted_dt[, Landcover_area_ha :=  get(paste("LandcoverFraction", landcover, sep = "_")) * Area]
    predicted_dt[, Predicted_burnt_area_glm := Predicted_burnt_fraction * Landcover_area_ha]
    predicted_dt[, Observed_burnt_area := Observed_burnt_fraction * Landcover_area_ha]
    predicted_dt[, Predicted_burnt_fraction := Predicted_burnt_fraction * 100]
    predicted_dt[, Observed_burnt_fraction := Observed_burnt_fraction * 100]
    
    
    ### Remove small monthly burnt areas from the prediction    
    unscaled_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_glm"]])
    print(unscaled_NME)
    predicted_dt[, Predicted_burnt_area_threshold := fifelse(Predicted_burnt_area_glm > min_fire_size_ha, Predicted_burnt_area_glm, 0)]
    threshold_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_threshold"]])
    print(threshold_NME)
    
    # ### Scale the 
    if(apply_scaling) {
      
      # apply an exponent and then scale the burnt area
      observed_total_ba <- sum(predicted_dt[["Observed_burnt_area"]])
      min_NME <- 100
      best_exp <- NULL
      best_scaling <- NULL
      for(this_exponent in seq(0.1, 5, 0.1)) {
        
        tic()
        this_exp_predictions <- predicted_dt$Predicted_burnt_area_threshold^this_exponent
        this_scaling <- observed_total_ba / sum(this_exp_predictions)
        toc()
        
        
        this_final <- this_exp_predictions *  this_scaling
        print(sum(this_final))
        
        this_NME <-  calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = this_final)
        
        if(this_NME < min_NME) {
          min_NME <- this_NME
          best_exp <- this_exponent
          best_scaling <- this_scaling
        }
        print(min_NME)
        print(best_exp)
        print(best_scaling)
      }
      
      # predicted_dt[, Predicted_burnt_area_final := a * Predicted_burnt_area ^ b]
      # linear_scaled_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_final"]])
      # print(linear_scaled_NME)
      # 
      
      # select the fial predicted burnt area
      predicted_dt[, Predicted_burnt_area := best_scaling * Predicted_burnt_area_threshold^best_exp]
      
    } else {
      predicted_dt[, Predicted_burnt_area := Predicted_burnt_area_threshold]
    }
    
    
    #### SPATIAL PREDICTIONS ####
    
    # make an appropriate overlay
    all_lons <- sort(unique(fitting_dt[["Lon"]]))
    all_lats <- sort(unique(fitting_dt[["Lat"]]))
    plot_region <- c(xmin = min(all_lons), xmax = max(all_lons), ymin = min(all_lats), ymax = max(all_lats))
    this_overlay <- st_crop(overlay, plot_region)
    
    
    # monthly
    print(" ** Plotting monthly predictions")
    
    # if necessary average across years
    if("Year" %in% names(predicted_dt)){
      predicted_dt_meanyear <- predicted_dt[, lapply(.SD, FUN=mean, na.rm = TRUE), by = c("Lon","Lat", "Month"), .SDcols = c("Predicted_burnt_fraction", "Observed_burnt_fraction", "Predicted_burnt_area", "Observed_burnt_area")]
    } else {
      predicted_dt_meanyear <- copy(predicted_dt)
    }
    
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
        this_prediction_plot <- this_prediction_plot + coord_cartesian() + geom_sf(data=this_overlay, 
                                                                                   fill = "transparent", 
                                                                                   linewidth = 0.1,
                                                                                   colour= "black")
        magicPlot(p = this_prediction_plot, filename = file.path(plot.dir, paste("MonthlyPrediction",  paste0("Burnt", this_var$name),  month_labels[this_month], sep = "_")), height = 900, width =1300)
        
      }
      
    }
    
    # annual
    print(" ** Plotting annual predictions")
    
    this_prediction <- predicted_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat")] 
    this_prediction[, Month := NULL]
    
    setnames(this_prediction,  gsub(pattern = "_", replacement = " ", names(this_prediction), ))
    this_prediction <- melt(this_prediction, id.vars = c("Lon", "Lat"))
    
    spatial_NME <- calcNME(this_prediction[variable == "Observed burnt area", value], this_prediction[variable == "Predicted burnt area", value])
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
      magicPlot(p = this_prediction_plot, filename = file.path(plot.dir, paste("AnnualPrediction",  paste0("Burnt", this_var$name), sep = "_")), height = 900, width =1300)
      
    }
    
    #### SUBANNUAL PREDICTIONS ####
    print(" ** Plotting climatology prection")
    
    # sum burnt areas for each year 
    prediction_dt_subannual <- predicted_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
    prediction_dt_subannual <- melt(prediction_dt_subannual, id.vars = c("Month"), variable.name = "Type", value.name = "Burnt_area")
    this_prediction_ts_plot <- ggplot(prediction_dt_subannual) +  geom_line(data = prediction_dt_subannual, aes(x = Month, y = Burnt_area, col = Type)) + ylab("Burnt area (ha)")
    this_prediction_ts_plot <- this_prediction_ts_plot + scale_x_continuous(breaks = 1:12, labels = month_labels) + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
    #print(this_prediction_ts_plot)
    this_prediction_ts_plot <- this_prediction_ts_plot + labs(title = paste("Burnt Area in landcover:", landcover),
                                                              caption = caption_text)
    
    magicPlot(p = this_prediction_ts_plot, filename = file.path(plot.dir, paste("SeasonalPrediction", sep = "_")), height = 1000, width =1500)
    
    seasonal_NME <- calcNME(prediction_dt_subannual[Type == "Observed_burnt_area", Burnt_area], prediction_dt_subannual[Type == "Predicted_burnt_area", Burnt_area])
    print(paste("Final seasonal burnt area NME =",  round(seasonal_NME, 3)))
    
    
    #### ANNUAL TS (IAV) PREDICTIONS ####
    
    # only try if actually got yearly values
    if("Year" %in% names(predicted_dt)){
      
      #  sum all month in the years
      predicted_dt_yearly <- predicted_dt[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon","Lat","Year"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
      
      # sum across all gridcells melt
      predicted_dt_yearly <- predicted_dt_yearly[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
      
      # plot
      predicted_dt_yearly_melted <- melt(predicted_dt_yearly, id.vars = c("Year"), variable.name = "Type", value.name = "Burnt_area")
      
      this_prediction_yearly_plot <- ggplot(predicted_dt_yearly_melted) +  geom_line(data = predicted_dt_yearly_melted, aes(x = Year, y = Burnt_area, col = Type)) + ylab("Burnt area (ha)")
      this_prediction_yearly_plot <- this_prediction_yearly_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
      #print(this_prediction_ts_plot)
      this_prediction_yearly_plot <- this_prediction_yearly_plot + labs(title = paste("Burnt Area in landcover:", landcover),
                                                                        caption = caption_text)
      
      magicPlot(p = this_prediction_yearly_plot, filename = file.path(plot.dir, paste("YearlyTSPrediction", sep = "_")), height = 1000, width =1500)
      
      iav_NME <- calcNME(predicted_dt_yearly_melted[Type == "Observed_burnt_area", Burnt_area], predicted_dt_yearly_melted[Type == "Predicted_burnt_area", Burnt_area])
      print(paste("Final IAV burnt area NME =",  round(iav_NME, 3)))
      
      
    }
    
    ### END PREDICTION CODE
    
  } else {
    print(" ** No GLM fitted or found on disk")
  }
  
 
  full_timing <- capture.output(toc())
  writeLines(c("Total run time", full_timing , con = this_model_textfile))
  
  close(this_model_textfile)
  
} # for specified model
