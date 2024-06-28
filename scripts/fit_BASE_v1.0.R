#### LOAD PACKAGES AND DEFINE RELATIVE PATHS ####

if("DGVMTools" %in% (.packages())){  detach("package:DGVMTools", unload=TRUE) }
library(DGVMTools)

library(mgcv)
library(viridis)
library(tictoc)
library(rnaturalearth)
library(viridis)
library(car) # for variable inflation factor
library(fastshap)
library(vip)
library(ggcorrplot)
library(visreg)
library(hexbin)
library(ggpubr)
library(sf)


# define root path with here package and 
here::i_am("scripts/fit_BASE.R")
library(here)
source(here("scripts", "glm_fitting_helper_functions.R"))
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))

# input data directory containing large data.tables already constructed with script 
data_dir <- here("data_tables")


#### USER SETTINGS ####


# use at ma hlaf of Mr P
num_threads <- 48

# wether to plot the predictor terms (can sometimes be slow)
plot_terms <- TRUE

# main analysis version
analysis_version <- "BASE_v1.0"

# data to use
data_version <- "BASE_v1.0_publication"

# fit version
fit_batch_version <- "BASE_v1.0"

#
sig_figs <- 6

# 
min_landcover_frac <- 0.1

# re-fit
force_refit <- TRUE

# years for fitting
first_year_available <- 2002
last_year_available <- 2014
# take every even year
fit_years <- (first_year_available:last_year_available)[which(first_year_available:last_year_available %% 2 == 1)]
fit_years <-  first_year_available:(first_year_available+ceiling(length(first_year_available:last_year_available)/2))

# fraction of gridcells to use for fitting
fit_fraction <- 0.75

# number of iterations for bootstrapping null model
nbootstrap = 1000 

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

# lcc class colours
lcc_colours <- c("PureCropland" = "orchid4", "NCV" ="springgreen4")


#### SPECIFY MODELS TO FIT ####
source(here("scripts/v1.0_sensitivity_models.R"))
model_specifications_list <- c(final_v1.0_models(), all_ncv_sensitivity_models(), all_cropland_sensitivity_models())



#### DEFINE DIRECTORIES

# define paths for output 
top.output.dir <- here("fitted_models", analysis_version, "GLMs")
dir.create(top.output.dir, showWarnings = FALSE, recursive = TRUE)


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

text.multiplier <- 2.8

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
                  cuts =  c(0,0.5,1,2,5,10,20,50,100) ),
  Area = list(name = "Area", 
              title = "Burnt Area (ha)", 
              columns = c("Predicted_burnt_area","Observed_burnt_area"),
              pretty_columns = c("Predicted burnt area","Observed burnt area"),
              cuts = c(0,5,10,20,50,100,200,500,1000,2000,5000,10000))
)


overlay <- rnaturalearth::ne_countries(returnclass = "sf")
sf::sf_use_s2(FALSE)


#### READ DATA.TABLE AND PREPARE ####
tic()
master_dt <- readRDS(file.path(data_dir, analysis_version, paste0("master_full_dt_", data_version, ".rds")))
message("Read full master data.table")
toc()

# some data prep
master_dt[ , Pop_dens := sqrt(Pop_dens)]
master_dt[ , GDP := sqrt(GDP)]



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

# derive some indices
if("MEPI" %in% all_predictors) master_dt[ , MEPI := GPP/Max_GPP13]
if("MEPI2" %in% all_predictors) master_dt[ , MEPI2 := ((GPP+GPP1)/2) /Max_GPP13]
if("MEPI_LPJmL" %in% all_predictors) master_dt[ , MEPI_LPJmL := GPP_LPJmL/Max_GPP_LPJmL13]

if("PHI" %in% all_predictors) master_dt[ , PHI := (GPP3/3)/Max_GPP13]
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


# do transforms (log)
for(this_predictor in all_predictors){
  
  if(substr(this_predictor, 1, 4) == "log_") {
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
  target <-  model_spec$target
  description <- model_spec$description
  version_id <- gsub(" ", "_", description)
  landcover <- model_spec$landcover
  linear_terms <- model_spec$linear_terms
  quadratic_terms <- model_spec$quadratic_terms
  interaction_terms <- model_spec$interaction_terms
  fixed_effect_terms <- model_spec$fixed_effect_terms
  random_effect_terms <- model_spec$random_effect_terms
  smooth_terms <- model_spec$smooth_terms
  
  
  # define the target for the model fitting
  dependent_var <- paste(target, landcover, sep = "_")
  
  
  
  # analysis and plot directories and text info
  output.dir <- file.path(top.output.dir, dependent_var, fit_batch_version, version_id)
  dir.create(output.dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output.dir, "GLM"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output.dir, "Scaled"), showWarnings = FALSE, recursive = TRUE)
  summary_line_filename <- file.path(output.dir, "metrics.txt")
  summary_metrics[["Model"]] <- version_id
  summary_metrics[["Description"]] <- description
  
  # check that the model hasn't already been fitted 
  # - note that the summary file is the last thing to be written, so if it done its existence signifies a successfull model fit
  if(!force_refit & file.exists(summary_line_filename)) {
    message( paste("******** Not refitting model:", model_spec$description, "********"))
    
  }
  else {
    
    # start the log file
    this_model_textfile <- file(file.path(output.dir, "log.txt"), open = "wt")
    writeLines(paste("Input data version:", data_version), this_model_textfile)
    
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
    
    head_text <- paste("******** Running model:", model_spec$description, "********")
    message("")
    message(strrep("*", nchar(head_text)))
    message(head_text)
    message(strrep("*", nchar(head_text)))
    message("")
    
    
    
    
    # simply don't run if not enough data points
    if(nrow(master_dt) > MIN_DATA_POINTS) {
      
   
      #### SUBSET THE DATA ####
      
      # select the columns we need for this particular model
      col_subset <- c("Lon", "Lat", "Year","Month",  dependent_var, paste("LandcoverFraction", landcover, sep = "_"), predictor_vars)
     
      # in case some things (such as land cover fractions) appear twice
      col_subset <- unique(col_subset)
      
      # select columns (variables) and require at least 1% of the area covered by the landcover type
      # Not that this very loose area minimum requirement of 0.1% will likely be deepened later on, this just an initial winnowing
      valid_subset_dt <- na.omit(master_dt[get(paste("LandcoverFraction", landcover, sep = "_")) > 0.01, ..col_subset])
      
      #### FROM THIS POINT THE MASTER_DT IS NO LONGER USED FOR THIS MODEL FIT ####
      
   
      
      # if scale predictors desired
      if(isTRUE(model_spec$scale_predictors)) {
        
        scaled_predictors_matrix <- scale(valid_subset_dt[ , ..continuous_predictor_vars])
        for(this_pred in continuous_predictor_vars){
          valid_subset_dt[ , (this_pred) := scaled_predictors_matrix[ , this_pred]]
        }
        
      }
      
      
      # select only  a certain fraction of rows for fitting and the rest for testing
      num_fit <- as.integer(fit_fraction * nrow(valid_subset_dt))
      set.seed(5678)
      fitting_gridcells <- sample(nrow(valid_subset_dt), num_fit, replace = FALSE)
      training_dt <- valid_subset_dt[fitting_gridcells]
      testing_dt <- valid_subset_dt[-fitting_gridcells]
      
      
      # clean some rich/highly urban gridcells to give final 
      fitting_dt <- copy(training_dt)
      # changed so now fitting/training are identical
      
      # limit the predictor to 1.0 in case of very rare numerical artefacts
      if(target == "BurntFraction") fitting_dt <- fitting_dt[ , c(dependent_var) := cap_at_1(.SD), .SDcols = dependent_var]
      
      # only fit where the fraction of LCT of interest is above a certain threshold
      fitting_dt <- fitting_dt[ get(paste("LandcoverFraction", landcover, sep = "_"))  > min_landcover_frac, ] 
       
       
    
      
      #### CORRELATION PLOT ####  
      
      # make and save correlation plot
      all_cor <- cor(na.omit(valid_subset_dt[get(paste("LandcoverFraction", landcover, sep = "_"))  > min_landcover_frac,]), method = "pearson")
      pearsons_corrplot <- ggcorrplot(all_cor, hc.order = TRUE, type = "lower",
                                      outline.col = "white",
                                      ggtheme = ggplot2::theme_gray,
                                      colors = c("#6D9EC1", "white", "#E46726"),
                                      lab = TRUE,
                                      lab_size = 8,
                                      tl.cex = 24,
                                      show.legend = FALSE)
      magicPlot(p = pearsons_corrplot, filename = file.path(output.dir, paste("PearsonsCorrelation", sep = "_")), height = 800, width =800)
      #print(pearsons_corrplot)
      
      
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
      
      message(" ** Starting fit with formula:")
      print(this_formula_str)
      tic()
      
      
      # if got a smooth term then fit a GAM
      if(length(smooth_terms) > 0 ){
        this_m <-  mgcv::gam(this_formula,
                             data = fitting_dt,
                             method = "REML",
                             select = model_spec$select,
                             family = model_spec$family)
      }
      # else do a GLM
      # in my testing I found that fit a the same model with gam() and glm() gave identical results,
      # I think certain packages work with GLM objects but not GAM objects
      # therefore we fit with glm() where possible
      
      else  {
        this_m <-  glm(this_formula,
                             data = fitting_dt,
                             family = model_spec$family)
      }
      
      
      
      
      fit_timing <- capture.output(toc())
      writeLines(c("Total fit time", fit_timing) , con = this_model_textfile)
      print(fit_timing)
      
      print(" ** Summarising model")
      this_summary <- summary(this_m)
      print(this_summary)
      
      this_deviance_explained <- 1 - (this_m$deviance/this_m$null.deviance)
      summary_metrics[["Deviance explained"]] <- round(this_deviance_explained, sig_figs)
      
      
      print(paste0(" ** Finished fit.  Deviance explained = ", signif(this_deviance_explained, 3) *100, "%"))
      
      
      print(" ** Writing summary")
      writeLines(capture.output(print(this_summary)), con = this_model_textfile)
      
      print(" ** Writing coefficients table")
      library(broom)
      this_m_table <- tidy(this_m)
      print(this_m_table)
      write.table(this_m_table, file = file.path(output.dir, "coeffs_table.txt"), row.names = FALSE)
      
      print(" ** Calculating AIC and QAIC...")
      
      
      
      # Quasi AIC 
      # ML estimation
      # hacked.quasibinomial <- function(...) {
      #   res <- quasibinomial(...)
      #   res$aic <- binomial(...)$aic
      #   res
      # }
      # chat <- this_m$deviance / df.residual(this_m)
      # tic()
      # QAIC <- QAIC(update(this_m, family = hacked.quasibinomial), chat = chat)
      # print(paste("QAIC =", QAIC))
      # summary_metrics[["QAIC"]] <- round(QAIC, sig_figs)
      
      # Regular AIC (NA for quasi-models) 
      print(paste("AIC =", this_m$aic))
      summary_metrics[["AIC"]] <- round(this_m$aic, sig_figs)
      
      print("... done.")
      
      
      
      #### CALCULATE VIP BY SHAP ####
      print(" ** Calculating SHAP values...")
      if(isTRUE(model_spec$do_shap)) {
        tic()
        vi_by_shap <- vi(this_m, 
                         method = "shap",
                         train = fitting_dt,
                         pred_wrapper = predict)
        
        # make into an ordered factor for plotting (notice the reversing of factors so that it plots in the right order)
        shap_df <- data.frame(Predictor = factor(rev(vi_by_shap$Variable), rev(vi_by_shap$Variable), ordered = TRUE), Importance = rev(vi_by_shap$Importance))
        
        # Make the plot
        shap_plot <- ggplot(data = shap_df) 
        shap_plot <- shap_plot + geom_bar(aes(y = Importance, x= Predictor), stat = "identity", fill = lcc_colours[model_spec$landcover])
        shap_plot <- shap_plot + coord_flip() + theme_bw()
        shap_plot <- shap_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
        this_lcc_string <- model_spec$landcover
        if(this_lcc_string == "PureCropland" ) this_lcc_string <- "Cropland"
        shap_plot <- shap_plot + labs(y = "SHAP Importance", x = paste(this_lcc_string, "Predictor"))
        
        # print and save the plots and SHAP values
        print(shap_plot)
        magicPlot(p = shap_plot, filename = file.path(output.dir, paste("SHAP_Values")), height = 1200, width =1200)
        write.table(x = shap_df, file =  file.path(output.dir, paste("shap_values.txt")), row.names = FALSE)
        print(" ** Done.")
        toc()
        
      }
      
      
      
      ####  CALCULATE PREDICTED VALUES #### 
      
      
      print(" ** Making full prediction")
      
      # note the requirement add that the prediction is only done in gridcells where there is at least 1% of the landcover type present
      tic()
      predicted_training_dt <- copy(training_dt)[ , "Predicted_burnt_fraction" := predict(object = this_m, newdata = training_dt, type = "response")]
      predicted_testing_dt <- copy(testing_dt)[ , "Predicted_burnt_fraction" := predict(object = this_m, newdata = testing_dt, type = "response")]
      setnames(predicted_training_dt, dependent_var, "Observed_burnt_fraction")
      setnames(predicted_testing_dt, dependent_var, "Observed_burnt_fraction")
      toc()
      
      print(" ** Done.")
      
      
      # calculate burnt area from burnt fraction, landcover fraction and gridcell area (separately for the training and testing)
      
      print(" ** Calculating gridcell area and burnt area for _training_ data")
      tic()
      predicted_training_dt <- addArea(predicted_training_dt, "ha", tolerance = 0.0000001)
      predicted_training_dt[, Landcover_area_ha :=  get(paste("LandcoverFraction", landcover, sep = "_")) * Area]
      predicted_training_dt[, Predicted_burnt_area_raw := Predicted_burnt_fraction * Landcover_area_ha]
      predicted_training_dt[, Observed_burnt_area := Observed_burnt_fraction * Landcover_area_ha]
      toc()
      
      print(" ** Calculating gridcell area and burnt area for _testing_ data")
      tic()
      predicted_testing_dt <- addArea(predicted_testing_dt, "ha", tolerance = 0.0000001)
      predicted_testing_dt[, Landcover_area_ha :=  get(paste("LandcoverFraction", landcover, sep = "_")) * Area]
      predicted_testing_dt[, Predicted_burnt_area_raw := Predicted_burnt_fraction * Landcover_area_ha]
      predicted_testing_dt[, Observed_burnt_area := Observed_burnt_fraction * Landcover_area_ha]
      toc()
      
      
      print(" ** Done.")
      
      #### COMBINE DATASETS AND CALCULATE NMES ####
      
      training_NME <- calcNME(obs = predicted_training_dt[["Observed_burnt_area"]], mod = predicted_training_dt[["Predicted_burnt_area_raw"]])
      testing_NME <- calcNME(obs = predicted_testing_dt[["Observed_burnt_area"]], mod = predicted_testing_dt[["Predicted_burnt_area_raw"]])
      predicted_dt <- rbind(predicted_training_dt, predicted_testing_dt)
      rm(predicted_training_dt, predicted_testing_dt); gc()
      unscaled_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_raw"]])
      
      
      
      # bootstrap null method
      summary_metrics[["Full NME"]] <- round(unscaled_NME, sig_figs)
      summary_metrics[["Training NME"]] <- round(training_NME, sig_figs)
      summary_metrics[["Testing NME"]] <- round(testing_NME, sig_figs)
      
      
      #### OPTIONAL: APPLY THRESHOLD AND SCALING ####
      if(apply_scaling) {
        
        
        ### Remove small monthly burnt areas from the prediction    
        print(" ** Applying threshold")
        summary_metrics[["Raw full NME"]] <- round(unscaled_NME, sig_figs)
        predicted_dt[, Predicted_burnt_area_threshold := fifelse(Predicted_burnt_area_raw > min_fire_size_ha, Predicted_burnt_area_raw, 0)]
        threshold_NME <- calcNME(obs = predicted_dt[["Observed_burnt_area"]], mod = predicted_dt[["Predicted_burnt_area_threshold"]])
        
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
        
        
        summary_metrics[["Scaled full NME"]] <- round(min_NME, sig_figs)
        summary_metrics[["Scaling Factor"]] <- round(best_scaling, sig_figs)
        summary_metrics[["Exponent"]] <- round(best_exp, sig_figs)
        
        # select the fial predicted burnt area
        predicted_dt[, Predicted_burnt_area_scaled := best_scaling * Predicted_burnt_area_threshold^best_exp]
        subset_to_save <- predicted_dt[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw", "Predicted_burnt_area_scaled", "Observed_burnt_area", "Predicted_burnt_fraction", "Observed_burnt_fraction")]
        
      } else {
        subset_to_save <- predicted_dt[ , c("Lon", "Lat", "Year", "Month", "Predicted_burnt_area_raw", "Observed_burnt_area", "Predicted_burnt_fraction", "Observed_burnt_fraction")]
      }
      
      
      # save the GLM and the data to the output directory
      print(" ** Saving model")
      tic()
      saveRDS(object = this_m, file = file.path(output.dir, paste("GLM", version_id, "rds", sep = ".")))
      toc()
      print(" ** Saving results data.frame")
      tic()
      saveRDS(object = predicted_dt, file = file.path(output.dir, paste("DT", version_id, "rds", sep = ".")))
      toc()
      
      
      
      #### VARAIBLE INFLATION FACTOR ####   
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
      caption_text <- paste(dependent_var, description, paste0("Dev. expl. = ",  round(this_deviance_explained * 100, 1), "%"), sep = ", ")
      
      
      # Variable Importance through Permutation (VIP)
      # try catch
      
      
      
      ########################### PLOT PREDICTORS ##############################
      print(" ** Plotting predictors")
      
      
      # derive the visreg object for more plotting effects
      print("  ***** Making a visreg object for plotting.")
      tic()
      this_visreg_list <- visreg(this_m, plot = FALSE) 
      print("  ***** Finished making visreg object.")
      toc()
      
      
      # all plots for combining in different figures
      all_single_ggplots_list <- list()
      all_interaction_ggplots_list <- list()
      
      
      # # generate a template data frame for prediction
      # print("  ***** Calculating fixed predictor values.")
      # tic()
      # template_dt <- calculatePredictionDT(model_object = this_m, npoints = 100) 
      # print("  ***** Finished calculating fixed predictor values.")
      # toc()
      
      
      
      
      
      # PLOT SINGLE TERMS
      all_predictors <- listContinuousPredictors(this_m)
      for(this_var in all_predictors){
        print(paste0("   **** Plotting simple predictor:", this_var))
        
        # get the visreg from the list and make the plot
        this_visreg <- getVisRegFromList(this_visreg_list, this_var) 
        effect_plot <- plotSimpleTermVisReg(this_visreg, this_var, model = this_m)
        effect_plot <- effect_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
        bar.height.unit <- unit(0.7, units = "npc")
        effect_plot <- effect_plot + guides(fill = guide_colorbar(barwidth = 2, barheight = bar.height.unit))
        
        # add an asterisk if the single variable actually has an interaction too   
        variables_with_interactions <- NULL
        if(length(interaction_terms) > 0) variables_with_interactions <- unique(unlist(strsplit(x =interaction_terms, split = "*", fixed = TRUE)))
        if(this_var %in% variables_with_interactions) {        
          
          effect_plot <- effect_plot + annotate("text",
                                                x = min(this_visreg$fit[[this_var]]) + 0.9 * diff(range(this_visreg$fit[[this_var]])), 
                                                y = min(this_visreg$res[["visregRes"]], na.rm = TRUE) + 0.9 * diff(range(this_visreg$res[["visregRes"]], na.rm = TRUE)),
                                                label = "+",
                                                size = theme_get()$text$size * text.multiplier)
        }
        
        # and also save it for combining with others 
        all_single_ggplots_list[[this_var]] <- effect_plot
        
        # tweak and save individually
        magicPlot(p = effect_plot, filename = file.path(output.dir, paste("Predictor_Single_scaled", this_var, sep = "_")), height = 1300, width =1800)
        
        
        
      }
      
      
      # PLOT INTERACTION TERMS
      for(this_interaction in interaction_terms){
        
        print(paste0("   **** Plotting interation:", this_interaction))
        
        # determine the variables
        these_vars <-  unlist(strsplit(x =this_interaction, split = "*", fixed = TRUE))
        
        
        if(length(these_vars) == 2) {
          this_visreg_2d <- visreg2d(this_m, these_vars[1], these_vars[2], nn = 201, plot = FALSE)
          
          interaction_plot <- plotInteractionTermVisReg(this_visreg_2d, vars = these_vars, model = this_m)
          interaction_plot <- interaction_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
          bar.height.unit <- unit(0.7, units = "npc")
          interaction_plot <- interaction_plot + guides(fill = guide_colorbar(barwidth = 2, barheight = bar.height.unit))
          
          interaction_plot <- interaction_plot+ scale_fill_viridis(name = paste0("f(", these_vars[1], ",", these_vars[2], ")" ))
          all_interaction_ggplots_list[[this_interaction]] <- interaction_plot
          interaction_plot <- interaction_plot + labs(caption = caption_text)
          magicPlot(p = interaction_plot, filename = file.path(output.dir, paste("Predictor_Interaction_scaled", paste0(these_vars[1], "x", these_vars[2]), sep = "_")), height = 1300, width =1800)
        }
        
      }
      
      #### ARRANGE THE PREDICTOR PLOTS ####
      if(length(all_single_ggplots_list) > 0) {
        all_single_predictors_plot <- ggarrange(plotlist = all_single_ggplots_list, common.legend = TRUE, legend = "right")
        magicPlot(p = all_single_predictors_plot, filename = file.path(output.dir, "All_Single_Predictors"), height = 1400, width = 1600)
        pdf(file = file.path(output.dir, "All_Single_Predictors.pdf"), width = 16, height = 14)
        print(all_single_predictors_plot)
        dev.off()
      }
      if(length(all_interaction_ggplots_list) > 0) {
        all_interactions_plot <- ggarrange(plotlist = all_interaction_ggplots_list, common.legend = TRUE, legend = "right")
        magicPlot(p = all_interactions_plot, filename = file.path(output.dir, "All_Interacting_Predictors"), height = 1400, width = 1600)
        pdf(file = file.path(output.dir, "All_Interacting_Predictors.pdf"), width = 16, height = 14)
        print(all_interactions_plot)
        dev.off()
      }
      
      
      
      
      ############ PREDICTIONS IN TIME AND SPACE ################
      
      # convert fraction to percentage for more even plotting
      predicted_dt[, Predicted_burnt_fraction := Predicted_burnt_fraction * 100]
      predicted_dt[, Observed_burnt_fraction := Observed_burnt_fraction * 100]
      
      
      
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
                magicPlot(p = this_prediction_plot, filename = file.path(output.dir, scaled_or_unscaled, paste("MonthlyPrediction",  paste0("Burnt", this_var$name),  paste(this_month, month_labels[this_month], sep = "-"), sep = "_")), height = 900, width =1300)
              })
            }
          }
          
          # annual
          print(" ** Plotting annual predictions")
          
          # calculate maps
          this_prediction <- predicted_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon", "Lat")] 
          this_prediction[, Month := NULL]
          
          # calculate stats
          spatial_stats <- continuousComparison(x = this_prediction, 
                                                layers1 = "Predicted_burnt_area", 
                                                layers2 = "Observed_burnt_area", 
                                                area = TRUE, additional = NULL)
          summary_metrics[[paste(scaled_or_unscaled, "Spatial NME")]] <- round(spatial_stats$NME, sig_figs)
          summary_metrics[[paste("Null", scaled_or_unscaled, "Spatial NME")]] <- round(spatial_stats$`null NME`, sig_figs)
          print(paste("Final spatial burnt area NME =",  round(spatial_stats$NME, sig_figs)))
          
          
          
          setnames(this_prediction,  gsub(pattern = "_", replacement = " ", names(this_prediction)))
          this_prediction <- melt(this_prediction, id.vars = c("Lon", "Lat"))
          
          
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
            magicPlot(p = this_prediction_plot, filename = file.path(output.dir, scaled_or_unscaled, paste("AnnualPrediction",  paste0("Burnt", this_var$name), sep = "_")), height = 900, width =1300)
            
          }
          
          #### SUBANNUAL PREDICTIONS ####
          print(" ** Plotting climatology prediction")
          
          # calculate the seasonal stats before summing and melting
          tic()
          seasonal_stats <- doSeasonalStats(predicted_dt_meanyear, layers1 = "Predicted_burnt_area", layers2 = "Observed_burnt_area")
          summary_metrics[[paste(scaled_or_unscaled, "Null MPD")]] <- round(seasonal_stats$null_MPD, sig_figs)
          summary_metrics[[paste(scaled_or_unscaled, "MPD")]] <- round(seasonal_stats$MPD, sig_figs)
          summary_metrics[[paste(scaled_or_unscaled, "NULL NME_conc")]] <- round(seasonal_stats$null_NME_con, sig_figs)
          summary_metrics[[paste(scaled_or_unscaled, "NME_conc")]] <- round(seasonal_stats$NME_conc, sig_figs)
          toc()
          
          tic()
          seasonal_stats <- doSeasonalStats(predicted_dt, layers1 = "Predicted_burnt_area_raw", layers2 = "Observed_burnt_area")
          summary_metrics[[paste(scaled_or_unscaled, "Full Null MPD")]] <- round(seasonal_stats$null_MPD, sig_figs)
          summary_metrics[[paste(scaled_or_unscaled, "Full MPD")]] <- round(seasonal_stats$MPD, sig_figs)
          summary_metrics[[paste(scaled_or_unscaled, "Full NULL NME_conc")]] <- round(seasonal_stats$null_NME_con, sig_figs)
          summary_metrics[[paste(scaled_or_unscaled, "Full NME_conc")]] <- round(seasonal_stats$NME_conc, sig_figs)
          toc()
          
          
          
          # sum burnt areas across whole domain
          prediction_dt_subannual <- predicted_dt_meanyear[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Month"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
          prediction_dt_subannual <- melt(prediction_dt_subannual, id.vars = c("Month"), variable.name = "Type", value.name = "Burnt_area")
          this_prediction_ts_plot <- ggplot(prediction_dt_subannual) +  geom_line(data = prediction_dt_subannual, aes(x = Month, y = Burnt_area, col = Type, linewidth = Type)) + ylab("Burnt area (ha)")
          this_prediction_ts_plot <- this_prediction_ts_plot + scale_x_continuous(breaks = 1:12, labels = month_labels) + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
          this_prediction_ts_plot <- this_prediction_ts_plot + labs(title = paste("Burnt Area in landcover:", landcover),
                                                                    caption = caption_text,
                                                                    y = "Burnt area (ha)")
          
          this_prediction_ts_plot <- this_prediction_ts_plot + scale_color_viridis_d(labels = c("FireCCI51", "BASE"), option = "viridis")
          this_prediction_ts_plot <- this_prediction_ts_plot + scale_linewidth_manual(values = c(2,2), labels = c("FireCCI51", "BASE"))
          this_prediction_ts_plot <- this_prediction_ts_plot + scale_linetype_manual(values = c("solid", "solid"), labels = c("FireCCI51", "BASE"))
          
          
          magicPlot(p = this_prediction_ts_plot, filename = file.path(output.dir, scaled_or_unscaled, paste("SeasonalPrediction", sep = "_")), height = 1000, width =1500)
          
          
          
          
          #### ANNUAL TS (IAV) PREDICTIONS ####
          
          #  sum all month in the years, then sum across all gridcells
          predicted_dt_yearly <- predicted_dt[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Lon","Lat","Year"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
          predicted_dt_yearly <- predicted_dt_yearly[ , lapply(.SD, FUN=sum, na.rm = TRUE), by = c("Year"), .SDcols = c("Observed_burnt_area", "Predicted_burnt_area")]
          
          # calculate stats
          iav_stats <- continuousComparison(x = predicted_dt_yearly, 
                                            layers1 = "Predicted_burnt_area", 
                                            layers2 = "Observed_burnt_area", 
                                            area = TRUE, additional = NULL)
          summary_metrics[[paste(scaled_or_unscaled, "IAV NME")]] <- round(iav_stats$NME, sig_figs)
          summary_metrics[[paste("Null", scaled_or_unscaled, "IAV NME")]] <- round(iav_stats$`null NME`, sig_figs)
          print(paste("Final IAV burnt area NME =",  round(iav_stats$NME, sig_figs)))
          
          
          
          # plot
          predicted_dt_yearly_melted <- melt(predicted_dt_yearly, id.vars = c("Year"), variable.name = "Type", value.name = "Burnt_area")
          iav_plot <- IAVPlot(predicted_dt_yearly, #linewidths = c(2,2), linetypes = c("solid", "solid"),
                              text.multiplier = text.multiplier,
                              filename = file.path(output.dir, scaled_or_unscaled, paste("YearlyTSPrediction", sep = "_")), width = 1400, height = 900)
          iav_plot <- iav_plot + scale_color_viridis_d(labels = c("FireCCI51", "BASE"), option = "viridis")
          iav_plot <- iav_plot + scale_linewidth_manual(values = c(2,2), labels = c("FireCCI51", "BASE"))
          iav_plot <- iav_plot + scale_linetype_manual(values = c("solid", "solid"), labels = c("FireCCI51", "BASE"))
          iav_plot <- iav_plot + geom_smooth(method=lm) 
          iav_plot <- iav_plot + geom_line() 
          iav_plot <- iav_plot + labs(title = paste("Burnt Area in landcover:", landcover),
                                      caption = caption_text,
                                      y = "Burnt area (ha)")
          magicPlot(p = iav_plot, filename = file.path(output.dir, scaled_or_unscaled, paste("YearlyTSPrediction", sep = "_")), width = 1400, height =900)
          
          
          iav_NME <- calcNME(predicted_dt_yearly, predicted_dt_yearly_melted[Type == "Predicted_burnt_area", Burnt_area])
          
          
          
        } # catch if no scaling applied
        
      } # for scaled and unscaled
      
      ### END PREDICTION CODE
      
      
      full_timing <- capture.output(toc())
      writeLines(c("Total run time:", full_timing) , con = this_model_textfile)
      close(this_model_textfile)
      
      write.csv(x = as.data.table(summary_metrics), file = summary_line_filename, row.names = FALSE)
      
    } # if enough data points
    
  } # check in fitting is really necessary
  
} # for specified model
