

all_ncv_sensitivity_models <- function(){
  
  list(
    
    # #### BEST MODEL  ####
    list(landcover = "NCV",
         description = "BASE v1.0",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE),
    
    
    #### SCALE PREDICTORS  ####
    list(landcover = "NCV",
         description = "BASE v1.0 with scaled predictors",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         scale_predictors = TRUE,
         select = TRUE),
    
    #### ALL SIMPLE LINEAR  ####
    list(landcover = "NCV",
         description = "BASE v1.0 all linear",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "Treecover_Gridcell","MEPI", "log_FWI" ),
         quadratic_terms = c(),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE),
    
    
    #### BEST MODEL FITTED FOR LPJml ####
    list(landcover = "NCV",
         description = "BASE v1.0 for LPJml",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI_LPJmL*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    #### OMIT VARIABLES ####
    
    list(landcover = "NCV",
         description = "Omit TPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit FAPAR12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Slope", "PopDens", "TPI", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit Slope",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12", "PopDens", "TPI", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "NCV",
         description = "Omit PopDens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit Treecover_Gridcell",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "PopDens", "TPI", "HDI"),
         quadratic_terms = c(),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    
    list(landcover = "NCV",
         description = "Omit MEPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "PopDens", "HDI", "TPI", "log_FWI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit FWI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope",  "PopDens", "HDI", "TPI", "MEPI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    #### MODIFY FUNCTIONAL FORMS ####
    
    # don't log FWI
    list(landcover = "NCV",
         description = "FWI not logged",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "PopDens", "TPI", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # don't uses polynomial tree cover
    list(landcover = "NCV",
         description = "Treecover not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope",  "PopDens", "HDI", "TPI", "Treecover_Gridcell"),
         quadratic_terms = c(),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE), 
    
    # MEPI and FWI non-interacting
    list(landcover = "NCV",
         description = "MEPI and FWI not interacting",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "PopDens", "HDI", "MEPI", "TPI", "log_FWI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # quadratic PopDens
    list(landcover = "NCV",
         description = "Pop dens quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "HDI"),
         quadratic_terms = c("Treecover_Gridcell", "PopDens"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    #### TRY NEW THINGS ####
    
    # include Wind
    list(landcover = "NCV",
         description = "Include wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "WindSpeed"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # include Wind quadratic
    list(landcover = "NCV",
         description = "Include wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell", "WindSpeed"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    # include max Wind
    list(landcover = "NCV",
         description = "Include max wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "MaxWindSpeed"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # include max Wind quadratic
    list(landcover = "NCV",
         description = "Include max wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell", "MaxWindSpeed"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # include std dev Wind
    list(landcover = "NCV",
         description = "Include std dev wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "StdDevWindSpeed"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # include std dev Wind quadratic
    list(landcover = "NCV",
         description = "Include std dev wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell", "StdDevWindSpeed"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    
    # swap FAPAR12 for GPP12
    list(landcover = "NCV",
         description = "Swap GPP12 for FAPAR12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("GPP12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # Interact HDI and FW
    list(landcover = "NCV",
         description = "Interact HDI and FWI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "MEPI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("HDI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # Include NCV fraction
    list(landcover = "NCV",
         description = "Include NCV fraction",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "LandcoverFraction_NCV"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # try deltaNatural for land abandonment
    
    list(landcover = "NCV",
         description = "Include deltaNatural10",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "deltaNatural10"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    list(landcover = "NCV",
         description = "Include deltaNatural20",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "deltaNatural20"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # 
    list(landcover = "NCV",
         description = "Include deltaNatural30",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "deltaNatural30"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # 
    list(landcover = "NCV",
         description = "Include deltaNatural40",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI", "deltaNatural40"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Include deltaNatural10 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural10*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Include deltaNatural20 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural20*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Include deltaNatural30 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural30*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "NCV",
         description = "Include deltaNatural40 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural40*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    
    
    #### SOCIOECONOMIC VARIATIONS  ####
    list(landcover = "NCV",
         description = "Swap GDP for HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "GDP_capita"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Include HDI x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI",  "HDI*GDP_capita"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Include HDI x PopDens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI",  "HDI*PopDens"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "NCV",
         description = "Include PopDens x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI",  "PopDens*GDP_capita"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Swap MEPI2 for MEPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "PopDens", "HDI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI2*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE)
  )
  
}


all_cropland_sensitivity_models <- function(){
  
  list(
    
    #### BEST MODEL  ####
    list(landcover = "PureCropland",
         description = "BASE v1.0",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE),
    
    #### ALL LINEAR  ####
    list(landcover = "PureCropland",
         description = "BASE v1.0 all linear",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "FWI", "GPP12", "MEPI", "GPP3_index", "WindSpeed"),
         quadratic_terms = c(),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE),
    
    
    #### OMIT VARIABLES ####
    list(landcover = "PureCropland",
         description = "Omit PopDens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "Slope"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit Slope",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    
    list(landcover = "PureCropland",
         description = "Omit FWI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "WindSpeed"),
         quadratic_terms = c("GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit GPP12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "WindSpeed"),
         quadratic_terms = c("FWI"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit MEPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "GPP3_index"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit GPP3_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "MEPI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include TPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "TPI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Wind speed not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Include max wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "MaxWindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include max wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "MaxWindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include std dev wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "StdDevWindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include std dev wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "StdDevWindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    # change functional forms
    list(landcover = "PureCropland",
         description = "MEPI GPP3_index not interacting",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "MEPI", "GPP3_index"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "FWI not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "FWI"),
         quadratic_terms = c("GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP12 not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "GPP12"),
         quadratic_terms = c("FWI", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Include max wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "MaxWindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Swap GPP for MEPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("GPP*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Swap GPP12 for FAPAR12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "FAPAR12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Swap GPP6_index for GPP3_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP6_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    #### TRY deltaNatural for land abandonment  ####
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural10",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "deltaNatural10"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural20",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "deltaNatural20"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural30",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "deltaNatural30"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural40",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope", "deltaNatural40"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural10 x HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index", "HDI*deltaNatural10"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural20 x HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index", "HDI*deltaNatural20"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural30 x HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index", "HDI*deltaNatural30"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural40 x HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index", "HDI*deltaNatural40"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    #### SOCIOECONOMIC VARIATIONS ####
    list(landcover = "PureCropland",
         description = "Swap GDP for HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "GDP_capita", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include HDI x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index",  "HDI*GDP_capita"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include HDI x PopDens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index",  "HDI*PopDens"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include PopDens x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_index",  "PopDens*GDP_capita"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    #### ALTERNATIVE GPP INDICES FOR HARVEST  ####
    list(landcover = "PureCropland",
         description = "GPP4_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP4_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP5_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP5_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP3_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_2_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP3_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP3_2_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP4_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP4_2_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP5_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP5_2_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP6_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI*GPP6_2_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Swap MEPI for MEPI2",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("PopDens", "HDI", "Slope"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
         interaction_terms = c("MEPI2*GPP3_index"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE)
    
    
    
  )
  
}

