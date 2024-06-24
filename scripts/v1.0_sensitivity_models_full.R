
final_v1.0_models <- function(){
  
  list(
    
    list(landcover = "NCV",
         description = "BASE v1.0",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "BASE v1.0",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE)
    
  )
  
}


all_ncv_sensitivity_models <- function(){
  
  list(
    
    #### SCALE PREDICTORS  ####
    list(landcover = "NCV",
         description = "BASE v1.0 with scaled predictors",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "Treecover","MEPI", "log_FWI" ),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit FAPAR12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Slope", "Pop_dens", "TPI", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit Slope",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12", "Pop_dens", "TPI", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "NCV",
         description = "Omit Pop_dens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit Treecover",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "Pop_dens", "TPI", "HDI"),
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
         linear_terms = c("FAPAR12","Slope", "Pop_dens", "HDI", "TPI", "log_FWI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "NCV",
         description = "Omit FWI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope",  "Pop_dens", "HDI", "TPI", "MEPI"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "Pop_dens", "TPI", "HDI"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope",  "Pop_dens", "HDI", "TPI", "Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "Pop_dens", "HDI", "MEPI", "TPI", "log_FWI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # MEPI quadratic
    list(landcover = "NCV",
         description = "MEPI quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "Pop_dens", "HDI", "TPI", "log_FWI"),
         quadratic_terms = c("Treecover", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # quadratic Pop_dens
    list(landcover = "NCV",
         description = "Pop dens quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "HDI"),
         quadratic_terms = c("Treecover", "Pop_dens"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "WindSpeed"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover", "WindSpeed"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "MaxWindSpeed"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover", "MaxWindSpeed"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "StdDevWindSpeed"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover", "StdDevWindSpeed"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    
    # Replace FAPAR12 with GPP12
    list(landcover = "NCV",
         description = "Replace FAPAR12 with GPP12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("GPP12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # Replace Pop_dens with Pop_dens_static
    list(landcover = "NCV",
         description = "Replace Pop_dens with Pop_dens_static",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens_static", "HDI"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "MEPI"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "LandcoverFraction_NCV"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # Include Cropland fraction
    list(landcover = "NCV",
         description = "Include crop fraction",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "LandcoverFraction_PureCropland"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    # Include crop ratio
    list(landcover = "NCV",
         description = "Include crop ratio",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "Crop_ratio"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "deltaNatural10"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    list(landcover = "NCV",
         description = "Include deltaNatural20",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "deltaNatural20"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "deltaNatural30"),
         quadratic_terms = c("Treecover"),
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
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI", "deltaNatural40"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Include deltaNatural10 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural10*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Include deltaNatural20 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural20*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Include deltaNatural30 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural30*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),


    list(landcover = "NCV",
         description = "Include deltaNatural40 interaction with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI", "deltaNatural40*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),



    # #### SOCIOECONOMIC VARIATIONS  ####
    list(landcover = "NCV",
         description = "Replace HDI with GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "GDP"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Replace HDI with GDP_capita_Wang",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "GDP_capita_Wang"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Replace HDI with GDP_gridcell_Wang",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "GDP_gridcell_Wang"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),


    list(landcover = "NCV",
         description = "Include HDI x Pop_dens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI",  "HDI*Pop_dens"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),


    list(landcover = "NCV",
         description = "Replace HDI with Pop_dens x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI",  "Pop_dens*GDP"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Replace HDI with Pop_dens x GDP_capita_Wang",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI",  "Pop_dens*GDP_capita_Wang"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Replace HDI with Pop_dens x GDP_gridcell_Wang",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI*log_FWI",  "Pop_dens*GDP_gridcell_Wang"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),

    list(landcover = "NCV",
         description = "Replace MEPI with MEPI2",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "TPI", "Pop_dens", "HDI"),
         quadratic_terms = c("Treecover"),
         interaction_terms = c("MEPI2*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE)
  )
  
}

# 
# all_cropland_sensitivity_models <- function(){
#   
#   list(
#     
#     #### BEST MODEL  ####
#     list(landcover = "PureCropland",
#          description = "BASE v1.0",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          do_shap = TRUE,
#          select = TRUE),
#     
#     #### ALL LINEAR  ####
#     list(landcover = "PureCropland",
#          description = "BASE v1.0 all linear",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "FWI", "GPP12", "MEPI", "PHI", "WindSpeed"),
#          quadratic_terms = c(),
#          interaction_terms = c(),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          do_shap = TRUE,
#          select = TRUE),
#     
#     
#     #### OMIT VARIABLES ####
#     list(landcover = "PureCropland",
#          description = "Omit Pop_dens",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Omit HDI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "Slope"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Omit Slope",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     
#     list(landcover = "PureCropland",
#          description = "Omit FWI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "WindSpeed"),
#          quadratic_terms = c("GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Omit GPP12",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "WindSpeed"),
#          quadratic_terms = c("FWI"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Omit MEPI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "PHI"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c(),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Omit PHI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "MEPI", "WindSpeed"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c(),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include TPI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "TPI", "WindSpeed"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Omit wind speed",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     list(landcover = "PureCropland",
#          description = "Wind speed not quadratic",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "WindSpeed"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     list(landcover = "PureCropland",
#          description = "Include max wind speed",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "MaxWindSpeed"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include max wind speed quadratic",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "MaxWindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include std dev wind speed",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "StdDevWindSpeed"),
#          quadratic_terms = c("FWI", "GPP12"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include std dev wind speed quadratic",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "StdDevWindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     # change functional forms
#     list(landcover = "PureCropland",
#          description = "MEPI PHI not interacting",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "MEPI", "PHI"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c(),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "FWI not quadratic",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "FWI"),
#          quadratic_terms = c("GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP12 not quadratic",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "GPP12"),
#          quadratic_terms = c("FWI", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     list(landcover = "PureCropland",
#          description = "Include max wind speed quadratic",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "MaxWindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Replace MEPI with GPP",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("GPP*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Replace GPP12 with FAPAR12",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "FAPAR12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Replace PHI with GPP6_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP6_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     #### TRY deltaNatural for land abandonment  ####
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural10",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "deltaNatural10"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural20",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "deltaNatural20"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural30",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "deltaNatural30"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural40",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope", "deltaNatural40"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural10 x HDI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI", "HDI*deltaNatural10"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural20 x HDI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI", "HDI*deltaNatural20"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural30 x HDI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI", "HDI*deltaNatural30"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include deltaNatural40 x HDI",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI", "HDI*deltaNatural40"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     #### SOCIOECONOMIC VARIATIONS ####
#     list(landcover = "PureCropland",
#          description = "Replace HDI with GDP",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "GDP", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include HDI x GDP",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI",  "HDI*GDP"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include HDI x Pop_dens",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI",  "HDI*Pop_dens"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "Include Pop_dens x GDP",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*PHI",  "Pop_dens*GDP"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     #### ALTERNATIVE GPP INDICES FOR HARVEST  ####
#     list(landcover = "PureCropland",
#          description = "GPP4_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP4_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP5_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP5_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP3_2_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP3_2_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP3_2_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP3_2_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP4_2_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP4_2_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP5_2_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP5_2_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     list(landcover = "PureCropland",
#          description = "GPP6_2_index",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI*GPP6_2_index"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE),
#     
#     
#     list(landcover = "PureCropland",
#          description = "Replace MEPI with MEPI2",
#          target = "BurntFraction",
#          family = quasibinomial(link=logit),
#          linear_terms = c("Pop_dens", "HDI", "Slope"),
#          quadratic_terms = c("FWI", "GPP12", "WindSpeed"),
#          interaction_terms = c("MEPI2*PHI"),  # Form "Var*Var2"
#          fixed_effect_terms = c(),
#          random_effect_terms = c(),
#          smooth_terms = c(),
#          select = TRUE)
#     
#     
#     
#   )
#   
# }



all_cropland_sensitivity_models<- function(){
  
  list(
  
    #### ALL LINEAR  ####
    list(landcover = "PureCropland",
         description = "BASE v1.0 all linear",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "FWI", "GPP12", "MEPI", "PHI", "WindSpeed"),
         quadratic_terms = c(),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         do_shap = TRUE,
         select = TRUE),
    
    
    #### OMIT VARIABLES ####
    list(landcover = "PureCropland",
         description = "Omit Pop_dens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("GDP", "Slope","PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "Slope","PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit Slope",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    
    list(landcover = "PureCropland",
         description = "Omit FWI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit GPP12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit MEPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit PHI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Omit wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "WindSpeed", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Include max wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "MaxWindSpeed", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include TPI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed", "TPI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include max wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MaxWindSpeed", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include std dev wind speed",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "StdDevWindSpeed", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include std dev wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "StdDevWindSpeed", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    # change functional forms
    list(landcover = "PureCropland",
         description = "MEPI PHI interacting",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c("MEPI*PHI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
     list(landcover = "PureCropland",
         description = "MEPI not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed", "MEPI"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "PHI quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "PHI", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "FWI not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "FWI", "PHI", "WindSpeed"),
         quadratic_terms = c("GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP12 not quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP12", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Include max wind speed quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MaxWindSpeed", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Replace MEPI with GPP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Replace MEPI with GPP quadratic",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "GPP"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Replace Pop_dens with Pop_dens_static",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens_static", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Replace GPP12 with FAPAR12",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "FAPAR12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Replace PHI with GPP6_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP6_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    #### TRY deltaNatural for land abandonment  ####
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural10",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "deltaNatural10", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural20",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "deltaNatural20","PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural30",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "deltaNatural30", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural40",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "deltaNatural40", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural10 x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "Slope", "WindSpeed", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c("GDP*deltaNatural10"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural20 x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "Slope", "WindSpeed", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c( "GDP*deltaNatural20"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural30 x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "Slope", "WindSpeed", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c( "GDP*deltaNatural30"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include deltaNatural40 x GDP",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "Slope", "WindSpeed", "PHI"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c( "GDP*deltaNatural40"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    #### SOCIOECONOMIC VARIATIONS ####
    list(landcover = "PureCropland",
         description = "Replace GDP with HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "HDI", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Include GDP x Pop_dens",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c("GDP*Pop_dens"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "Replace GDP with Pop_dens x HDI",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c( "Pop_dens*HDI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    #### ALTERNATIVE GPP INDICES FOR HARVEST  ####
    list(landcover = "PureCropland",
         description = "GPP4_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "WindSpeed", "GPP4_index"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP5_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope" , "GPP5_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP3_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP3_2_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP3_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP3_2_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP4_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP4_2_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP5_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP5_2_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    list(landcover = "PureCropland",
         description = "GPP6_2_index",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "GPP6_2_index", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE),
    
    
    list(landcover = "PureCropland",
         description = "Replace MEPI with MEPI2",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("Pop_dens", "GDP", "Slope", "PHI", "WindSpeed"),
         quadratic_terms = c("FWI", "GPP12", "MEPI2"),
         interaction_terms = c(),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE)
    
    
    
  )
  
}
