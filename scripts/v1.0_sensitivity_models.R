
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
    
    
    #### TRY WIND THINGS ####
    
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
    
    ##### SOCIOECONOMIC VARIATIONS  ####
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
         select = TRUE)
  )
  
}


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
         select = TRUE)
  
  )
  
}
