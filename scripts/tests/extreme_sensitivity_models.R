

all_ncv_sensitivity_models <- function(){
  
  list(
    
    #### BEST MODEL  ####
    list(landcover = "NCV",
         description = "BASE v1.0",
         target = "BurntFraction",
         family = quasibinomial(link=logit),
         linear_terms = c("FAPAR12","Slope", "PopDens", "HDI", "TPI"),
         quadratic_terms = c("Treecover_Gridcell"),
         interaction_terms = c("MEPI*log_FWI"),  # Form "Var*Var2"
         fixed_effect_terms = c(),
         random_effect_terms = c(),
         smooth_terms = c(),
         select = TRUE)
  )
   
}