
cap_at_1000 <- function(vect) {  pmin(vect, 1000)  }
cap_at_1 <- function(vect) {  pmin(vect, 1.0)  }




### Ripped from DGVMTools (not currently exported by package)

#' Calculate Normalised Mean Error
#' 
#' Calculates NME between two datasets (represented as two equally sized numeric vectors) 
#' 
#' @param obs A numeric vector of observed values
#' @param mod A numeric vector of modelled values (same size as obs)
#' @param area A numeric vector of the areas by which to weight the values (same size as obs) 
#' 
#' @details  No check currently done on vector lengths
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A numeric
calcNME <- function(mod, obs, area) {
  
  if(missing(area) || is.null(area))   return( sum(abs(mod - obs), na.rm=TRUE) / sum(abs(obs - mean(obs)), na.rm=TRUE)) 
  else {
    return( sum(abs(mod - obs) * area, na.rm=TRUE) / sum(abs(obs - mean(obs)) * area, na.rm=TRUE) ) 
  }
}



#' Calculate Normalised Mean Square Error
#' 
#' Calculates NMSE between two datasets (represented as two equally sized numeric vectors) 
#' 
#' @param mod A numeric vector of observed values
#' @param obs A numeric vector of modelled values (same size as mod)
#' @param area A numeric vector of the areas by which to weight the values (same size as obs) 
#' 
#' @details  No check currently done on vector lengths
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A numeric
calcNMSE <- function(mod, obs, area) {
  
  if(missing(area) || is.null(area)) return( sum((mod - obs)^2, na.rm=TRUE) / sum((obs - mean(obs))^2 , na.rm=TRUE) ) 
  else return( sum((mod - obs)^2  * area, na.rm=TRUE) / sum((obs - mean(obs))^2 * area, na.rm=TRUE)) 
  
}


listContinuousPredictors <- function(x){
  
  # determine list of predictor variable involved
  tic()
  all_vars <- as.character(attr(x$terms, "variables"))[3:length(as.character(attr(x$terms, "variables")))] 
  print("INVESTIGATION.  Got all_vars")
  toc()
  
  # identify fixed terms so we can avoid adding them (since we don't wanna add column of NAs)
  tic()
  all_factors <- names(x$var.summary[sapply(x$var.summary, function(x) class(x) == "factor")])
  print("INVESTIGATION.  Got all_factors")
  toc()
  
  
  # extract polynomial and splines
  tic()
  for(this_var in all_vars) {
    
    # polynomials
    if(substr(this_var, 1, 5) == "poly(") {
      first_string <- strsplit(this_var, ",")[[1]][1]
      new_var <- gsub(pattern = "poly(", replacement = "", x = first_string, fixed = TRUE)
      all_vars <- replace(x = all_vars, all_vars == this_var, new_var)
    }
    
    # splines
    if(substr(this_var, 1, 2) == "s(") {
      first_string <- strsplit(this_var, ",")[[1]][1]
      new_var <- gsub(pattern = "s(", replacement = "", x = first_string, fixed = TRUE)
      all_vars <- replace(x = all_vars, all_vars == this_var, new_var)
    }
    
  }
  print("INVESTIGATION.  Got final list.")
  toc()
  
  
  return(all_vars[!all_vars %in% all_factors])
  
  
}

getFactorPredictorsFromModelObject <- function(model_object){
  return(names(model_object$var.summary[sapply(model_object$var.summary, function(x) class(x) == "factor")]))
}




calculatePredictionDT <- function(dt, model_object, agg.method, npoints = 100) {
  
  # get the non-factor predictors (because these are the ones we want to calculate the mean and subset)
  print("getting continuous predictors")
  tic()
  all_continuous_predictors <- listContinuousPredictors(model_object)
  print("... got em")
  toc()
  
  
  # apply the method and make repeated values repeat 
  tic()
  if(missing(agg.method) || missing(dt)) {
    print("Using pre-calculated median values")
    line_dt <- as.data.table(data.frame(model_object$var.summary)[2,])
  }
  else if(is.character(agg.method)){
    if(agg.method == "mean") {
      print("Doing mean")
      line_dt <- dt[ , lapply(.SD, mean, na.rm=TRUE), .SDcols = all_continuous_predictors]
    }
    if(agg.method == "median") {
      print("Doing median")
      line_dt <- dt[ , lapply(.SD, median, na.rm=TRUE), .SDcols = all_continuous_predictors]
    }
  }
  else {
    print("Doing custom")
    line_dt <- dt[ , lapply(.SD, agg.method, na.rm=TRUE), .SDcols = all_continuous_predictors]
  }
  print("INVESTIGATION: Made prediction DT")
  toc()
  
  return(line_dt[rep(1, npoints)])
  
}

getInteractionTerms <- function(model_object){
  
  all_terms <- strsplit(as.character(model_object$terms)[[3]],split = " ", fixed = TRUE)
  interaction_terms <- unlist(all_terms)[grepl(":", unlist(all_terms))]
  
  interaction_pairs <- list()
  for(this_interaction in interaction_terms) {
    interaction_pairs[[this_interaction]] <- unlist(strsplit(this_interaction, split = ":"))
  }
  return((interaction_pairs))
}


plotSimpleTermMultipleModels <- function(var, dts, models, names){
  
  if(length(dts) != length(models)) stop()
  
  
  for_plotting_dt <- data.table()
  for(index in 1:length(dts)) {
    this_name <- names[index]
    this_prediction_dt <- predictForPlotting(var, dts[[index]], models[[index]])
    this_prediction_dt[ , Model := this_name]
    for_plotting_dt <- rbind(for_plotting_dt, this_prediction_dt, fill = TRUE)
  }
  
  # make the plot
  for_plotting_dt <- ggplot(data = for_plotting_dt, aes(x = .data[[var]], y = response)) + geom_line(aes(col = Model)) 
  for_plotting_dt <- for_plotting_dt + geom_ribbon(aes(ymin = se_lower, ymax = se_upper, fill = Model), alpha = 0.25)
  
}


predictForPlotting <- function(var, dt, model){
  
  
  # generate data for the linear term and predict
  this_dt <- copy(dt)
  this_var_summary <- model$var.summary[[var]]
  this_dt[ , c(var) := seq(this_var_summary[1], this_var_summary[3], length.out = nrow(dt))]
  
  # IDENTIFY FACTOR IF THERE IS ONE
  fixed_effect <- getFactorPredictorsFromModelObject(model)
  if(length(fixed_effect) > 1) stop("Can only plot data with a single factor (i.e. only one fixed effect)")
  
  # if necessary repeat for each factor present 
  if(length(fixed_effect) > 0) {
    for_plotting <- data.table()
    for(this_level in model$xlevels[[1]]){
      this_level_dt <- copy(this_dt)
      set(this_level_dt, j = fixed_effect, value = factor(this_level, levels = model$xlevels[[1]]))
      for_plotting <- rbind(for_plotting, this_level_dt)
    }
  }
  else{
    for_plotting <- this_dt
  }
  
  # now predict with standard errors
  for_plotting[ , c("fit_link", "se_link")  := predict(model, for_plotting, se.fit = TRUE)]
  for_plotting[ , response :=  model$family$linkinv(fit_link) ]
  for_plotting[ , se_upper :=  model$family$linkinv(fit_link + (1.96 * se_link)) ]
  for_plotting[ , se_lower :=  model$family$linkinv(fit_link - (1.96 *se_link)) ]
  
  
}



plotSimpleTerm <- function(var, model, dt, full_dt){
  
  print(paste0(" **** Plotting simple (non-interacting) term: ", var))
  
  # calculating the data for plotting
  for_plotting <- predictForPlotting(var, dt, model)
  
  # check for a fixed effect
  fixed_effect <- getFactorPredictorsFromModelObject(model)  
  if(length(fixed_effect) > 1) stop("Can only plot data with a single factor (i.e. only one fixed effect)")
  
  # first plot the partial residuals as a  heatmap as a background
  subset_cols <- c("Predicted_burnt_fraction", "Observed_burnt_fraction", var)
  full_plotting_dt <- full_dt[ , ..subset_cols]
  full_plotting_dt <- full_dt[ , Partial_residual := Observed_burnt_fraction - Predicted_burnt_fraction + this_m$coefficients[["HDI"]] * HDI]
  print(full_plotting_dt)
  
  this_plot <- ggplot() + stat_bin_2d(data = full_plotting_dt, mapping = aes(x = .data[[var]], y = Partial_residual))
  this_plot <- this_plot + scale_fill_viridis(option = "F", direction = -1, trans = "log10")
  
  
  # if fixed effect
  if(length(fixed_effect) > 0) {
    this_plot <- this_plot + geom_line(data = for_plotting, aes(x = .data[[var]], y = response, col = .data[[fixed_effect]]))
    this_plot <- this_plot + geom_ribbon(data = for_plotting, aes(ymin = se_lower, ymax = se_upper, fill = .data[[fixed_effect]]), alpha = 0.5)
    this_plot <- this_plot + scale_fill_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect)) + scale_color_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect))
  }
  else {
    this_plot <- this_plot + geom_line(data = for_plotting, aes(x = .data[[var]], y = response), col = "darkturquoise")
    this_plot <- this_plot + geom_ribbon(data = for_plotting, aes(x = .data[[var]], ymin = se_lower, ymax = se_upper), col = "darkturquoise", alpha = 0.5)
    this_plot <- this_plot + geom_line(data = for_plotting, aes(x = .data[[var]], y = response), col = "darkturquoise")
    
    #this_plot <- this_plot + scale_fill_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect)) + scale_color_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect))
  }
  
  
  this_plot <- this_plot + scale_y_log10()
  
  return(this_plot)
  
}

getVisRegFromList <- function(x, var){
  
  # find this index in the visreg object
  this_index <- NULL
  for(i in 1:length(x)) {
     if(x[[i]]$meta$x == var) {
      this_index <- i
      break
    }
  }
  
  if(is.null(this_index)) fail(paste("Couldn't find variable", var))
  return(x[[this_index]])
  
}



plotSimpleTermVisReg <- function(this_visreg, var, model, interaction_terms = NULL){
  
  print(paste0(" **** Plotting simple (non-interacting) term from VisReg object: ", var))
  

  # plot the background as a heatmap 
  this_plot <- ggplot() + geom_hex(data = this_visreg$res, mapping = aes(x = .data[[this_visreg$meta$x]], y = visregRes), bins = 50)
  this_plot <- this_plot + scale_fill_viridis(option = "F", direction = -1, tran = "log10")
  
  # check for a fixed effect
  fixed_effect <- getFactorPredictorsFromModelObject(model)  
  if(length(fixed_effect) > 0) stop("This function can't currently plot fixed effects")
  
  this_plot <- this_plot + geom_ribbon(data =this_visreg$fit, aes(x = .data[[this_visreg$meta$x]], ymin = visregLwr, ymax = visregUpr), fill = "darkturquoise", linewidth = 2, alpha = 0.35)
  this_plot <- this_plot + geom_line(data = this_visreg$fit, aes(x = .data[[this_visreg$meta$x]], y = visregFit), col = "darkturquoise")
  
  this_plot <- this_plot + labs(y = paste0("f", "(", var, ")"))
  this_plot <- this_plot + theme_bw()
 
  return(this_plot)
  
}



# 
plotInteractionTerm <- function(vars, model, dt, ranges, levels_to_plot = NULL){
  
  print(paste0(" **** Plotting interaction term: ", paste(vars, collapse = "*")))
  
  # generate data for the interaction term
  single_dt <- copy(dt)
  # first variable is comparatively easy
  var1 <- vars[1]
  var1_summary <- model$var.summary[[var1]]
  single_dt[ , c(var1) := seq(var1_summary[1], var1_summary[3], length.out = nrow(dt))]
  # second variable requires copying the data.table many times
  var2 <- vars[2]
  var2_summary <- model$var.summary[[var2]]
  this_dt <- data.table()
  for(this_var2_value in seq(var2_summary[1], var2_summary[3], length.out = nrow(dt))){
    temp_dt <- copy(single_dt)
    temp_dt[ , c(var2) := this_var2_value]
    this_dt <- rbind(this_dt, temp_dt)
    rm(temp_dt)
  }
  
  print(this_dt)
  
  # IDENTIFY FACTOR IF THERE IS ONE
  fixed_effect <- names(model$var.summary[sapply(model$var.summary, function(x) class(x) == "factor")])
  if(length(fixed_effect) > 1) stop("Can only plot data with at most single factor (i.e. only one fixed effect)")
  
  # DETERMINE IF WE PLOT AT ALL LEVELS OR JUST THE INDICATED ONES
  if(length(fixed_effect) > 0) {
    if(is.null(levels_to_plot) | missing(levels_to_plot)) {
      if(sum(!levels_to_plot %in% model$xlevels[[1]])) stop("Requested to plot a fator level that doesn't seem to be in the data")
      levels_to_plot <- model$xlevels[[1]]
    }
  }
  
  # if necessary repeat for each factor present 
  if(length(fixed_effect) > 0) {
    for_plotting <- data.table()
    for(this_level in levels_to_plot){
      this_level_dt <- copy(this_dt)
      set(this_level_dt, j = fixed_effect, value = factor(this_level, levels = levels_to_plot))
      for_plotting <- rbind(for_plotting, this_level_dt)
    }
  }
  else{
    for_plotting <- this_dt
  }
  
  # now predict with standard errors
  tic()
  for_plotting[ , c("fit_link", "se_link")  := predict(model, for_plotting, se.fit = TRUE)]
  toc()
  for_plotting[ , response :=  model$family$linkinv(fit_link) ]
  for_plotting[ , se_upper :=  model$family$linkinv(fit_link + (1.96 * se_link)) ]
  for_plotting[ , se_lower :=  model$family$linkinv(fit_link - (1.96 *se_link)) ]
  for_plotting[ , se :=  model$family$linkinv(1.96 *se_link) ]
  tic()
  # plot (and return)
  this_plot <- ggplot(data = for_plotting, mapping = aes(x = .data[[var1]], y = .data[[var2]])) + geom_raster(aes( fill = response)) +  xlab(var1) + ylab(var2) + scale_fill_viridis() +  scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) 
  toc()
  
  return(this_plot)
  
  
  
}


# 
plotInteractionTermVisReg <- function(x, vars, model){
  
  print(paste0(" **** Plotting interaction term: ", paste(vars, collapse = "*")))
  
  
  # first variable is comparatively easy
  var1 <- vars[1]
  var2 <- vars[2]
  
  
  # IDENTIFY FACTOR IF THERE IS ONE
  fixed_effect <- names(model$var.summary[sapply(model$var.summary, function(x) class(x) == "factor")])
  if(length(fixed_effect) > 0) stop("Can currently only plot interaction terms via visreg with no factors.")
   
  
  for_plotting <- data.frame(x = x$x[row(x$z)], y = x$y[col(x$z)], z = c(x$z)) #  data.frame(x = x$x, y = x$y, z = x$z)

  
  # # DETERMINE IF WE PLOT AT ALL LEVELS OR JUST THE INDICATED ONES
  # if(length(fixed_effect) > 0) {
  #   if(is.null(levels_to_plot) | missing(levels_to_plot)) {
  #     if(sum(!levels_to_plot %in% model$xlevels[[1]])) stop("Requested to plot a fator level that doesn't seem to be in the data")
  #     levels_to_plot <- model$xlevels[[1]]
  #   }
  # }
  
  
  # plot (and return)
  this_plot <- ggplot(data = for_plotting, mapping = aes(x = x, y = y)) + geom_raster(aes( fill = z)) +  xlab(var1) + ylab(var2) 
  this_plot <- this_plot + scale_fill_viridis(name = paste0("f(", vars[1], ",", vars[2], ")")) +  scale_x_continuous(expand = c(0,0)) +  scale_y_continuous(expand = c(0,0)) 
  
  
  return(this_plot)
  
  
  
}



doSeasonalStats <- function(dt, layers1, layers2, nboot = 1000) {
  
  # first make a copy to keep it clean 
  x <- copy(dt)
  
  
  #### FIRST CALCULATE THE SEASONAL CONCENTRATION AND PHASE
  ##   Equations numbers refer to Kelley et al 2013 Biogeosciences
  
  #  calculate the theta_t, L_x and L_y
  theta_t <- 2 * pi * (1:12 -1) /12  # equation (5)
  x[, Theta_t := theta_t[Month]] # apply equation (5)
  x[, L_x_1 := get(layers1) * cos(Theta_t) ] # equation (6) without summation
  x[, L_y_1 := get(layers1) * sin(Theta_t) ] # equation (6) without summation
  x[, L_x_2 := get(layers2) * cos(Theta_t) ] # equation (6) without summation
  x[, L_y_2 := get(layers2) * sin(Theta_t) ] # equation (6) without summation
  
  # now sum over all months - the summations from equation (6), but also make the denominator from equation(7)
  dims <- c("Lat", "Lon")
  if("Year" %in% names(x))  dims <- c("Lat", "Lon", "Year")
  x.summed <-  x[, j=list(L_x_1 = sum(L_x_1), L_y_1 = sum(L_y_1), L_x_2 = sum(L_x_2), L_y_2 = sum(L_y_2), Sigma_x_1 = sum(get(layers1)), Sigma_x_2 = sum(get(layers2))), by=dims]
  
  # remove where either Sigma == 0
  x.summed <- x.summed[ Sigma_x_1 != 0 & Sigma_x_2 != 0]
  
  # concentration and phase equations (7) and (8)
  x.summed[, C_1 := (L_x_1^2 + L_y_1^2) ^0.5 / Sigma_x_1]
  x.summed[, C_2 := (L_x_2^2 + L_y_2^2) ^0.5 / Sigma_x_2]
  x.summed[, P_1 := atan2(L_x_1,L_y_1)]
  x.summed[, P_2 := atan2(L_x_2,L_y_2)]
  
  #### KELLEY ET AL 2013 METRICS
  
  #### CONCENTRATION
  # code essentially ripped from continuousComparison above
  
  # Preamble - extract vectors and remove NAs from both vectors 
  vector1 <- x.summed[["C_1"]]
  vector2 <- x.summed[["C_2"]]
  
  # first remove where there are NAs in vector1
  vector2 <- vector2[!is.na(vector1)]
  vector1 <- vector1[!is.na(vector1)]
  # now for vector2
  vector1 <- vector1[!is.na(vector2)]
  vector2 <- vector2[!is.na(vector2)]
  
  # Normalised metrics: NME and NMSE (step 1)
  NME <- calcNME(mod = vector1, obs = vector2)
  NMSE <- calcNMSE(mod = vector1, obs = vector2)
  
  # Bootstrap null for NME
  null_NME <- NA
  set.seed(5678)
  if(nboot > 0) {
    null_NME <- 0
    for(rep in 1:nboot){
      null_NME <- null_NME + calcNME(mod = sample(vector2), obs =vector2)
    }
    null_NME <- null_NME/nboot
  }
  
  
  
  # and step 2 for NME and NMSE
  vector1_step2 <- vector1 - mean(vector1)
  vector2_step2 <- vector2 - mean(vector2)
  NME_2 <- calcNME(mod = vector1_step2, obs = vector2_step2)
  NMSE_2 <- calcNMSE(mod = vector1_step2, obs = vector2_step2)
  
  # and step 3 for NME and NMSE
  vector1_step3_NME <- vector1_step2 / sum(abs(vector1_step2 - mean(vector1_step2)))/ length(vector1_step2)
  vector2_step3_NME <- vector2_step2 / sum(abs(vector2_step2 - mean(vector2_step2)))/ length(vector2_step2)
  NME_3 <- calcNME(mod = vector1_step3_NME, obs = vector2_step3_NME)
  
  vector1_step3_NMSE <- vector1_step2 / stats::var(vector1_step2)
  vector2_step3_NMSE <- vector2_step2 / stats::var(vector2_step2)
  NMSE_3 <- calcNMSE(mod = vector1_step3_NMSE, obs = vector2_step3_NMSE)
  
  #### PHASE
  # Preamble - extract vectors and remove NAs from both vectors 
  phase1 <- x.summed[["P_1"]]
  phase2 <- x.summed[["P_2"]]
  
  # first remove where there are NAs in phase1
  phase2 <- phase2[!is.na(phase1)]
  phase1 <- phase1[!is.na(phase1)]
  # now for phase2
  phase1 <- phase1[!is.na(phase2)]
  phase2 <- phase2[!is.na(phase2)]
  
  MPD <- (1/pi) * sum(acos (cos(phase1 - phase2))) / length(phase1)
  
  tic()
  null_MPD <- NA
  set.seed(9753)
  if(nboot > 0) {
    null_MPD <- 0
    for(rep in 1:nboot){
      null_MPD <- null_MPD + ((1/pi) * sum(acos (cos(sample(phase2) - phase2))) / length(phase1))
    }
    null_MPD <- null_MPD/nboot
  }
  toc()
  
  
  #### COMPILE STATS
  stats <- list("NME_conc" = NME,
                "null_NME_conc" = null_NME,
                "NMSE_conc" = NMSE,
                "NME_conc_2" = NME_2,
                "NMSE_conc_2" = NMSE_2,
                "NME_conc_3" = NME_3,
                "NMSE_conc_3" = NMSE_3,
                "MPD" = MPD,
                "null_MPD" = null_MPD
  )
  
}




#' Compare continuous data
#' 
#' Compares two datasets of continuous data. Specifically calculates and returns a Statistics object (which contains many metrics) given numeric two vectors of equal size 
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containing the data to be compared
#' @param layers1 A character string giving the first layer to compare (should be a column in x).  For the normalised metrics, this is the *modelled* values.
#' @param layers2 A character string giving the second layer to compare (should be a column in x).  For the normalised metrics, this is the *observed* values.
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{\link{compareLayers}}
#' @param verbose A logical, if TRUE print out all the metric scores
#' @param area A logical, if true weight the metrics by gridcell area
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
continuousComparison <- function(x, layers1, layers2, additional = NULL, verbose = TRUE, area = TRUE, tolerance = 0.01, nbootstrap = 1000){
  
  # check the layers are present
  if(!layers1 %in% layers(x)) stop("Argument layers1 is not a column in x")
  if(!layers2 %in% layers(x)) stop("Argument layers2 is not a column in x")
  
  # add the area if selected
  if(area) {
    
    x.dims <- getDimInfo(x)
    
    if(!"Lon" %in% x.dims ||  !"Lat" %in% x.dims) {
      warning("Comparison stats will not be weighted because Lon/Lat not present")
      area.vec <- NULL
    } 
    else {
      x <- addArea(x, unit = "km^2", tolerance = tolerance)
      area.vec <- x[["Area"]]
    }
    
  }
  else area.vec <- NULL
  
  
  ###  STANDARD PACKAGE BENCHMARKS WHICH CAN RUN SIMPLY ON TWO VECTORS
  
  # Preamble - extract vectors and remove NAs from both vectors 
  vector1 <- x[[layers1]]
  vector2 <- x[[layers2]]
  
  # first remove where there are NAs in vector1
  vector2 <- vector2[!is.na(vector1)]
  vector1 <- vector1[!is.na(vector1)]
  # now for vector2
  vector1 <- vector1[!is.na(vector2)]
  vector2 <- vector2[!is.na(vector2)]
  
  
  #### KELLEY ET AL 2013 METRICS
  
  # Unnormalised metrics:  ME, MSE and RSME
  if(is.null(area.vec)) {
    ME <- mean(abs(vector1 - vector2))
    MSE <- mean((vector1 - vector2)^2, na.rm=TRUE)
  }
  else {
    ME <- sum(abs(vector1 - vector2) * area.vec, na.rm=TRUE) / sum(area.vec)
    MSE <- sum((vector1 - vector2)^2 * area.vec, na.rm=TRUE) / sum(area.vec)
  }
  RMSE <- MSE^0.5
  
  # Normalised metrics: NME and NMSE (step 1)
  NME <- calcNME(mod = vector1, obs = vector2, area = area.vec)
  NMSE <- calcNMSE(mod = vector1, obs = vector2, area = area.vec)
  
  # bootstrap NME
  tic()
  null_NME <- 0
  set.seed(2468)
  for(iter in 1:nbootstrap){
    null_NME <- null_NME + calcNME(mod = sample(vector2), obs = vector2, area = area.vec)
  }
  null_NME <- null_NME/nbootstrap
  toc()
  
  # and step 2 for NME and NMSE
  vector1_step2 <- vector1 - mean(vector1)
  vector2_step2 <- vector2 - mean(vector2)
  NME_2 <- calcNME(mod = vector1_step2, obs = vector2_step2, area = area.vec)
  NMSE_2 <- calcNMSE(mod = vector1_step2, obs = vector2_step2, area = area.vec)
  
  # and step 3 for NME and NMSE
  vector1_step3_NME <- vector1_step2 / sum(abs(vector1_step2 - mean(vector1_step2)))/ length(vector1_step2)
  vector2_step3_NME <- vector2_step2 / sum(abs(vector2_step2 - mean(vector2_step2)))/ length(vector2_step2)
  NME_3 <- calcNME(mod = vector1_step3_NME, obs = vector2_step3_NME, area = area.vec)
  
  
  # bootstrap NME
  tic()
  null_NME3 <- 0
  set.seed(1357)
  for(iter in 1:nbootstrap){
    null_NME3 <- null_NME3 + calcNME(mod = sample(vector2_step3_NME), obs = vector2_step3_NME, area = area.vec)
  }
  null_NME3 <- null_NME3/nbootstrap
  toc()
  
  vector1_step3_NMSE <- vector1_step2 / stats::var(vector1_step2)
  vector2_step3_NMSE <- vector2_step2 / stats::var(vector2_step2)
  NMSE_3 <- calcNMSE(mod = vector1_step3_NMSE, obs = vector2_step3_NMSE, area = area.vec)
  
  
  #### MORE 'STANDARD' METRICS MORE BASED ON LINEAR REGRESSION AND NOT FOCUSSED ON MODEL-OBSERVATION COMPARISON
  
  if(!is.null(area.vec)) {
    if(verbose) message("NOTE: metrics r, r2, m, and c are NOT weighted by gridcell area, the other metrics are.")
    warning("NOTE: metrics r, r2, m, and c are NOT weighted by gridcell area, the other metrics are.")
    
  }
  
  # r2_eff - Nash-Sutcliffe model efficiency (actually is focussed on model-obs Comparisons)
  r2_eff <- 1 - NMSE
  
  # Pearson product moment correlation coefficient, and then R^2
  # MF: not the best thing in my opinion
  r <- stats::cor(vector1, vector2, method = "pearson", )
  r2 <- r^2
  
  # calculate a simple linear regression 
  simple.regression <- stats::lm(formula = mod ~ obs, data = data.frame("mod" = vector1, "obs" = vector2))
  c <- stats::coef(simple.regression)[1]
  m <- stats::coef(simple.regression)[2]
  
  
  stats <- list("ME" = ME, 
                "NME" = NME,
                "NMSE" = NMSE,
                "RMSE" = RMSE,
                "NME_2" = NME_2,
                "NMSE_2" = NMSE_2,
                "NME_3" = NME_3,
                "NMSE_3" = NMSE_3,
                "r2_eff" = r2_eff,
                "r" = r,
                "r2" = r2, 
                "m" = m,
                "c" = c,
                "null NME" = null_NME,
                "null NME3" = null_NME3
  )
  
  
  ##### HERE DO CUSTOM BENCHMARKS
  if(length(additional) > 0) {
    for(counter in 1:length(additional)) {
      stats[[names(additional)[counter]]] <- additional[[counter]](x, layers1, layers2) 
    }
  }
  
  
  if(verbose) {
    
    print(paste("+++ Stats for", paste(layers1, sep = ","), "vs",  paste(layers2, sep = ","),  "+++", sep = " "))
    for(counter in 1:length(stats)) {
      
      stat.val <- stats[[counter]]
      stat.name <- names(stats)[counter]
      
      if(length(stat.val) == 1) {
        
        # also give a little more info for the standard metrics (ie their full name) before printing
        if(stat.name == "ME") stat.name <- "ME (Mean Error)"
        else if(stat.name == "NME") stat.name <- "NME (Normalised Mean Error)"
        else if(stat.name == "NMSE") stat.name <- "NMSE (Normalised Mean Square Error)"
        else if(stat.name == "RMSE") stat.name <- "RMSE (Root Mean Squared Error)"
        else if(stat.name == "NME_2") stat.name <- "NME_2 (NME with mean removed)"
        else if(stat.name == "NMSE_2") stat.name <- "NMSE_2 (NMSE with mean removed)"
        else if(stat.name == "NME_3") stat.name <- "NME_3 (NME with mean and variance removed)"
        else if(stat.name == "NMSE_3") stat.name <- "NMSE_3 (NMSE with mean and variance removed)"
        else if(stat.name == "r2_eff") stat.name <- "r2_eff (Nash-Sutcliffe Model Efficiency)"
        else if(stat.name == "r2") stat.name <- "r2 (Coefficient of Determination)"
        else if(stat.name == "r") stat.name <- "r (Pearson's PMCC)"
        else if(stat.name == "m") stat.name <- "m (gradient of linear fit mod = m * obs + c)"
        else if(stat.name == "c") stat.name <- "c (intercept of linear fit mod = m * obs + c)"
        print(paste(stat.name,  "=", round(stat.val, 4), sep = " "))
        
      }
      else {
        
        # here print each sub value of the metric
        print(paste0(stat.name, ":"))
        for(counter2 in 1:length(stat.val)) {
          sub.stat.val <- stat.val[[counter2]]
          sub.stat.name <- names(stat.val)[[counter2]]
          print(paste("  ", sub.stat.name,  "=", round(sub.stat.val, 4), sep = " "))
        }
        
      }
    }
  }
  
  return(stats)
  
}

#' Compare relative proportions data
#' 
#' Compares two datasets of relative proportions of multiple classes (sum of classes equals one at each point) where the total for each  data where the totally value for each. 
#' Specifically calculates and returns a list with the Manhattan Metric (MM) and Square Chord Distance (SCD).
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containing the data to be compared
#' @param layers1 A vector of character strings giving the layers from the first dataset to compare (should be columns in x and sum to 1 or 100)
#' @param layers1 A vector of character strings giving the layers from the second dataset to compare (should be columns in x and sum to 1 or 100)
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{\link{compareLayers}}
#' @param verbose A logical, if TRUE print out all the metric scores
#' @param area A logical, if true weight the metrics by gridcell area (not implemented)
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
proportionsComparison <- function(x, layers1, layers2, additional, verbose = TRUE, area = TRUE){
  
  if(area) {
    if(verbose) message("Gridcell area weighting not currently implemented for proportionsComparison")
    warning("Gridcell area weighting not currently implemented for proportionsComparison")
  }
  
  # check the layers are present
  if(!sum(layers1 %in% names(x)) == length(layers1)) stop("Some of argument layers1 are not a column in x")
  if(!sum(layers2 %in% names(x)) == length(layers2)) stop("Some of argument layers2 are not a column in x")
  
  dt1 <- x[, layers1]
  dt2 <- x[, layers2]
  
  # check the incoming data.tables are the same size
  if(ncol(dt1) != ncol(dt2)) stop("Trying to compare proportions (Manhattan Metric and Square Chord Distance) with different number of components")
  
  # check the incoming data.tables are the same size
  if(nrow(dt1) != nrow(dt2)) stop("Trying to compare proportions (Manhattan Metric and Square Chord Distance) with different number of rows")
  
  # quick fix, divide by 100 if max > 1.01
  dt1.sum <- rowSums(dt1)
  dt2.sum <- rowSums(dt2)
  if(max(dt1.sum) > 1.01 || max(dt2.sum) > 1.01){
    dt1 <- dt1/100
    dt2 <- dt2/100
  }
  
  # calculate Manhattan Metric and Squared Chord Distance
  MM <- 0
  SCD <- 0
  for(layer.index in 1:ncol(dt1)){
    
    
    # for Manhattan Metric
    difference.vector <- abs(dt1[[layer.index]] - dt2[[layer.index]])
    MM <- MM + sum(difference.vector)
    
    # for Square Chord Distance
    difference.vector <- ((dt1[[layer.index]])^0.5 - (dt2[[layer.index]])^0.5)^2
    SCD <- SCD + sum(difference.vector)
    
  }
  
  MM <- MM/nrow(dt1)
  SCD <- SCD/nrow(dt1)
  
  
  stats <- list("MM" = MM, 
                "SCD" = SCD
  )
  
  ##### HERE DO CUSTOM BENCHMARKS
  if(length(additional) > 0) {
    for(counter in 1:length(additional)) {
      stats[[names(additional)[counter]]] <- additional[[counter]](x, layers1, layers2) 
    }
  }
  
  
  if(verbose) {
    
    print(paste("+++ Stats for", paste(layers1, sep = ","), "vs",  paste(layers2, sep = ","),  "+++", sep = " "))
    for(counter in 1:length(stats)) {
      
      stat.val <- stats[[counter]]
      stat.name <- names(stats)[counter]
      
      if(length(stat.val) == 1) {
        
        # also give a little more info for the standard metrics (ie their full name) before printing
        if(stat.name == "MM") stat.name <- "MM (Manhattan Metric)"
        else if(stat.name == "SCD") stat.name <- "SCD (Square Chord Distance)"
        print(paste(stat.name,  "=", round(stat.val, 4), sep = " "))
        
      }
      else {
        
        # here print each sub value of the metric
        print(paste0(stat.name, ":"))
        for(counter2 in 1:length(stat.val)) {
          sub.stat.val <- stat.val[[counter2]]
          sub.stat.name <- names(stat.val)[[counter2]]
          print(paste("  ", sub.stat.name,  "=", round(sub.stat.val, 4), sep = " "))
        }
        
      }
    }
  }
  
  
  return(stats)
  
}

