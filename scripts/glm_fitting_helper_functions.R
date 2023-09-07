
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

# Error in `[.data.table`(dt, , lapply(.SD, method, na.rm = TRUE), .SDcols = all_vars[!all_vars %in%  : 
# Some items of .SDcols are not column names: [poly(PopDens, 2)]
# In addition: There were 50 or more warnings (use warnings() to see the first 50)


calculatePredictionDT <- function(dt, model, method, npoints = 100) {
  
  # determine list of predictor variable involved
  tic()
  all_vars <- as.character(attr(model$terms, "variables"))[3:length(as.character(attr(model$terms, "variables")))] 
  print(all_vars)
  toc()
  # identify fixed terms so we can avoid adding them (since we don't wanna add column of NAs)
  tic()
  all_factors <- names(model$var.summary[sapply(model$var.summary, function(x) class(x) == "factor")])
  print(all_factors)
  tic()
  # extract polynomial and splines
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
  print("SDcols:")
  print(all_vars[!all_vars %in% all_factors])
  
  
  # apply the method and make repeated values repeat 
  tic()
  line_dt <- dt[ , lapply(.SD, method, na.rm=TRUE), .SDcols = all_vars[!all_vars %in% all_factors]]
  print(line_dt)
  toc()
  fixed_dt <-	line_dt[rep(1, npoints)]
  
}


plotSimpleTerm <- function(var, model, dt, range){
  
  print(paste0(" **** Plotting simple (non-interacting) term: ", var))
  
  # generate data for the linear term and predict
  this_dt <- copy(dt)
  this_dt[ , c(var) := seq(range[1], range[2], length.out = nrow(dt))]
  print
  
  
  # IDENTIFY FACTOR IF THERE IS ONE
  fixed_effect <- names(model$var.summary[sapply(model$var.summary, function(x) class(x) == "factor")])
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
  prediction_dt <- predict(model, for_plotting, se = TRUE)
  for_plotting[ , c("fit_link", "se_link")  := predict(model, for_plotting, se.fit = TRUE)]
  for_plotting[ , response :=  model$family$linkinv(fit_link) ]
  for_plotting[ , se_upper :=  model$family$linkinv(fit_link + (1.96 * se_link)) ]
  for_plotting[ , se_lower :=  model$family$linkinv(fit_link - (1.96 *se_link)) ]
  
  # if fixed effect
  if(length(fixed_effect) > 0) {
    this_plot <- ggplot(data = for_plotting, aes(x = .data[[var]], y = response)) + geom_line(aes(col = .data[[fixed_effect]])) 
    this_plot <- this_plot + geom_ribbon(aes(ymin = se_lower, ymax = se_upper, fill = .data[[fixed_effect]]), alpha = 0.25)
    this_plot <- this_plot + scale_fill_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect)) + scale_color_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect))
  }
  else {
    this_plot <- ggplot(data = for_plotting, aes(x = .data[[var]], y = response)) + geom_line() 
    this_plot <- this_plot + geom_ribbon(aes(ymin = se_lower, ymax = se_upper), alpha = 0.25)
    #this_plot <- this_plot + scale_fill_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect)) + scale_color_discrete(name = gsub(pattern = "_", replacement = " ", fixed_effect))
    
  }
  
  
  return(this_plot)
  
}


# 
plotInteractionTerm <- function(vars, model, dt, ranges, levels_to_plot = NULL){
  
  print(paste0(" **** Plotting interaction term: ", paste(vars, collapse = "*")))
  
  # generate data for the interaction term
  single_dt <- copy(dt)
  # first variable is comparatively easy
  var1 <- vars[1]
  single_dt[ , c(var1) := seq(ranges[[var1]][1], ranges[[var1]][2], length.out = nrow(dt))]
  # second variable requires copying the data.table many times
  var2 <- vars[2]
  this_dt <- data.table()
  for(this_var2_value in seq(ranges[[var2]][1], ranges[[var2]][2], length.out = nrow(dt))){
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
  prediction_dt <- predict(model, for_plotting, se = TRUE)
  for_plotting[ , c("fit_link", "se_link")  := predict(model, for_plotting, se.fit = TRUE)]
  for_plotting[ , response :=  model$family$linkinv(fit_link) ]
  for_plotting[ , se_upper :=  model$family$linkinv(fit_link + (1.96 * se_link)) ]
  for_plotting[ , se_lower :=  model$family$linkinv(fit_link - (1.96 *se_link)) ]
  for_plotting[ , se :=  model$family$linkinv(1.96 *se_link) ]
  
  # plot (and return)
  this_plot <- ggplot(data = for_plotting) + geom_tile(aes(x = .data[[var1]], y = .data[[var2]], fill = response)) +  xlab(var1) + ylab(var2) + scale_fill_viridis() +  scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) 
  
  return(this_plot)
  
  
  
}

