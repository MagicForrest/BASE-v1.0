#### PREAMBLE ####
library(ggplot2)
library(ggpubr)
library(data.table)
library(viridis)
library(sf)
library(tictoc)
library(mgcv)

# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_final_predictors.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))
source(here("scripts", "glm_fitting_helper_functions.R"))


lcc_colours <- c("Cropland" = "orchid4", "NCV" ="springgreen4")

agg_method = "median"

# define the models to plot
ncv_model <- "BASE_v1.0"
cropland_model <- "BASE_v1.0"
prefix_string <- "BASE_v1.0"


# dataset names
obs_name <- "FireCCI51"
sim_name <- "BASE"
lcc_names <- c(obs_name, sim_name)



text.multiplier <- 2.8

npoints <- 1000

#  Directories for reading data and saving plots
plot_dir <- here("plots/manuscript_BASE_v1.0")
intermediates_dir <- here("intermediates", "GLMs",  prefix_string)


#### READ DATA AND MODEL AND MAKE SOME TEMPLATES ####

# read data.tables and set names
ncv_dt <- readRDS(file.path(intermediates_dir, "BurntFraction_NCV",  ncv_model, paste("DT", ncv_model, "rds", sep = ".")))
ncv_model <- readRDS(file.path(intermediates_dir, "BurntFraction_NCV",  ncv_model, paste("GLM", ncv_model, "rds", sep = ".")))
cropland_dt <- readRDS(file.path(intermediates_dir, "BurntFraction_PureCropland",  cropland_model, paste("DT", cropland_model, "rds", sep = ".")))
cropland_model <- readRDS(file.path(intermediates_dir, "BurntFraction_PureCropland",  cropland_model, paste("GLM", cropland_model, "rds", sep = ".")))
setnames(ncv_dt, c("Observed_burnt_area", "Predicted_burnt_area_raw"), lcc_names) 
setnames(cropland_dt, c("Observed_burnt_area", "Predicted_burnt_area_raw"), lcc_names) 

# calculate the a data.table with the predictor values at mean values
tic()
ncv_prediction_template_dt <- calculatePredictionDT(dt = ncv_dt, model = ncv_model, npoints = npoints) 
cropland_prediction_template_dt <- calculatePredictionDT(dt = cropland_dt, model = cropland_model,  npoints = npoints) 

# calculate burnt area at mean value
ncv_at_const <- predict(ncv_model, ncv_prediction_template_dt[1,], type = "response")
cropland_at_const <- predict(cropland_model, cropland_prediction_template_dt[1,], type = "response")
toc()



#### DEFINE MODELS ####
all_models <- list(
  
  "NCV" = list(name = "NCV", model = ncv_model, dt = ncv_prediction_template_dt, mean_value = ncv_at_const),
  "Cropland" = list(name = "Cropland", model = cropland_model, dt = cropland_prediction_template_dt, mean_value = cropland_at_const)
  
)


#### SINGLE-VARIATE RESPONSES ####

# get all continuous predictors
all_cont_vars <- c()
all_common_vars <- c()

for(index in 1:length(all_models)) {
  this_model <- all_models[[index]]
  all_cont_vars <- append(all_cont_vars, listContinuousPredictors(this_model$model))
  if(index ==1) all_common_vars <- listContinuousPredictors(this_model$model)
  else all_common_vars <- intersect(all_common_vars, listContinuousPredictors(this_model$model) )
}
all_cont_vars <- unique(c(all_common_vars, all_cont_vars))


# calculate responses of all variables in all models
all_responses_dt <- data.table()
for(this_predictor in all_cont_vars){
  
  for(this_model_name in names(all_models)) {
    
    # if predictor is used in model, predict it and add it to the data.table
    if(this_predictor %in% names(all_models[[this_model_name]]$model$var.summary)) {
      this_prediction <- predictForPlotting(var = this_predictor, dt = all_models[[this_model_name]]$dt, model = all_models[[this_model_name]]$model)
      this_prediction <- this_prediction[,  c(this_predictor, "fit_link",  "se_link",   "response",  "se_upper", "se_lower"), with = FALSE]
      print(this_prediction)
      this_prediction <- melt(this_prediction, measure.vars = this_predictor, variable.name = "predictor")
      this_prediction[ , model := this_model_name ]
      all_responses_dt <- rbind(all_responses_dt, this_prediction)
    }
    
  }
  
}

# make a plot for each (and save them in a list)
all_predictor_plots <- list()
for(this_predictor in all_cont_vars) {
  
  this_response_dt <- all_responses_dt[ predictor == this_predictor , ]
  
  this_response_plot <- ggplot(data = this_response_dt, aes(x = value, y = response)) + geom_line(aes(col = model)) 
  this_response_plot <- this_response_plot + geom_ribbon(aes(ymin = se_lower, ymax = se_upper, fill = model), alpha = 0.25)
  this_response_plot <- this_response_plot + theme_bw()
  this_response_plot <- this_response_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                                                   legend.text=element_text(size=theme_get()$text$size * text.multiplier))
  this_response_plot <- this_response_plot + labs(y = "Burnt fraction response",
                                                  x = this_predictor, 
                                                  fill= "Land cover class", 
                                                  colour = "Land cover class")
  this_response_plot <- this_response_plot + scale_fill_manual(values = lcc_colours, )
  this_response_plot <- this_response_plot + scale_colour_manual(values = lcc_colours)
  #this_response_plot <- this_response_plot + scale_x_continuous(expand = c(0, 0)) 
  #this_response_plot <- this_response_plot + scale_y_continuous(expand = c(0, 0)) 
  if("NCV" %in% unique(this_response_dt$model)) this_response_plot <- this_response_plot + geom_hline(aes(yintercept = ncv_at_const), col = lcc_colours[["NCV"]], linetype = 2)
  if("Cropland" %in% unique(this_response_dt$model)) this_response_plot <- this_response_plot + geom_hline(aes(yintercept = cropland_at_const), col = lcc_colours[["Cropland"]], linetype = 2)
  #print(this_response_plot)
  all_predictor_plots[[this_predictor]] <- this_response_plot
  
}

all_predictor_plot <- ggarrange(plotlist = all_predictor_plots, common.legend = TRUE, legend = "top", ncol = 3, nrow = 4)
print(all_predictor_plot)
magicPlot(p = all_predictor_plot, filename = file.path(plot_dir, paste0("Figure5")),  width = 1500, height = 1800)


#### INTERACTION RESPONSES ####

# determine the interaction responses
interaction_terms_per_model <- list()
all_interaction_plots <- list()
for(this_model in all_models) {
  
  these_interactions <- list(getInteractionTerms(this_model$model))
  for(this_interaction in these_interactions){
    
    these_terms <- unlist(these_interactions)
    inter_plot <- plotInteractionTerm(vars = these_terms, this_model$model, dt = this_model$dt) 
    inter_plot <- inter_plot + geom_contour(aes(z = response),
                                            color = "red",
                                            linetype = 2,
                                            breaks = this_model$mean_value,
                                            show.legend = F)
    inter_plot <- inter_plot + labs(fill = "Burnt fraction \nresponse",
                                    title = paste(this_model$name, "interaction response: ", paste(these_terms[1], these_terms[2], sep = "*")))
    inter_plot <- inter_plot + theme_bw()
    inter_plot <- inter_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
    all_interaction_plots[[paste0(this_model$name, "_", these_terms[1], "*", these_terms[2])]] <- inter_plot

  }
  
  
}

all_interaction_plots <- ggarrange(plotlist = all_interaction_plots)
magicPlot(p = all_interaction_plots, filename = file.path(plot_dir, paste0("Figure6")),  width = 1900, height = 800)







