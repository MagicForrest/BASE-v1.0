# NOTE: For this script we are reading a table of the SHAP-derived variable important values that we saved when we fitted the model.
# This is because the vi() function used to get the variable importance doesn't seem to work well if it the fitted model object is 
# no longer in it's original environment

library(ggplot2)
library(ggpubr)
# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_variable_importance_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))


# text.multiplier <- 2.3

analysis_version <- "BASE_v1.0"

#  Directories for reading data and saving plots
pub_results_dir <- here("plots", analysis_version, "manuscript")
models_dir <- here("fitted_models", analysis_version, "GLMs")


#### DEFINE BASELINES ####

cropland_col <- "orchid4"
ncv_col <- "springgreen4"
  
baselines_list <- list(
  
  "NCV" = list(name = "NCV",
               nice_name = "NCV",
               colour = ncv_col,
               linetype = "dashed",
               baseline_model_id ="BASE_v1.0",
               baseline_model_name ="BASE v1.0"),
  
  "Cropland" = list(name = "PureCropland",
                    nice_name = "Cropland",
                    colour = cropland_col,
                    linetype = "dashed",
                    baseline_model_id ="BASE_v1.0",
                    baseline_model_name ="BASE v1.0")     
  
)




# loop across models to plot

all_shap_plots <- list()
for(this_baseline in baselines_list) {

  #### READ AND PLOT THE SHAP STATS ####
  shap_df <- read.table(file.path(models_dir, paste("BurntFraction", this_baseline$name, sep = "_"),  this_baseline$baseline_model_id,  "shap_values.txt"),
                           header = TRUE, stringsAsFactors = TRUE)
  shap_df <- data.frame(Predictor = factor(shap_df$Predictor, shap_df$Predictor, ordered = TRUE), Importance = shap_df$Importance)
  
  # Make the plot
  shap_plot <- ggplot(data = shap_df) 
  shap_plot <- shap_plot + geom_bar(aes(y = Importance, x= Predictor), stat = "identity", fill = this_baseline$colour)
  shap_plot <- shap_plot + coord_flip() + theme_bw()
  shap_plot <- shap_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier),
                                 plot.margin = margin(r = 15))
  shap_plot <- shap_plot + labs(y = "SHAP Importance", x = paste(this_baseline$nice_name, "Predictor"))

  # print and save the plots and SHAP values
  print(shap_plot)
  #magicPlot(p = shap_plot, filename = file.path(plot.dir, paste("SHAP_Values")), height = 1200, width =1200)

  all_shap_plots[[this_baseline$name]] <- shap_plot                                    
  
}


fig03_png <- ggarrange(plotlist = all_shap_plots,
                       ncol = 2,
                       labels = "auto",
                       font.label = list(size = 18, face = "bold"), 
                       align = "hv"
)

magicPlot(p = fig03_png, filename = file.path(pub_results_dir, paste("Figure_03_Variable_Importance")), width= 1800, height  = 900)

fig03_pdf <- ggarrange(plotlist = all_shap_plots,
                       ncol = 2,
                       labels = "auto",
                       font.label = list(size = 18, face = "bold"), 
                       align = "hv"
)


pdf(file = file.path(pub_results_dir, paste0("Figure_03_Variable_Importance.pdf")), width = 18, height = 9)
print(fig03_pdf)
dev.off()

pdf(file = file.path(pub_results_dir, paste0("fig_03.pdf")), width = 18, height = 9)
print(fig03_pdf)
dev.off()

