library(data.table)
library(gt)
library(stringr)
library(glue)
# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/plot_correlations_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))

# handy function for rounding numerics columns only
signif_df <- function(x, digits) {
  numeric_columns <- names(x)[sapply(x, mode) == 'numeric']
  x[ , (numeric_columns) := signif(.SD, digits), .SDcols = numeric_columns]
  return(x)
}

prefix_string <- "BASE_v1.0"


#  Directories for reading data and saving plots
pub_results_dir <- here("plots/BASE_v1.0/manuscript/")
fitted_models_dir <- here("fitted_models", prefix_string, "GLMs")


#### GET ALL THE PREDICTORS ####
source(here("scripts/v1.0_sensitivity_models.R"))


baselines_list <- list(
  
  "NCV" = list(name = "NCV",
               nice_name = "NCV",
               baseline_model_id ="BASE_v1.0",
               tab_number = 1),
  
  "Cropland" = list(name = "PureCropland",
                    nice_name = "Cropland",
                    baseline_model_id ="BASE_v1.0",
                    tab_number = 2)     
)


for(this_lcc_class in baselines_list) {
  
  # read table
  this_coeffs_table <- as.data.table(read.table(file.path(fitted_models_dir, paste("BurntFraction", this_lcc_class$name, sep = "_"), this_lcc_class$baseline_model_id, "coeffs_table.txt"),
                                  header = TRUE))
  
  # tidy a little 
  setnames(this_coeffs_table, c("Term", "Value", "Std error", "t-statistic", "p-value"))
  this_coeffs_table[ , `p-value` := ifelse(`p-value` < 1E-16, 0, `p-value`)]
  this_coeffs_table[ , Term := gsub(pattern = ", 2, raw = TRUE)1", repl = "", x = Term)]
  this_coeffs_table[ , Term := gsub(pattern = ", 2, raw = TRUE)2", repl = "^2", x = Term)]
  this_coeffs_table[ , Term := gsub(pattern = "poly(", repl = "", x = Term, fixed = TRUE)]
  
  
  # round to 3 signif figs
  this_coeffs_table_rounded <- signif_df(this_coeffs_table, 3)
  
  # convert to a gt table and save
  this_coeffs_gt <- gt(this_coeffs_table_rounded)
  gtsave(this_coeffs_gt, file = file.path(pub_results_dir, paste(paste0("Table_B0", this_lcc_class$tab_number),  this_lcc_class$name ,"coeffs.docx", sep = "_")))
  gtsave(this_coeffs_gt, file = file.path(pub_results_dir, paste(paste0("Table_B0", this_lcc_class$tab_number),  this_lcc_class$name ,"coeffs.rtf", sep = "_")))

}
