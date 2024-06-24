# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/run_all_pub_v1.0.R")
library(here)

# tables
source(here("scripts", "publication_plots", "BASE_v1.0", "tabulate_coefficients_v1.0_pub.R"))
source(here("scripts", "publication_plots", "BASE_v1.0", "tabulate_metrics_v1.0_pub.R"))

# plots
source(here("scripts", "publication_plots", "BASE_v1.0", "plot_BA_per_LC_v1.0_pub.R"))
source(here("scripts", "publication_plots", "BASE_v1.0", "plot_correlations_v1.0_pub.R"))
source(here("scripts", "publication_plots", "BASE_v1.0", "plot_final_predictors_v1.0_pub.R"))
source(here("scripts", "publication_plots", "BASE_v1.0", "plot_variable_importance_v1.0_pub.R"))
source(here("scripts", "publication_plots", "BASE_v1.0", "plot_BA_patterns_main_models_v1.0_pub.R"))
source(here("scripts", "publication_plots", "BASE_v1.0", "plot_BA_patterns_alternate_models_v1.0_pub.R"))
