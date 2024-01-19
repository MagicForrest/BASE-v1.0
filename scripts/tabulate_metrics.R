#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)
library(tictoc)

# define root path with here package and 
here::i_am("scripts/tabulate_metrics.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))
source(here("scripts", "glm_fitting_helper_functions.R"))


#### 


#  Versions and sirectories for reading data and saving plots
all_lcc_types <- c("NCV", "PureCropland") 
baseline_models <- list(
  "NCV" = "BASE_v1.0",
  "PureCropland" = "BASE_v1.0"
)

target <- "BurntFraction"
version <- "BASE_v1.0"
plot_dir <- here("plots/manuscript_BASE_v1.0")


for(lcc_type in all_lcc_types) {
  
  
  runs_dir <- here("results", "GLMs",  version, paste(target, lcc_type, sep = "_"))
  
  
  # build table and set names
  all_metrics_dt <- data.table()
  for(run_dir in list.dirs(runs_dir)) {
    
    metrics_file <- file.path(run_dir, "metrics.txt" )
    if(file.exists(metrics_file)) {
      metric_line <- read.csv(metrics_file)
      all_metrics_dt <- rbind(all_metrics_dt, metric_line, fill=TRUE)
    }
    
  }
  
  setnames(all_metrics_dt, gsub(pattern = ".", replacement = " ", names(all_metrics_dt), fixed = TRUE))
  
  # select columns
  cols <- c("Description", 
            "Deviance explained", 
            "Full NME",
            "Null GLM Spatial NME", 
            "GLM Spatial NME", 
            "GLM Null MPD", 
            "GLM MPD", 
            "Null GLM IAV NME",  
            "GLM IAV NME",  
            "GLM NULL NME_conc",
            "GLM NME_conc"
  )
  
  final_table <- all_metrics_dt[ , ..cols]
  
  # remove "GLM" from the names
  setnames(final_table, gsub(pattern = "GLM ", replacement = "", names(final_table)))
  
  round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- names(x)[sapply(x, mode) == 'numeric']
    print(numeric_columns)
   
    print(x[ , ..numeric_columns])
    
    x[ , (numeric_columns) := round(.SD, digits), .SDcols = numeric_columns]
    x
  }
  
  final_table <- round_df(final_table, 3)
  
  
  
  print(final_table$Description)
  
  # re-order rows
  if(lcc_type == "NCV") {
    
    row_order <- c(
      "BASE v1.0",
      "Omit FWI",
      "Omit HDI",
      "Omit Treecover_Gridcell", 
      "Omit FAPAR12",
      "Omit MEPI",
      "Omit PopDens",
      "Omit Slope",
      "Omit TPI",
      "FWI not logged",
      "MEPI and FWI not interacting",
      "Pop dens quadratic",
      "Treecover not quadratic",
      "Swap GPP12 for FAPAR12",
      "Include wind", 
      "Include NCV fraction"
    )
    
    for(row_index in 2:nrow(final_table)){
      for(col_index in 2:ncol(final_table)){
        final_table[ row_index, col_index] <- final_table[ row_index, ..col_index ] - final_table[ 1, ..col_index] 
      }
    }
    
    
    final_table <- final_table[match(row_order, final_table$Description),]
    write.table(x = final_table, file = file.path(plot_dir, "Table2_NCV_metrics.csv"), row.names = FALSE)
    
  }
  
  # re-order rows
  else if(lcc_type == "PureCropland") {
    
    row_order <- c(
      "BASE v1.0",
      "Omit FWI",
      "Omit HDI",
      "Omit GPP12",
      "Omit GPP3_index",
      "Omit GPP",
      "Omit PopDens",
      "Omit Slope",
      "Include TPI",
      "FWI not quadratic",
      "GPP GPP3_index not interacting",
      "GPP12 not quadratic"
    )
    
    for(row_index in 2:nrow(final_table)){
      for(col_index in 2:ncol(final_table)){
        final_table[ row_index, col_index] <- final_table[ row_index, ..col_index ] - final_table[ 1, ..col_index] 
      }
    }
    
    final_table <- final_table[match(row_order, final_table$Description),]
    write.table(x = final_table, file = file.path(plot_dir, "Table3_Cropland_metrics.csv"), row.names = FALSE)
    
  }
  
  
  
  
}
