#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)
library(tictoc)
library(gt)

# define root path with here package and 
here::i_am("scripts/tabulate_metrics.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))
source(here("scripts", "glm_fitting_helper_functions.R"))


#### 


#  Versions and directories for reading data and saving plots
all_model_types <- list(list(name = "NCV",
                             baseline_model = "BASE v1.0",
                             row_subset_order =  c(
                               "BASE v1.0",
                               "Omit FWI",
                               "Omit HDI",
                               "Omit Treecover_Gridcell", 
                               "Omit FAPAR12",
                               "Omit MEPI",
                               "Omit PopDens",
                               "Omit Slope",
                               "Omit TPI",
                               "Include wind speed", 
                               "Include NCV fraction",
                               "FWI not logged",
                               "MEPI and FWI not interacting",
                               "Pop dens quadratic",
                               "Treecover not quadratic",
                               "Swap GPP12 for FAPAR12",
                               "Swap GDP for HDI",
                               "Include HDI x GDP",
                               "Include HDI x PopDens",
                               "Include PopDens x GDP"
                             ),
                             tab_num = 2), 
                        
                        list(name = "PureCropland",
                             baseline_model = "BASE v1.0",
                             row_subset_order =  c(
                               "BASE v1.0",
                               "Omit FWI",
                               "Omit HDI",
                               "Omit GPP12",
                               "Omit GPP3_index",
                               "Omit MEPI",
                               "Omit PopDens",
                               "Omit Slope",
                               "Omit wind speed",
                               "Include TPI",
                               "Wind speed not quadratic",
                               "FWI not quadratic",
                               "GPP12 not quadratic",
                               "MEPI GPP3_index not interacting",
                               "Swap GPP6_index for GPP3_index",
                               "Swap GPP for MEPI",
                               "Swap GDP for HDI",
                               "Include HDI x GDP",
                               "Include HDI x PopDens",
                               "Include PopDens x GDP"),
                             tab_num = 3) 
)


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

dev_col <- "Deviance explained"
descr_col <- "Description"
pattern_fit_metrics <- c("Spatial NME", "MPD", "IAV NME")

target <- "BurntFraction"
version <- "BASE_v1.0"
plot_dir <- here("plots/manuscript_BASE_v1.0")



# handy function for rounding numerics columns only
round_df <- function(x, digits) {
  numeric_columns <- names(x)[sapply(x, mode) == 'numeric']
  x[ , (numeric_columns) := round(.SD, digits), .SDcols = numeric_columns]
  return(x)
}

for(lcc_type in all_model_types) {
  
  # directory where runs are stored
  runs_dir <- here("results", "GLMs",  version, paste(target, lcc_type$name, sep = "_"))
  
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
  
  
  # select columns and tidy names
  final_table <- all_metrics_dt[ , ..cols]
  setnames(final_table, gsub(pattern = "GLM ", replacement = "", names(final_table)))
  
  # find baseline row
  baseline_row_index <- (1:nrow(final_table))[which(final_table$Description == baseline_models[[lcc_type$name]])]
  
  # make a relative table
  relative_table <- copy(final_table)
  for(row_index in 1:nrow(relative_table)){
    if(row_index != baseline_row_index){
      for(col_index in 2:ncol(relative_table)){
        relative_table[ row_index, col_index] <- relative_table[ row_index, ..col_index ] - relative_table[ baseline_row_index, ..col_index] 
      }
    }
  }
  final_table <- round_df(final_table, 5)
  relative_table <- round_df(relative_table, 5)
  
  # write the full tables before subsetting and reordering rows 
  write.table(x = final_table, file = file.path(runs_dir, paste(target, lcc_type$name, "all_model_metrics_absolute.csv", sep = "_")), row.names = FALSE)
  write.table(x = relative_table, file = file.path(runs_dir,  paste(target, lcc_type$name, "all_model_metrics_relative.csv", sep = "_")), row.names = FALSE)
  
  
  #write.csv(x = relative_table, file = file.path(plot_dir, paste("Table3", lcc_type$name, "metrics.csv", )), row.names = FALSE)
  
  # make final subselection and pretty for publication using gt package
  relative_table <- relative_table[match(lcc_type$row_subset_order, relative_table$Description),]
  
  # round to the decimal places
  relative_table <- as.data.table(round_df(relative_table, 3))
  
  # move the NULL mode values to the names, also keep the final names
  final_col_names <- c(descr_col, dev_col)
  for(this_col in pattern_fit_metrics){
    
    this_null_name <- paste("Null", this_col)
    final_col_name <- paste0(this_col, " (", relative_table[1, ..this_null_name],  ")")
    setnames(relative_table, this_col, final_col_name)
    relative_table[ , (this_null_name) := NULL]
    final_col_names <- append(final_col_names, final_col_name)
  }
  
  # select final columns
  relative_table_final <- relative_table[ , .SD, .SDcols = final_col_names]
  relative_table_gt <- gt(relative_table_final)
  
  
  
  relative_table_gt <- data_color(relative_table_gt, 
                                  rows = 2:length(row_order), 
                                  columns = dev_col,
                                  method = "bin", 
                                  bins = c(-Inf, -0.005, 0.005, Inf), 
                                  palette = c("red4", "white", "blue4"))
  
  relative_table_gt <- data_color(relative_table_gt, 
                                  rows = 2:length(row_order), 
                                  columns =  names(relative_table_final)[-which(names(relative_table_final) %in% c(descr_col, dev_col))],
                                  method = "bin",
                                  bins = c(-Inf, -0.005, 0.005, Inf),
                                  palette = c("blue4", "white", "red4"))
  
  relative_table_gt <- tab_style(relative_table_gt,
                                 style = list(cell_text(weight = "bold")),
                                 locations = cells_body(rows = 1)
  )
  
  
  
  print(relative_table_gt)
  gtsave(relative_table_gt, file = file.path(plot_dir, paste(paste0("Table", lcc_type$tab_num),  lcc_type$name ,"metrics.docx", sep = "_")))
  
}
