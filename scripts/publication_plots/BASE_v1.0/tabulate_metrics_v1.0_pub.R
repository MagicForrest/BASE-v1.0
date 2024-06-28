#### PREAMBLE ####
library(ggplot2)
library(data.table)
library(viridis)
library(sf)
library(tictoc)
library(gt)

# define root path with here package and 
here::i_am("scripts/publication_plots/BASE_v1.0/tabulate_metrics_v1.0_pub.R")
library(here)
source(here("scripts", "plot_utils.R"))
source(here("scripts", "plot_helper_functions.R"))
source(here("scripts", "glm_fitting_helper_functions.R"))


#### 
target <- "BurntFraction"
analysis_version <- "BASE_v1.0"
# fit version
fit_batch_version <- "BASE_v1.0"

plot_dir <- here("plots", analysis_version, "manuscript")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# threshold


#  Versions and directories for reading data and saving plots
all_model_types <- list(list(name = "NCV",
                             baseline_model = "BASE v1.0",
                             row_subset_order =  c(
                               "BASE v1.0",
                               "Omit FWI",
                               "Omit HDI",
                               "Omit Treecover", 
                               "Omit FAPAR12",
                               "Omit MEPI",
                               "Omit Pop_dens",
                               "Omit Slope",
                               "Omit TPI",
                               "Include wind speed", 
                               "FWI not logged",
                               "MEPI and FWI not interacting",
                               "Pop dens quadratic",
                               "MEPI quadratic",
                               "Treecover not quadratic",
                               "Replace FAPAR12 with GPP12",
                               "Include HDI x Pop_dens",
                               "Replace HDI with GDP",
                               "Replace HDI with Pop_dens x GDP"
                             ),
                             tab_num = 2), 
                        
                        list(name = "PureCropland",
                             baseline_model = "BASE v1.0",
                             row_subset_order =  c(
                               "BASE v1.0",
                               "Omit FWI",
                               "Omit GDP",
                               "Omit GPP12",
                               "Omit PHI",
                               "Omit MEPI",
                               "Omit Pop_dens",
                               "Omit Slope",
                               "Omit wind speed",
                               "Include TPI",
                               "FWI not quadratic",
                               "GPP12 not quadratic",
                               "MEPI not quadratic",
                               "PHI quadratic",
                               "MEPI PHI interacting",
                               "Replace MEPI with GPP",
                               "Include GDP x Pop_dens",
                               "Replace GDP with HDI",
                               "Replace GDP with Pop_dens x HDI"),
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



# handy function for rounding numerics columns only
round_df <- function(x, digits) {
  numeric_columns <- names(x)[sapply(x, mode) == 'numeric']
  x[ , (numeric_columns) := round(.SD, digits), .SDcols = numeric_columns]
  return(x)
}

for(lcc_type in all_model_types) {
  
  # directory where runs are stored
  runs_dir <- here("fitted_models",  analysis_version,  "GLMs", paste(target, lcc_type$name, sep = "_"), fit_batch_version)
  
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
  baseline_row_index <- (1:nrow(final_table))[which(final_table$Description == lcc_type$baseline_model)]
  
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
  
  
  
  # colour deviance explained text
  relative_table_gt <- tab_style(relative_table_gt, 
                                 locations = cells_body(rows = get(dev_col) >= 0.005,
                                                        columns = dev_col),
                                 style = cell_text(color = "blue4", weight = "bold"))
  relative_table_gt <- tab_style(relative_table_gt, 
                                 locations = cells_body(rows = get(dev_col) <= -0.005,
                                                        columns = dev_col),
                                 style = cell_text(color = "red4", weight = "bold"))
  
  # colour other metric text (opposite way)
  for(this_metric in final_col_names[3:length(final_col_names)]) {
    relative_table_gt <- tab_style(relative_table_gt, 
                                   locations = cells_body(rows = get(this_metric) >= 0.005,
                                                          columns = this_metric),
                                   style = cell_text(color = "red4", weight = "bold"))
    relative_table_gt <- tab_style(relative_table_gt, 
                                   locations = cells_body(rows = get(this_metric) <= -0.005,
                                                          columns = this_metric),
                                   style = cell_text(color = "blue4", weight = "bold"))
  }
  
   # ste main model black and bold
  relative_table_gt <- tab_style(relative_table_gt,
                                 style = list(cell_text(weight = "bold", color = "black", style = "normal")),
                                 locations = cells_body(rows = 1)
  )
  
  
  
  print(relative_table_gt)
  gtsave(relative_table_gt, file = file.path(plot_dir, paste(paste0("Table_0", lcc_type$tab_num),  lcc_type$name ,"metrics.docx", sep = "_")))
  
}
