# NOTE: For this script we are reading a table of the SHAP-derived variable important valuesthat we saved when we fitted the model.
# This is because the vi() function used to get the variable importance doesn't seem to work well if it the fitted model object is 
# no longer in it's original environment

library(ggplot2)



