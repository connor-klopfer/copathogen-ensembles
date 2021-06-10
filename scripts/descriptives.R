#' Scripts for descriptive statisitcs and tables. 

library(dplyr)
library(ggplot2)

library(copathogenTools)

# source(file.path("utils", "import_all.R"), echo = F, chdir = T)

# Final color choice, light blue and dark green, high saturation. 
two_var_coloring <- c("#1f78b4", "#67D067")

complete_data <- import_complete_datasets(bangladesh_only = T, parent_dir = "data/")
