# Data needs to be downloaded and locally stored first. We download from the ASE
# private repository (owned by F.H.). Alternatively, files can be directly 
# downloaded from the repository, but access to it must be granted by the owner 
# - and the code should be modified accordingly.

# useful package to manage packages
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

# install packages if needed and load them
pacman::p_load(tidyverse, dplyr)

# local working directory path: change as needed
setwd("C:\\Users\\soffi\\Desktop\\CONSULTING\\ASE-main\\")

# get table names
data_files <- list.files(pattern = "\\.CSV$", full.names = T, recursive = T)
data_files

# a list of TS and a list of non-TS files should be obtained

## DATA WRANGLING STEPS:

# first, TS files should be pivoted longer (take colnames from "tables overview")

# then, a "Year" column with value="2022" should be added to all non-TS files

# finally, make sure all colnames are unique 


