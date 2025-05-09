#############Loading Library#####################
library(fda)
library(openxlsx)
library(doParallel)
library(plyr)
library(tidyr)
library(ggplot2)

#############Loading utilities#####################
source("R/utilities.R")

########Loading Parallel#####################
source("R/Parallel.R")
#############Loading Data#####################
source("R/Data_functions.R")

#############Loading Functions#####################

# Fmax function
source("R/Fmax.R")

# SPM function
source("R/SPM.R")

##################Loading plot functions#####################
source("R/Plot_functions.R")