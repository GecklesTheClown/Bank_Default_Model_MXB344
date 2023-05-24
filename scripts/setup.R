## Refresh R
rm(list = ls())
if (length((.packages())) > 7){
  invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),
                   detach,
                   character.only = TRUE, unload = TRUE))}

# List of Required Packages

pckgs <- c("tidyverse",
           "plotly",
           "broom", 
           "readr", 
           "MASS", 
           "ggpubr", 
           "AER", 
           "DHARMa", 
           "jtools", 
           "broom.mixed", 
           "MASS", 
           "reshape2",
           "GGally",
           "dplyr",
           "here",
           "gmodels",
           "ROCR",
           "glmm",
           "pROC",
           "parallel",
           "boot",
           "lattice",
           "grid",
           "png"
           "lme4")



# Install Missing packages
new.pckgs <- pckgs[!(pckgs %in% installed.packages()[,"Package"])]
if(length(new.pckgs)>0) 
  install.packages(new.pckgs)

# Library packages
lapply(pckgs, require, character.only = TRUE)


