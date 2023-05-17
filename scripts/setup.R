rm(list = ls())
if (length((.packages())) > 7){
  invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)),
                   detach,
                   character.only = TRUE, unload = TRUE))}

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
           "ggplot2")

lapply(pckgs, require, character.only = TRUE)