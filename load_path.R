# ==========================================================================================================================================
## Script Name: Load Path
## Purpose: Assigns directories and functions needed for IBLS project
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

rm(list=ls())

#directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
LongDataDir <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Latest Longitudinal Survey Data_May 2025")
CSDataDir <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Field data/HF_data")
CSMenDataDirDry <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data/Kano/Dry Season Data/Wide Data")
CSMenDataDirWet <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data/Kano/Wet Season Data/Wide Data")
CSMenDataDirWetIbadan <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data/Ibadan/Wet Season Data/Wide Data")
CSMenDataDirDryIbadan <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data/Ibadan/Dry Season Data")
KidsData <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/new_field_data/mrpt_analysis data/")
FigDir <- file.path(DriveDir, "projects/ChatMRPT/IBLS/Figures")
OutputDir <- file.path(DriveDir, "projects/ChatMRPT/IBLS/cleaned")

library(readstata13)

#load packages


# list_of_packages <- c("RColorBrewer", "readr", "haven", "data.table", "reshape",
#                       "ggplot2", "labelled", "tidyverse", "janitor", "terra",
#                       "readxl", "mapsf", "survey","srvyr", "plotly", "hdf5r",
#                       "broom", "ggthemes", "ggrepel", "sjlabelled", "sf", "ggpubr", "viridis", "patchwork", 
#                       "raster", "wordcloud", "ggwordcloud", "terra", "plotly",
#                       "gridExtra", "grid", "openxlsx", "officer", "magrittr", "mclust",
#                       "foot", "units", "tidyr", "foreach", "doParallel", "future.apply", "dplyr",
#                       "stringr", "purrr", "stars")
# 
# read_install_pacakges <- function(packages = list_of_packages
# ){
#   new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
#   if(length(new.packages)) install.packages(new_packages)
#   return(sapply(list_of_packages, require, character.only = TRUE))
# }
# 
# read_install_pacakges()

# was getting error about new_packages being empty, here is new code that avoids it:
list_of_packages <- c(
  "RColorBrewer", "readr", "haven", "data.table", "reshape",
  "ggplot2", "labelled", "tidyverse", "janitor", "terra",
  "readxl", "mapsf", "survey", "srvyr", "plotly", "hdf5r",
  "broom", "ggthemes", "ggrepel", "sjlabelled", "sf", "ggpubr", "viridis", "patchwork",
  "raster", "wordcloud", "ggwordcloud", "plotly", "gridExtra", "grid",
  "openxlsx", "officer", "magrittr", "mclust", "foot", "units", "tidyr",
  "foreach", "doParallel", "future.apply", "dplyr", "stringr", "purrr", "stars", "tictoc", "readstata13"
)

read_install_packages <- function(packages = list_of_packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  
  # Install new packages if any
  if (length(new_packages) > 0) {
    install.packages(new_packages)
  } else {
    message("All packages are already installed.")
  }
  
  # Load all required packages
  return(sapply(packages, require, character.only = TRUE))
}

# Run the function
read_install_packages()

#custom functions

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=8, colour = 'black', hjust = 0.5), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}


get_model_results <- function(data) {
  #survey design
  design <- svydesign(
    id = ~sn + ea,
    strata = ~Ward + settlement_type,
    weights = ~ind_weight,
    data = data,
    nest = TRUE
  )
  #adjusted
  adjusted_model <- svyglm(malaria_positive ~ net_own + net_use3, family = "binomial", design = design)
  adjusted_results <- broom::tidy(adjusted_model) %>%
    mutate(
      oddsratio = round(exp(estimate), 3),
      ci_low = round(exp(estimate - (1.96 * std.error)), 3),
      ci_high = round(exp(estimate + (1.96 * std.error)), 3),
      model = "adjusted"
    )
  #unadjusted: net_own only
  unadjusted_net_own <- svyglm(malaria_positive ~ net_own, family = "binomial", design = design)
  unadjusted_net_own_results <- broom::tidy(unadjusted_net_own) %>%
    mutate(
      oddsratio = round(exp(estimate), 3),
      ci_low = round(exp(estimate - (1.96 * std.error)), 3),
      ci_high = round(exp(estimate + (1.96 * std.error)), 3),
      model = "unadjusted_net_own"
    )
  # unadjusted: net_use only
  unadjusted_net_use <- svyglm(malaria_positive ~ net_use3, family = "binomial", design = design)
  unadjusted_net_use_results <- broom::tidy(unadjusted_net_use) %>%
    mutate(
      oddsratio = round(exp(estimate), 3),
      ci_low = round(exp(estimate - (1.96 * std.error)), 3),
      ci_high = round(exp(estimate + (1.96 * std.error)), 3),
      model = "unadjusted_net_use"
    )
  bind_rows(adjusted_results, unadjusted_net_own_results, unadjusted_net_use_results)
}


#source("~/NMEP_classification/15_extraction_function.R", echo = T)
#source("~/NMEP_classification/pop_estimate_function.R", echo = T)