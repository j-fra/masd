{
    rm(list = ls())
    
    library(tidyverse)
    library(magrittr)
    
    load("results/es_prepared2.Rda")
    
    walk(list.files("code/functions/", full.names = T), source)
}

# This R script contains code to run all other R scripts as "jobs" in RStudio (see https://blog.rstudio.com/2019/03/14/rstudio-1-2-jobs/). 
# This way, scripts can be run in parallel in separate R sessions, which speeds up development and avoids dependency issues with 
# incorrectly loaded packages or objects floating around in the environment. Running the scripts that way is not strictly necessary though.

# Get data.
rstudioapi::jobRunScript("code/01_compute_es.R", workingDir = getwd())

# Clean and prepare.
rstudioapi::jobRunScript("code/02_prepare_df.R", workingDir = getwd())

# Analyze. 
rstudioapi::jobRunScript("code/03_all_items.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_correlation_analysis.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_distribution_overlap.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_inventory_overview.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_figure.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_funnels.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_funnels_control.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_funnels_secondary.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_histograms.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_leave1out.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_main_analysis_table.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_moderation_analysis.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_moderation_analysis_secondary.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_moderator_overview.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_moderator_overview_secondary.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_pubbias_3PSM.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_search_terms_item_overview.R", workingDir = getwd())
rstudioapi::jobRunScript("code/03_theory_figure.R", workingDir = getwd())

# Collect results.
rstudioapi::jobRunScript("code/04_retrieve_results.R", workingDir = getwd())
rstudioapi::jobRunScript("code/04_moderation_plots.R", workingDir = getwd())
