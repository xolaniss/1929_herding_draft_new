# Description
# Descriptives for 1929 paper by Xolani Sibande 2nd November 2022

# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
results <- read_rds(here("Outputs", "artifacts_csad_cssd.rds"))

combined_results_list <- list(
  "All industries" = results$csad_crisis$results_all_industries_crisis_csad_tbl,
  "Consumables" = results$csad_crisis$results_consumables_group_crisis_csad_tbl,
  # "Durables group" = results$data$results_durables_group_tbl,
  "Health"  = results$csad_crisis$results_health_group_crisis_csad_tbl,
  "Manufacturing"  = results$csad_crisis$results_manuf_group_crisis_csad_tbl,
  "Other" = results$csad_crisis$results_mines_group_crisis_csad_tbl,
  "Business services" = results$csad_crisis$results_bus_group_crisis_csad_tbl
)

# Cleaning -----------------------------------------------------------------
combined_results_tbl <- 
  combined_results_list %>% 
  map(~rename(., "Market Return" = "Mkt")) %>% 
  bind_rows(.id = "Category") %>% 
  relocate(Date, .before = "Category")

# Descriptives -------------------------------------------------------------

descriptives_full_tbl <- 
  combined_results_tbl %>% 
  dplyr::select(-Crisis) %>% 
  drop_na() %>% 
  pivot_longer(cols = -c(Date, Category), names_to = "Variables", values_to = "Value") %>% 
  group_by(Category, Variables) %>% 
  summarise(across(.cols = -c(Date),
                   .fns = list(Median = median, 
                               SD = sd,
                               Min = min,
                               Max = max,
                               IQR = IQR,
                               Obs = ~ n()), 
                   .names = "{.fn}")) %>% 
  mutate(Crisis = "Full Sample")


descriptives_crisis_tbl <- 
  combined_results_tbl %>% 
  drop_na() %>% 
  pivot_longer(cols = -c(Date, Category, Crisis), names_to = "Variables", values_to = "Value") %>% 
  group_by(Category, Crisis, Variables) %>% 
  summarise(across(.cols = -c(Date),
                   .fns = list(Median = median, 
                               SD = sd,
                               Min = min,
                               Max = max,
                               IQR = IQR,
                               Obs = ~ n()), 
                   .names = "{.fn}")) %>% 
  filter(!Crisis == "No Crisis")

descriptives_tbl <- 
  rbind(descriptives_full_tbl, descriptives_crisis_tbl) %>% 
  arrange(Category)  %>% 
  relocate(Crisis, .before = Variables)

  

# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  combined_results_list = combined_results_list,
  combined_results_tbl = combined_results_tbl,
  descriptives_tbl = descriptives_tbl
)


write_rds(artifacts_descriptives, file = here("Outputs", "artifacts_descriptives.rds"))


