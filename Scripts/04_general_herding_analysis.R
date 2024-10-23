# Description
# General and extreme market herding analysis by Xolani Sibande - 1 October 2023

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
library(quantreg)

options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_results_tbl <- result_csad_cssd$combined_results_tbl

# OLS ------------------------------------------------------
## OLS -------------------------------------------------------------------
formula <-  as.formula(CSAD ~ abs(`Market Return`) + I(`Market Return` ^ 2))

ols_full_tbl <- 
  combined_results_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "Full market") %>% 
  ungroup()

ols_tbl <- ols_full_tbl %>%
  arrange(Category)

## OLS extreme market ----------------------------------------------------

### Top market --------------------------------------------------------
combined_top_results_tbl <- 
  combined_results_tbl %>% 
  group_by(Category) %>% 
  slice_max(order_by = `Market Return`, prop = 0.05) 

ols_max_tbl <- 
  combined_top_results_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "Top market (5% market returns)") %>% 
  ungroup()

### Bottom market --------------------------------------------------------
combined_bottom_results_tbl <- 
  combined_results_tbl %>% 
  group_by(Category) %>% 
  slice_min(order_by = `Market Return`, prop = 0.05)

ols_min_tbl <- 
  combined_bottom_results_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>%
  mutate(Extreme = "Bottom market (5% market returns)") %>% 
  ungroup()

## Combined --------------------------------------------------------------
ols_combined_tbl <- rbind(ols_full_tbl, ols_max_tbl, ols_min_tbl)

##  Rolling regressions ----------------------------------------------------
models_rol <-
  combined_results_tbl %>%
  dplyr::select(-Crisis) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  ols_slidify_models_standard() %>% 
  unnest_rol_col_standard(rol_column = models) 

## Graphing ---------------------------------------------------------------
rol_coeff_gg <-
  models_rol %>%
  dplyr::select(- starts_with("t")) %>% 
  slidyfy_gg_workflow_standard() +
  theme(
    legend.position = "none")

rol_tstats_gg <-
  models_rol %>%
  dplyr::select(- starts_with("a")) %>% 
  slidyfy_gg_workflow_standard() +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 
rol_gg <- rol_coeff_gg / rol_tstats_gg

# Export ---------------------------------------------------------------
artifacts_general_herding <- list (
  data = list(
    combined_results_tbl = combined_results_tbl
  ),
  models = list(
    ols_combined_tbl = ols_combined_tbl,
    models_rol = models_rol
  ),
  graphs = list(
    rol_gg = rol_gg
  )
)

write_rds(artifacts_general_herding, file = here("Outputs", "artifacts_general_herding.rds"))


