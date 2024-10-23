# Description
# models with PAR and PEAR data - Xolani Sibande July 2024
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
library(quantmod)

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
library(midasr)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))
clean_combine <- function(data, filter_date, join_data, join_var){
  data %>% 
    dplyr::select(
      -a0, -a1, -starts_with("t-statistic")
    ) %>% 
    pivot_wider(names_from = Category, values_from = a2) %>% 
    filter(Date >= ymd({{ filter_date }})) %>% 
    left_join(join_data, by = "Date") %>% 
    pivot_longer(
      cols = -c(Date, join_var),
      names_to = "Category",
      values_to = "a2"
    ) %>% 
    relocate({{ join_var }}, .after = a2)
}

# Import -------------------------------------------------------------
par <- read_rds(here("Outputs", "artifacts_presidential_ratings.rds"))
par_daily_tbl <- par$data$par_daily_tbl %>% dplyr::select(Date, PAR)
pear_daily_tbl <- par$data$pear_daily_tbl %>% dplyr::select(Date, PEAR)

general_herding <- read_rds(here("Outputs", "artifacts_general_herding.rds"))
general_herding_tbl <- general_herding$models$models_rol

fundamental_herding <- read_rds(here("Outputs", "artifacts_fundamental_herding.rds"))
fundamental_herding_tbl <- fundamental_herding$ols_rol$ols_full_fundamental_rol_tbl
non_fundamental_herding_tbl <- fundamental_herding$ols_rol$ols_full_nonfundamental_rol_tbl

# PAR ols -----------------------------------------------------------------

## General herding --------------------------------------------------------
par_general_ols_tbl <- 
  general_herding_tbl %>% 
  clean_combine(
    filter_date = "1941-07-01",
    join_data = par_daily_tbl,
    join_var = "PAR")

formula <-  as.formula(a2 ~ PAR)

ols_full_tbl <- 
  par_general_ols_tbl %>% 
  filter(a2 <= 0) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "General Herding") %>% 
  ungroup()

## Fundamental herding --------------------------------------------------------
par_fundamental_ols_tbl <- 
  fundamental_herding_tbl %>% 
  clean_combine(
    filter_date = "1941-07-01",
    join_data = par_daily_tbl,
    join_var = "PAR")

ols_full_fundamental_tbl <- 
  par_fundamental_ols_tbl %>% 
  filter(a2 <= 0) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "Fundamental Herding") %>% 
  ungroup()

## Non-fundamental herding --------------------------------------------------------
par_non_fundamental_ols_tbl <- 
  non_fundamental_herding_tbl %>% 
  clean_combine(
    filter_date = "1941-07-01",
    join_data = par_daily_tbl,
    join_var = "PAR"
    ) 

ols_full_non_fundamental_tbl <- 
  par_non_fundamental_ols_tbl %>% 
  filter(a2 <= 0) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "Non-fundamental Herding") %>% 
  ungroup()

## Combine --------------------------------------------------------
par_ols_tbl <- 
  bind_rows(
    ols_full_tbl,
    ols_full_fundamental_tbl,
    ols_full_non_fundamental_tbl
  )

# PEAR ols -----------------------------------------------------------------

##  General herding --------------------------------------------------------
pear_general_ols_tbl <- 
  general_herding_tbl %>% 
  clean_combine(
    filter_date = "1981-04-01",
    join_data = pear_daily_tbl,
    join_var = "PEAR") 

formula <-  as.formula(a2 ~ PEAR)

ols_full_tbl <- 
  pear_general_ols_tbl %>% 
  filter(a2 <= 0) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "General Herding") %>% 
  ungroup()

## Fundamental herding --------------------------------------------------------
pear_fundamental_ols_tbl <- 
  fundamental_herding_tbl %>% 
  clean_combine(
    filter_date = "1981-04-01",
    join_data = pear_daily_tbl,
    join_var = "PEAR")

ols_full_fundamental_tbl <-
  pear_fundamental_ols_tbl %>% 
  filter(a2 <= 0) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "Fundamental Herding") %>% 
  ungroup()

## Non-fundamental herding --------------------------------------------------------
pear_non_fundamental_ols_tbl <- 
  non_fundamental_herding_tbl %>% 
  clean_combine(
    filter_date = "1981-04-01",
    join_data = pear_daily_tbl,
    join_var = "PEAR"
  ) 

ols_full_non_fundamental_tbl <-
  pear_non_fundamental_ols_tbl %>% 
  filter(a2 <= 0) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Extreme = "Non-fundamental Herding") %>% 
  ungroup()

## Combine --------------------------------------------------------
pear_ols_tbl <- 
  bind_rows(
    ols_full_tbl,
    ols_full_fundamental_tbl,
    ols_full_non_fundamental_tbl
  )

# Export ---------------------------------------------------------------
artifacts_par_models <- list (
  data = list(
    par_general_ols_tbl = par_general_ols_tbl,
    par_fundamental_ols_tbl = par_fundamental_ols_tbl,
    par_non_fundamental_ols_tbl = par_non_fundamental_ols_tbl,
    pear_general_ols_tbl = pear_general_ols_tbl,
    pear_fundamental_ols_tbl = pear_fundamental_ols_tbl,
    pear_non_fundamental_ols_tbl = pear_non_fundamental_ols_tbl
  ),
  models = list(
    par_ols_tbl = par_ols_tbl,
    pear_ols_tbl = pear_ols_tbl
  )
)

write_rds(artifacts_par_models, file = here("Outputs", "artifacts_par_models.rds"))
  

