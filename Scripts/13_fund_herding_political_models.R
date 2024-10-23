# Description
# Fundamental versus non fundamental herding Xolani Sibande - 16 October 2024
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

options(scipen = 999)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
cross_sectionals <- read_rds(here("Outputs", "artifacts_general_herding.rds"))
CSAD_crisis_tbl <- cross_sectionals$data$combined_results_tbl

fama_french <- read_rds(here("Outputs", "artifacts_fama_french.rds"))
fama_french_tbl <- fama_french$data$fama_french_tbl

ratings <- read_rds(here("Outputs", "artifacts_presidential_ratings.rds")) %>% pluck(1)
par_tbl <- ratings$par_daily_tbl
pear_tbl <- ratings$pear_daily_tbl

# Combining -----------------------------------------------------------------
combined_tbl <- 
  CSAD_crisis_tbl %>% 
  left_join(
    fama_french_tbl,
    by = c("Date" = "Date")
  ) 

# EDA ---------------------------------------------------------------
combined_tbl %>% skim()

# Fundamental versus non fundamental equations --------------------------------
formula <- as.formula(CSAD ~ `Mkt-RF`+ RF + SMB + HML)
residuals_tbl <-
  combined_tbl %>%
  ols_nest_full_prep() %>% 
  mutate(models = map(data, ~ lm(formula, data = .))) %>% 
  mutate(models_residuals = map(models, ~ resid(.))) %>% 
  unnest(cols = models_residuals, names_repair = "universal") %>% 
  ungroup() %>%
  dplyr::select(Category, models_residuals) %>% 
  mutate(Date = combined_tbl$Date) %>%
  relocate(Date, .before = models_residuals)

# Re-combining ------------------------------------------------------------
combined_fundamental_tbl <-
  combined_tbl %>%
  left_join(residuals_tbl, by = c("Date" = "Date", "Category" = "Category")) %>%
  rename("CSAD_nonfund" = models_residuals) %>%
  mutate("CSAD_fund" = CSAD - CSAD_nonfund) %>% 
  left_join(
    par_tbl,
    by = c("Date" = "Date")
  ) %>% 
  left_join(
    pear_tbl,
    by = c("Date" = "Date")
  )

# PAR ---------------------------------------------------------------------
## Fundamental -------------------------------------------------------------------
formula <-  as.formula(CSAD_fund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         PAR_high_dummy:I(`Market Return` ^ 2) +
                         PAR_low_dummy:I(`Market Return` ^ 2))
par_ols_full_fundamental_tbl <-
  combined_fundamental_tbl %>%
  dplyr::select(-Crisis) %>%
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Fundamental") %>% 
  ungroup()

par_ols_full_fundamental_tbl

## Non Fundamental -------------------------------------------------------
formula <-  as.formula(CSAD_nonfund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         PAR_high_dummy:I(`Market Return` ^ 2) +
                         PAR_low_dummy:I(`Market Return` ^ 2))
par_ols_full_nonfundamental_tbl <- 
  combined_fundamental_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Non Fundamental") %>% 
  ungroup() 

par_ols_full_nonfundamental_tbl


# PEAR ---------------------------------------------------------------------
## Fundamental -------------------------------------------------------------------
formula <-  as.formula(CSAD_fund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         PEAR_high_dummy:I(`Market Return` ^ 2) +
                         PEAR_low_dummy:I(`Market Return` ^ 2))
pear_ols_full_fundamental_tbl <-
  combined_fundamental_tbl %>%
  dplyr::select(-Crisis) %>%
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Fundamental") %>% 
  ungroup()

pear_ols_full_fundamental_tbl

## Non Fundamental -------------------------------------------------------
formula <-  as.formula(CSAD_nonfund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         PEAR_high_dummy:I(`Market Return` ^ 2) +
                         PEAR_low_dummy:I(`Market Return` ^ 2))
pear_ols_full_nonfundamental_tbl <- 
  combined_fundamental_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Non Fundamental") %>% 
  ungroup() 

pear_ols_full_nonfundamental_tbl


## Combined ----------------------------------------------------------------
par_ols_fund_tbl <- rbind(par_ols_full_fundamental_tbl, par_ols_full_nonfundamental_tbl)
pear_ols_fund_tbl <- rbind(pear_ols_full_fundamental_tbl, pear_ols_full_nonfundamental_tbl)

# Exporting -------------------------------------------------------------
artifacts_alternative_ratings_models <- list(
  models = list(
    par_ols_fund_tbl = par_ols_fund_tbl,
    pear_ols_fund_tbl = pear_ols_fund_tbl
  )
)

write_rds(artifacts_alternative_ratings_models, file = here("Outputs", "artifacts_alternative_ratings_models.rds"))
