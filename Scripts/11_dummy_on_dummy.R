# Description
# Dummy on dummy regressions - November 2023 Xolani Sibande
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
library(magrittr)
library(stringi)

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
library(mfx)
# library(margins)

options(scipen = 999)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_dummy_dummy.R"))
probit_marginal_effect <- 
  function(data, category) {
   models <-  probitmfx(formula, 
              data =  data %>%  filter(Category == category),
              robust = TRUE,
              atmean = FALSE) 
    coefs <- models$mfxest[, 1] %>% bind_rows()
    pvalues <- models$mfxest[, 4] %>% bind_rows()
    bind_cols(coefs, pvalues) 
  }

group_probit_marginal_effect <- 
  function(data, group_vec = industry, group_type = "General Herding") {
    group_vec %>% 
      map(~probit_marginal_effect(data = data,
                                  .x)) %>% 
      bind_rows() %>% 
      mutate(
        stars_party = ifelse(party_dummy...3 < 0.001, "***",
                             ifelse(party_dummy...3  < 0.01, "**", 
                                    ifelse(party_dummy...3  < 0.05, "*", "")))
      ) %>% 
      mutate(
        stars_volatility = ifelse(volatility...4 < 0.001, "***",
                                  ifelse(volatility...4  < 0.01, "**", 
                                         ifelse(volatility...4  < 0.05, "*", "")))
      ) %>% 
      mutate(across(1:2, ~strtrim(., 6))) %>% 
      mutate(Party = paste0(party_dummy...1, stars_party)) %>% 
      mutate(Volatility = paste0(volatility...2, stars_volatility)) %>% 
      dplyr::select(Party, Volatility) %>% 
      mutate(
        Category = group_vec %>% transpose() %>% unlist(),
        Group = group_type
      ) %>% 
      relocate(Category, .before = Party)
  }

# Importing -------------------------------------------------------------
presidential <- read_rds(here("Outputs", "artifacts_presidential_terms.rds"))
general_herding <- read_rds(here("Outputs", "artifacts_general_herding.rds"))
# herding_crisis <- read_rds(here("Outputs", "artifacts_herding_crisis.rds"))
fundamental_herding <- read_rds(here("Outputs", "artifacts_fundamental_herding.rds"))
# fundamental_herding_crisis <- read_rds(here("Outputs", "artifacts_crisis_fundamental.rds"))
volatility <- read_rds(here("Outputs", "artifacts_volatility.rds"))

# Cleaning -----------------------------------------------------------------
presidential_tbl <- 
  presidential$terms_daily_tbl %>% 
  filter(Date >= "1920-01-01") %>% 
  rename(
    "party_dummy" = "Dummy"
  )
presidential_tbl

volatility_tbl <- 
  volatility$data$volatility_tbl %>% 
  filter(Date >= "1920-01-01") %>% 
  rename(
    volatility = `Volatility of market returns (GARCH(1,1))`
  )

volatility_tbl

industry <- list("All industries",
                 "Business services",
                 "Consumables",
                 "Health",
                 "Manufacturing",
                 "Other"
                 
)

# General herding estimation ----------------------------------------------
# selecting date and herding (a2)
general_herding_a2_presidential_tbl <- 
  general_herding$models$models_rol %>% 
  dplyr::select(Date, Category, a2) %>% 
# creating a2 dummy
  mutate(a2_dummy = ifelse(a2 < 0, 1, 0)) %>% 
# merge general_herding_a2_tbl with presidential_tbl
  left_join(presidential_tbl, by = c("Date" = "Date"))

general_herding_a2_presidential_tbl

# OLS of a2_dummy on party_dummy
formula <- as.formula(a2_dummy ~ party_dummy + volatility)

ols_a2_party_dummy_model_tbl <- 
  general_herding_a2_presidential_tbl %>% 
  left_join(volatility_tbl, by = c("Date" = "Date")) %>% 
  dummy_dummy_full_workflow(formula = formula) %>% 
  mutate(Group = "General Herding")

ols_a2_party_dummy_model_tbl

# Fundamental herding -----------------------------------------------------
# selection date and herding (a2)
fundamental_herding_a2_presidential_tbl <- 
  fundamental_herding$ols_rol$ols_full_fundamental_rol_tbl %>% 
  dplyr::select(Date, Category, a2) %>% 
# creating a2 dummy
  mutate(a2_fund_dummy = ifelse(a2 < 0, 1, 0)) %>% 
# merge fundamental_herding_a2_tbl with presidential_tbl
  left_join(presidential_tbl, by = c("Date" = "Date"))

fundamental_herding_a2_presidential_tbl
# OLS of a2_fund_dummy on party_dummy
formula <- as.formula(a2_fund_dummy ~ party_dummy + volatility)

ols_a2_fund_party_dummy_model_tbl<- 
  fundamental_herding_a2_presidential_tbl %>% 
  left_join(volatility_tbl, by = c("Date" = "Date")) %>%
  dummy_dummy_full_workflow(formula = formula) %>% 
  mutate(Group = "Fundamental Herding")

ols_a2_fund_party_dummy_model_tbl

# Non-Fundamental herding  ----------------------------------------------------------

# selection date and herding (a2)
non_fundamental_herding_a2_presidential_tbl <- 
  fundamental_herding$ols_rol$ols_full_nonfundamental_rol_tbl %>% 
  dplyr::select(Date, Category, a2) %>% 
# creating a2 dummy
  mutate(a2_nonfund_dummy = ifelse(a2 < 0, 1, 0)) %>% 
# merge non_fundamental_herding_a2_tbl with presidential_tbl
  left_join(presidential_tbl, by = c("Date" = "Date"))

# OLS of a2_nonfund_dummy on party_dummy
formula <- as.formula(a2_nonfund_dummy ~ party_dummy + volatility)

ols_a2_nonfund_party_dummy_model_tbl<- 
  non_fundamental_herding_a2_presidential_tbl %>%
  left_join(volatility_tbl, by = c("Date" = "Date")) %>%
  dummy_dummy_full_workflow(formula = formula) %>% 
  mutate(Group = "Non-Fundamental Herding")
  
ols_a2_nonfund_party_dummy_model_tbl

# Combined ----------------------------------------------------------------
# combine all the models
ols_party_dummy_model_tbl <- 
  bind_rows(
    ols_a2_party_dummy_model_tbl,
    ols_a2_fund_party_dummy_model_tbl,
    ols_a2_nonfund_party_dummy_model_tbl
  )

ols_party_dummy_model_tbl

# Probit - General herding ---------------------------------------------------------
# function to estimating glm probit model a2_dummy on party_dummy
# get the marginal effects
formula <-  as.formula(a2_dummy ~ party_dummy + volatility)

#  marginal effects for "All industries", "Consumables", "Health", "Manufacturing", "Other", "Business services" 
general_herding_tbl <- 
  general_herding_a2_presidential_tbl %>% 
  left_join(volatility_tbl, by = c("Date" = "Date"))       
   
general_herding_probit <- group_probit_marginal_effect(data = general_herding_tbl)
general_herding_probit
  
# Probit - Fundamental herding  ---------------------------------------------------------
formula <- as.formula(a2_fund_dummy ~ party_dummy + volatility)
fundamental_herding_tbl <- 
  fundamental_herding_a2_presidential_tbl %>% 
  left_join(volatility_tbl, by = c("Date" = "Date"))

fundamental_herding_probit <- group_probit_marginal_effect(data = fundamental_herding_tbl,
                                                           group_type = "Fundamental Herding")
fundamental_herding_probit

# Non-Fundamental herding - Probit ---------------------------------------------------------
formula <- as.formula(a2_nonfund_dummy ~ party_dummy + volatility)
nonfundamental_herding_tbl <- 
  non_fundamental_herding_a2_presidential_tbl %>% 
  left_join(volatility_tbl, by = c("Date" = "Date"))

nonfundamental_herding_probit <- group_probit_marginal_effect(data = nonfundamental_herding_tbl, 
                                                              group_type = "Non-Fundamental Herding")
nonfundamental_herding_probit

# Combined - Probit ----------------------------------------------------------------
# combine all the models
probit_party_dummy_model_tbl <- 
  bind_rows(
    general_herding_probit,
    fundamental_herding_probit,
    nonfundamental_herding_probit
  )

# Export ---------------------------------------------------------------
artifacts_party_dummy <- list (
  ols_party_dummy_model_tbl = ols_party_dummy_model_tbl,
  probit_party_dummy_model_tbl = probit_party_dummy_model_tbl
)

write_rds(artifacts_party_dummy, file = here("Outputs", "artifacts_party_dummy.rds"))


