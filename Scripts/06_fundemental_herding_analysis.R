# Description
# Fundamental versus non fundamental herding Xolani Sibande - 13 October 2023
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
  mutate("CSAD_fund" = CSAD - CSAD_nonfund)

# Static OLS ------------------------------------------------------
## Fundamental -------------------------------------------------------------------
formula <-  as.formula(CSAD_fund ~ abs(`Market Return`) + I(`Market Return` ^ 2))
ols_full_fundamental_tbl <-
  combined_fundamental_tbl %>%
  dplyr::select(-Crisis) %>%
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Fundamental") %>% 
  ungroup()

## Non Fundamental -------------------------------------------------------
formula <-  as.formula(CSAD_nonfund ~ abs(`Market Return`) + I(`Market Return` ^ 2))
ols_full_nonfundamental_tbl <- 
  combined_fundamental_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Non Fundamental") %>% 
  ungroup() 

## Combined ----------------------------------------------------------------
ols_fund_tbl <- rbind(ols_full_fundamental_tbl, ols_full_nonfundamental_tbl)

# Rolling OLS -------------------------------------------------------------
rolling_reg_spec <-
  slidify(
    .f =  ~coeftest(lm(..1 ~ ..2 + ..3)),
    .period = 250,
    .align = "right",
    .unlist = FALSE,
    .partial = FALSE
  )

# ## Fundamental ----------------------------------------------------------
ols_full_fundamental_rol_tbl <- 
  combined_fundamental_tbl %>%
  dplyr::select(-Crisis) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  mutate(models = rolling_reg_spec(CSAD_fund, abs(`Market Return`), I(`Market Return` ^ 2))) %>%  
  unnest_rol_col_standard(rol_column = models) 

rol_fundamental_coeff_gg <-
  ols_full_fundamental_rol_tbl %>%
  dplyr::select(- starts_with("t")) %>% 
  slidyfy_gg_workflow_standard(variables_color = 6) +
  theme(
    legend.position = "none")

rol_fundamental_tstats_gg <-
  ols_full_fundamental_rol_tbl %>%
  dplyr::select(- starts_with("a")) %>% 
  slidyfy_gg_workflow_standard(variables_color = 6) +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 
rol_fundamental_gg <- rol_fundamental_coeff_gg / rol_fundamental_tstats_gg

# Non fundamental ---------------------------------------------------------
ols_full_nonfundamental_rol_tbl <- 
  combined_fundamental_tbl %>%
  dplyr::select(-Crisis) %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  group_by(Category) %>% 
  mutate(models = rolling_reg_spec(CSAD_nonfund, abs(`Market Return`), I(`Market Return` ^ 2))) %>% 
  unnest_rol_col_standard(rol_column = models) 

rol_nonfundamental_coeff_gg <-
  ols_full_nonfundamental_rol_tbl %>%
  dplyr::select(- starts_with("t")) %>% 
  slidyfy_gg_workflow_standard() +
  theme(
    legend.position = "none")

rol_nonfundamental_tstats_gg <-
  ols_full_nonfundamental_rol_tbl %>%
  dplyr::select(- starts_with("a")) %>% 
  slidyfy_gg_workflow_standard() +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 
rol_nonfundamental_gg <- rol_nonfundamental_coeff_gg / rol_nonfundamental_tstats_gg 

# Export ---------------------------------------------------------------
artifacts_fundamental_herding <- list (
  ols = list(
    combined_fundamental_tbl = combined_fundamental_tbl,
    combined_fundamental_tbl = combined_fundamental_tbl,
    ols_fund_tbl = ols_fund_tbl
  ),
  ols_rol = list(
    ols_full_fundamental_rol_tbl = ols_full_fundamental_rol_tbl,
    ols_full_nonfundamental_rol_tbl = ols_full_nonfundamental_rol_tbl
  ),
  ols_rol_graphs = list(
    rol_fundamental_gg = rol_fundamental_gg,
    rol_nonfundamental_gg = rol_nonfundamental_gg
  )
)

write_rds(artifacts_fundamental_herding, 
          file = here("Outputs",
                      "artifacts_fundamental_herding.rds"))


