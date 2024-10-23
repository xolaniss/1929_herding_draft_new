# Description
# Approval and term analysis - October 2024
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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))

# Import -------------------------------------------------------------
crisis_tbl <- read_rds(here("Outputs", "artifacts_alternative_ratings_models.rds")) %>% pluck(1, 1)
party_dummy_tbl <- read_rds(here("Outputs", "artifacts_party_dummy.rds")) %>% pluck(1) %>% 
  mutate(D_dummy = if_else(party_dummy == 1, 1, 0)) %>% 
  mutate(R_dummy = if_else(party_dummy == 0, 1, 0))

party_dummy_tbl %>% 
  pivot_longer(-c(Date, Party), names_to = "names", values_to = "value") %>% 
  ggplot(aes(x = Date, y = value )) +
  geom_line(aes(color = names)) +
  facet_wrap(~names, scales = "free_y")
  
# Combing -----------------------------------------------------------------
combined_tbl <- 
  crisis_tbl %>% 
  left_join(
    party_dummy_tbl,
    by = c("Date" = "Date")
  )


# PAR ---------------------------------------------------------------------

## Overall herding -------------------------------------------------------
formula <-  as.formula(CSAD ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         D_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         D_dummy:PAR_low_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_low_dummy:I(`Market Return` ^ 2)
                       )
par_ols_full_tbl <- 
  combined_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "General Herding") %>%
  ungroup()

par_ols_full_tbl 

## Fundamental -------------------------------------------------------------------
formula <-  as.formula(CSAD_fund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         D_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         D_dummy:PAR_low_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_low_dummy:I(`Market Return` ^ 2)
                       )
par_ols_full_fundamental_tbl <-
  combined_tbl %>%
  dplyr::select(-Crisis) %>%
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Fundamental Herding") %>% 
  ungroup()

par_ols_full_fundamental_tbl

## Non Fundamental -------------------------------------------------------
formula <-  as.formula(CSAD_nonfund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         D_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         D_dummy:PAR_low_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_low_dummy:I(`Market Return` ^ 2)
                       )
par_ols_full_nonfundamental_tbl <- 
  combined_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Non-Fundamental Herding") %>% 
  ungroup() 

par_ols_full_nonfundamental_tbl


# PEAR ---------------------------------------------------------------------
## Overall herding -------------------------------------------------------
formula <-  as.formula(CSAD ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         D_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         D_dummy:PAR_low_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_low_dummy:I(`Market Return` ^ 2)
                       )
pear_ols_full_tbl <-
  combined_tbl %>%
  dplyr::select(-Crisis) %>%
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "General Herding") %>% 
  ungroup()

pear_ols_full_tbl


## Fundamental -------------------------------------------------------------------
formula <-  as.formula(CSAD_fund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         D_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         D_dummy:PAR_low_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_low_dummy:I(`Market Return` ^ 2)
                       )
pear_ols_full_fundamental_tbl <-
  combined_tbl %>%
  dplyr::select(-Crisis) %>%
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Fundamental Herding") %>% 
  ungroup()

pear_ols_full_fundamental_tbl

## Non Fundamental -------------------------------------------------------
formula <-  as.formula(CSAD_nonfund ~ abs(`Market Return`) + 
                         I(`Market Return` ^ 2) + 
                         D_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         D_dummy:PAR_low_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_high_dummy:I(`Market Return` ^ 2) +
                         R_dummy:PAR_low_dummy:I(`Market Return` ^ 2)
                       )
pear_ols_full_nonfundamental_tbl <- 
  combined_tbl %>% 
  dplyr::select(-Crisis) %>% 
  ols_group_full_workflow(formula = formula) %>% 
  mutate(Herd = "Non-Fundamental Herding") %>% 
  ungroup() 

pear_ols_full_nonfundamental_tbl



# Combined -------------------------------------------------------------
combined_par_tbl <- 
  bind_rows(
    par_ols_full_tbl,
    par_ols_full_fundamental_tbl,
    par_ols_full_nonfundamental_tbl
  ) 

combined_pear_tbl <-
  bind_rows(
    pear_ols_full_tbl,
    pear_ols_full_fundamental_tbl,
    pear_ols_full_nonfundamental_tbl
  )

# Export ---------------------------------------------------------------
artifacts_approval_term_analysis <- list(
  models = list(
    combined_par_tbl = combined_par_tbl,
    combined_pear_tbl = combined_pear_tbl
  )
)

write_rds(artifacts_approval_term_analysis, file = here("Outputs", "artifacts_approval_term_analysis.rds"))


