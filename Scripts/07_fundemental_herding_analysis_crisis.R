# Description
# Fundamental and non fundamental herding in time of crisis Xolani Sibande - 17 October 2023
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
dummy_crisis <- function(data_date, start_date, end_date){
  ifelse(data_date > start_date & data_date < end_date, 1,0)
}
ols_fund_crisis <- function(data, 
                            fund_formula = fund_formula,
                            non_fund_formula = non_fund_formula,
                            crisis = "Great Depression"){
  formula <- fund_formula
  fund_tbl <-
    data %>% 
    ols_group_full_workflow(formula = formula) %>% 
    relocate(starts_with("dummy_abs"), .after = `(Intercept)`) %>%
    relocate(starts_with("dummy_squared"), .before = starts_with("anti_dummy_squared")) %>% 
    mutate(Herd = "Fundamental", Crisis = crisis) %>% 
    mutate(Fund = paste0(Crisis, ": ", Herd)) %>% 
    dplyr::select(-Herd, -Crisis)
    
  
  formula <- non_fund_formula
  non_fund_tbl <-
    data %>% 
    ols_group_full_workflow(formula = formula) %>% 
    relocate(starts_with("dummy_abs"), .after = `(Intercept)`) %>%
    relocate(starts_with("dummy_squared"), .before = starts_with("anti_dummy_squared")) %>% 
    mutate(Herd = "Non Fundamental", Crisis = crisis) %>% 
    mutate(Fund = paste0(Crisis, ": ", Herd)) %>% 
    dplyr::select(-Herd, -Crisis)
  
  rbind(fund_tbl, non_fund_tbl)
  
}

# Import -------------------------------------------------------------
combined_fundamental <- read_rds(here("Outputs", "artifacts_fundamental_herding.rds"))
combined_fundamental_tbl <- combined_fundamental$ols$combined_fundamental_tbl

# EDA ---------------------------------------------------------------
combined_fundamental_tbl %>% skim()

# Dummies --------------------------------------------------------
start_gd <- as.Date("1929-01-01")
end_gd <- as.Date("1939-12-31")

start_db <- as.Date("1997-01-01")
end_db <- as.Date("2003-12-31")

start_fc <- as.Date("2007-01-01")
end_fc <- as.Date("2009-12-31")

start_cv <- as.Date("2020-01-01")
end_cv <- as.Date("2021-12-31")

dummy_tbl <- tibble(
  Date  = seq(
    from = as.Date("1926-07-01"),
    to = as.Date("2022-07-29"),
    by = "day"
  ),
  dummy_gd = dummy_crisis(
    data_date = Date,
    start_date = start_gd,
    end_date = end_gd
  ),
  anti_dummy_gd = 1 - dummy_gd, 
  dummy_db = dummy_crisis(
    data_date = Date,
    start_date = start_db,
    end_date = end_db
  ),
  anti_dummy_db = 1 - dummy_db, 
  dummy_fc = dummy_crisis(
    data_date = Date,
    start_date = start_fc,
    end_date = end_fc
  ),
  anti_dummy_fc = 1 - dummy_fc,
  dummy_cv = dummy_crisis(
    data_date = Date,
    start_date = start_cv,
    end_date = end_cv
  ),
  anti_dummy_cv = 1 - dummy_cv
)

dummy_tbl %>% fx_plot()


# Combining with Dummies --------------------------------------------------
combined_fundamental_dummy_tbl <- 
  combined_fundamental_tbl %>% 
  left_join(dummy_tbl, by = c("Date" = "Date")) %>% 
  mutate(
    # Great Depression
    squared_market_returns_gd = `Market Return`^2, 
    absolute_market_returns_gd = abs(`Market Return`),
    dummy_abs_gd = dummy_gd*absolute_market_returns_gd,
    anti_dummy_abs_gd = anti_dummy_gd*absolute_market_returns_gd,
    dummy_squared_gd = dummy_gd*squared_market_returns_gd,
    anti_dummy_squared_gd = anti_dummy_gd*squared_market_returns_gd,
    # Dot-come bubble
    squared_market_returns_db = `Market Return`^2, 
    absolute_market_returns_db = abs(`Market Return`),
    dummy_abs_db = dummy_db*absolute_market_returns_db,
    anti_dummy_abs_db = anti_dummy_db*absolute_market_returns_db,
    dummy_squared_db = dummy_db*squared_market_returns_db,
    anti_dummy_squared_db = anti_dummy_db*squared_market_returns_db,
    # Financial Crisis
    squared_market_returns_fc = `Market Return`^2, 
    absolute_market_returns_fc = abs(`Market Return`),
    dummy_abs_fc = dummy_fc*absolute_market_returns_fc,
    anti_dummy_abs_fc = anti_dummy_fc*absolute_market_returns_fc,
    dummy_squared_fc = dummy_fc*squared_market_returns_fc,
    anti_dummy_squared_fc = anti_dummy_fc*squared_market_returns_fc,
    # Covid crisis
    squared_market_returns_cv = `Market Return`^2, 
    absolute_market_returns_cv = abs(`Market Return`),
    dummy_abs_cv = dummy_cv*absolute_market_returns_cv,
    anti_dummy_abs_cv = anti_dummy_cv*absolute_market_returns_cv,
    dummy_squared_cv = dummy_cv*squared_market_returns_cv,
    anti_dummy_squared_cv = anti_dummy_cv*squared_market_returns_cv
  )

# Static Regressions ---------------------------------------------------------------
## OLS ---------------------------------------------------------------------
### Great depression ----------------------------------------------------
fund_formula_gd <- as.formula(
  CSAD_fund ~ dummy_abs_gd + anti_dummy_abs_gd + dummy_squared_gd + anti_dummy_squared_gd
)
non_fund_formula_gd <- as.formula(
  CSAD_nonfund ~ dummy_abs_gd + anti_dummy_abs_gd + dummy_squared_gd + anti_dummy_squared_gd
)


ols_gd_tbl <- 
  combined_fundamental_dummy_tbl %>% 
  ols_fund_crisis(
    fund_formula = fund_formula_gd, 
    non_fund_formula = non_fund_formula_gd)

### Dot-com Bubble ----------------------------------------------------
fund_formula_db <- as.formula(
  CSAD_fund ~ dummy_abs_db + anti_dummy_abs_db + dummy_squared_db + anti_dummy_squared_db
)
non_fund_formula_db <- as.formula(
  CSAD_nonfund ~ dummy_abs_db + anti_dummy_abs_db + dummy_squared_db + anti_dummy_squared_db
)

ols_db_tbl <- 
  combined_fundamental_dummy_tbl %>% 
  ols_fund_crisis(
    fund_formula = fund_formula_db, 
    non_fund_formula = non_fund_formula_db,
    crisis = "Dot-com Bubble")


### Financial Crisis -----------------------------------------------------
fund_formula_fc <- as.formula(
  CSAD_fund ~ dummy_abs_fc + anti_dummy_abs_fc + dummy_squared_fc + anti_dummy_squared_fc
)
non_fund_formula_fc <- as.formula(
  CSAD_nonfund ~ dummy_abs_fc + anti_dummy_abs_fc + dummy_squared_fc + anti_dummy_squared_fc
)

ols_fc_tbl <- 
  combined_fundamental_dummy_tbl %>% 
  ols_fund_crisis(
    fund_formula = fund_formula_fc, 
    non_fund_formula = non_fund_formula_fc,
    crisis = "Financial Crisis")

### Covid Crisis ---------------------------------------------------------
fund_formula_cv <- as.formula(
  CSAD_fund ~ dummy_abs_cv + anti_dummy_abs_cv + dummy_squared_cv + anti_dummy_squared_cv
)
non_fund_formula_cv <- as.formula(
  CSAD_nonfund ~ dummy_abs_cv + anti_dummy_abs_cv + dummy_squared_cv + anti_dummy_squared_cv
)

ols_cv_tbl <- 
  combined_fundamental_dummy_tbl %>% 
  ols_fund_crisis(
    fund_formula = fund_formula_cv, 
    non_fund_formula = non_fund_formula_cv,
    crisis = "Covid Crisis")


### Combining -----------------------------------------------------------
ols_tbl <- 
  rbind(
  ols_gd_tbl %>% rename(dummy_abs = dummy_abs_gd, 
                        anti_dummy_abs = anti_dummy_abs_gd,
                        dummy_squared = dummy_squared_gd,
                        anti_dummy_squared = anti_dummy_squared_gd
  ),
  ols_db_tbl %>% rename(dummy_abs = dummy_abs_db, 
                        anti_dummy_abs = anti_dummy_abs_db,
                        dummy_squared = dummy_squared_db,
                        anti_dummy_squared = anti_dummy_squared_db
  ),
  ols_fc_tbl %>% rename(dummy_abs = dummy_abs_fc, 
                        anti_dummy_abs = anti_dummy_abs_fc,
                        dummy_squared = dummy_squared_fc,
                        anti_dummy_squared = anti_dummy_squared_fc
  ),
  ols_cv_tbl %>% rename(dummy_abs = dummy_abs_cv, 
                        anti_dummy_abs = anti_dummy_abs_cv,
                        dummy_squared = dummy_squared_cv,
                        anti_dummy_squared = anti_dummy_squared_cv
  )
) %>% 
  ungroup()
  
# Time varying Regressions ------------------------------------------------
rol_crisis_fundamental <- function(data, crisis_abbre = "gd"){
  models_rol <- 
    data %>% 
    dplyr::select(Date, Category, Crisis, `Market Return`, CSAD_fund, ends_with(crisis_abbre)) %>% 
    rename(
      dummy_abs = paste0("dummy_abs_", crisis_abbre), 
      anti_dummy_abs = paste0("anti_dummy_abs_", crisis_abbre),
      dummy_squared =  paste0("dummy_squared_", crisis_abbre),
      anti_dummy_squared = paste0("anti_dummy_squared_", crisis_abbre)
    ) %>% 
    ols_slidify_models_crisis(dep_var = CSAD_fund) 
  
  rol_gg_coef <-
    models_rol %>%
    dplyr::select(-starts_with("t")) %>% 
    fx_recode_prep_crisis() %>% 
    fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
    theme(legend.position = "none")
  
  rol_gg_tstats <-
    models_rol %>%
    dplyr::select(-starts_with("a")) %>% 
    fx_recode_prep_crisis() %>% 
    fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
    geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
    geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 
  
  rol_gg_coef / rol_gg_tstats
}
rol_crisis_nonfundamental <- function(data, crisis_abbre = "gd"){
  models_rol <- 
    data %>% 
    dplyr::select(Date, Category, Crisis, `Market Return`, CSAD_nonfund, ends_with(crisis_abbre)) %>% 
    rename(
      dummy_abs = paste0("dummy_abs_", crisis_abbre), 
      anti_dummy_abs = paste0("anti_dummy_abs_", crisis_abbre),
      dummy_squared =  paste0("dummy_squared_", crisis_abbre),
      anti_dummy_squared = paste0("anti_dummy_squared_", crisis_abbre)
    ) %>% 
    ols_slidify_models_crisis(dep_var = CSAD_nonfund) 
  
  rol_gg_coef <-
    models_rol %>%
    dplyr::select(-starts_with("t")) %>% 
    fx_recode_prep_crisis() %>% 
    fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
    theme(legend.position = "none")
  
  rol_gg_tstats <-
    models_rol %>%
    dplyr::select(-starts_with("a")) %>% 
    fx_recode_prep_crisis() %>% 
    fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
    geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
    geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 
  
  rol_gg_coef / rol_gg_tstats
}

## Great Depression ------------------------------------------------------
rol_fund_gd_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_fundamental(crisis_abbre = "gd")
rol_nonfund_gd_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_nonfundamental(crisis_abbre = "gd")

## Dot-com bubble ----------------------------------------------------------
rol_fund_db_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_fundamental(crisis_abbre = "db")
rol_nonfund_db_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_nonfundamental(crisis_abbre = "db")

## Financial Crisis ------------------------------------------------------
rol_fund_fc_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_fundamental(crisis_abbre = "fc")
rol_nonfund_fc_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_nonfundamental(crisis_abbre = "fc")

## Covid Crisis ----------------------------------------------------------
rol_fund_cv_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_fundamental(crisis_abbre = "cv")
rol_nonfund_cv_gg <- combined_fundamental_dummy_tbl %>% rol_crisis_nonfundamental(crisis_abbre = "cv")

# Export ---------------------------------------------------------------
artifacts_crisis_fundamental <- list (
  ols = list(
    ols_tbl = ols_tbl
  ),
  fundamental = list(
    rol_fund_gd_gg = rol_fund_gd_gg,
    rol_fund_db_gg = rol_fund_db_gg,
    rol_fund_fc_gg = rol_fund_fc_gg,
    rol_fund_cv_gg = rol_fund_cv_gg
  ),
  nonfundamental = list(
    rol_nonfund_gd_gg = rol_nonfund_gd_gg,
    rol_nonfund_db_gg = rol_nonfund_db_gg,
    rol_nonfund_fc_gg = rol_nonfund_fc_gg,
    rol_nonfund_cv_gg = rol_nonfund_cv_gg 
  )
)

write_rds(artifacts_crisis_fundamental, file = here("Outputs", "artifacts_crisis_fundamental.rds"))


