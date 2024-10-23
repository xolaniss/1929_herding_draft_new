# Description
## Analysis on crisis periods - Xolani Sibande 7 Sept 2023

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
library(quantreg)

options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "group_ols_functions.R"))
source(here("Functions", "group_ols_crisis_functions.R"))
source(here("Functions", "group_ols_slidify_functions.R"))
dummy_crisis <- function(data_date, start_date, end_date){
  ifelse(data_date > start_date & data_date < end_date, 1,0)
}

# Import -------------------------------------------------------------
result_csad_cssd <- read_rds(here("Outputs", "artifacts_descriptives.rds"))
combined_results_tbl <- result_csad_cssd$combined_results_tbl

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

# Combining data sets -----------------------------------------------------
combined_results_dummy_tbl <-
  combined_results_tbl %>% 
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

combined_dummy_gg <- 
  combined_results_dummy_tbl %>% 
  ggplot(aes(x = Date, y = dummy, group = Category)) +
  geom_line() +
  facet_wrap( . ~ Category)

# Regressions ---------------------------------------------------------------
## OLS ---------------------------------------------------------------------
### Great depression ----------------------------------------------------
formula <- as.formula(
  CSAD ~ dummy_abs_gd + anti_dummy_abs_gd + dummy_squared_gd + anti_dummy_squared_gd
)
ols_gd_tbl <-
  combined_results_dummy_tbl %>% 
  ols_group_full_workflow(formula = formula) %>% 
  relocate(dummy_abs_gd, .after = `(Intercept)`) %>%
  relocate(dummy_squared_gd, .before = anti_dummy_squared_gd)

### Dot-com bubble -------------------------------------------------------
formula <- as.formula(
  CSAD ~ dummy_abs_db + anti_dummy_abs_db + dummy_squared_db + anti_dummy_squared_db
)
ols_db_tbl <-
  combined_results_dummy_tbl %>% 
  ols_group_full_workflow(formula = formula) %>% 
  relocate(dummy_abs_db, .after = `(Intercept)`) %>%
  relocate(dummy_squared_db, .before = anti_dummy_squared_db)

### Financial crisis --------------------------------------------------------
formula <- as.formula(
  CSAD ~ dummy_abs_fc + anti_dummy_abs_fc + dummy_squared_fc + anti_dummy_squared_fc
)
ols_fc_tbl <-
  combined_results_dummy_tbl %>% 
  ols_group_full_workflow(formula = formula) %>% 
  relocate(dummy_abs_fc, .after = `(Intercept)`) %>%
  relocate(dummy_squared_fc, .before = anti_dummy_squared_fc)

### Covid crisis ---------------------------------------------------------
formula <- as.formula(
  CSAD ~ dummy_abs_cv + anti_dummy_abs_cv + dummy_squared_cv + anti_dummy_squared_cv
)
ols_cv_tbl <-
  combined_results_dummy_tbl %>% 
  ols_group_full_workflow(formula = formula) %>% 
  relocate(dummy_abs_cv, .after = `(Intercept)`) %>%
  relocate(dummy_squared_cv, .before = anti_dummy_squared_cv)

### Combined Regression -----------------------------------------------------
combined_regression_tbl <- 
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
  ungroup() %>% 
  mutate(Crisis = c(rep("Great Depression", 6), 
                    rep("Dot-com Bubble", 6), 
                    rep("Financial Crisis", 6), 
                    rep("Covid Crisis", 6)))


## Rolling -----------------------------------------------------------------
### Great depression -----------------------------------------------------
models_rol_gd <- 
  combined_results_dummy_tbl %>% 
  dplyr::select(Date, Category, Crisis, `Market Return`, CSAD, ends_with("gd")) %>% 
  rename(
    dummy_abs = dummy_abs_gd, 
    anti_dummy_abs = anti_dummy_abs_gd,
    dummy_squared = dummy_squared_gd,
    anti_dummy_squared = anti_dummy_squared_gd
  ) %>% 
  ols_slidify_models_crisis() 

rol_gd_coef_gg <-
  models_rol_gd %>%
  dplyr::select(-starts_with("t")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  theme(legend.position = "none")

rol_gd_tstats_gg <-
  models_rol_gd %>%
  dplyr::select(-starts_with("a")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 

rol_gd_gg <- rol_gd_coef_gg / rol_gd_tstats_gg

### Dot-com bubble -------------------------------------------------------
models_rol_db <- 
  combined_results_dummy_tbl %>% 
  dplyr::select(Date, Category, Crisis, `Market Return`, CSAD, ends_with("db")) %>% 
  rename(
    dummy_abs = dummy_abs_db, 
    anti_dummy_abs = anti_dummy_abs_db,
    dummy_squared = dummy_squared_db,
    anti_dummy_squared = anti_dummy_squared_db
  ) %>% 
  ols_slidify_models_crisis() 

rol_db_coef_gg <-
  models_rol_db %>%
  dplyr::select(-starts_with("t")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  theme(legend.position = "none")

rol_db_tstats_gg <-
  models_rol_db %>%
  dplyr::select(-starts_with("a")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3) 

rol_db_gg <- rol_db_coef_gg / rol_db_tstats_gg

### Financial crisis -----------------------------------------------------
models_rol_fc <- 
  combined_results_dummy_tbl %>% 
  dplyr::select(Date, Category, Crisis, `Market Return`, CSAD, ends_with("fc")) %>% 
  rename(
    dummy_abs = dummy_abs_fc, 
    anti_dummy_abs = anti_dummy_abs_fc,
    dummy_squared = dummy_squared_fc,
    anti_dummy_squared = anti_dummy_squared_fc
  ) %>% 
  ols_slidify_models_crisis() 

rol_fc_coef_gg <-
  models_rol_fc %>%
  dplyr::select(-starts_with("t")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  theme(legend.position = "none")

rol_fc_tstats_gg <-
  models_rol_fc %>%
  dplyr::select(-starts_with("a")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3)

rol_fc_gg <- rol_fc_coef_gg / rol_fc_tstats_gg

### Covid crisis ---------------------------------------------------------
models_rol_cv <- 
  combined_results_dummy_tbl %>% 
  dplyr::select(Date, Category, Crisis, `Market Return`, CSAD, ends_with("cv")) %>% 
  rename(
    dummy_abs = dummy_abs_cv, 
    anti_dummy_abs = anti_dummy_abs_cv,
    dummy_squared = dummy_squared_cv,
    anti_dummy_squared = anti_dummy_squared_cv
  ) %>% 
  ols_slidify_models_crisis() 

rol_cv_coef_gg <-
  models_rol_cv %>%
  dplyr::select(-starts_with("t")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  theme(legend.position = "none")

rol_cv_tstats_gg <-
  models_rol_cv %>%
  dplyr::select(-starts_with("a")) %>% 
  fx_recode_prep_crisis() %>% 
  fx_recode_group_plot(variables_color = 6, ncol = 5, nrow = 2) +
  geom_hline(yintercept = 1.96, colour = "grey10", linetype = 2, linewidth = 0.3) +
  geom_hline(yintercept = -1.96, colour = "grey10", linetype = 2, linewidth = 0.3)

rol_cv_gg <- rol_cv_coef_gg / rol_cv_tstats_gg

### Combined gg ----------------------------------------------------------
rol_gg <- rol_gd_gg / rol_db_gg / rol_fc_gg / rol_cv_gg

# Export ---------------------------------------------------------------
artifacts_herding_gfc_crisis <- list (
  models = list(
    combined_regression_tbl  = combined_regression_tbl,
    models_rol_gd = models_rol_gd,
    models_rol_db = models_rol_db,
    models_rol_fc = models_rol_fc,
    models_rol_cv = models_rol_cv 
  ),
  graphs = list(
    rol_gg = rol_gg,
    rol_gd_gg = rol_gd_gg,
    rol_db_gg = rol_db_gg,
    rol_fc_gg = rol_fc_gg,
    rol_cv_gg = rol_cv_gg
  )
)

write_rds(artifacts_herding_gfc_crisis, file = here("Outputs", "artifacts_herding_crisis.rds"))
