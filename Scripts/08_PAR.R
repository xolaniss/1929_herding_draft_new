# Description
# Looking at PAR data options 6 November 2023 - Xolani Sibande

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

# Import -------------------------------------------------------------
par <- read_excel(here("Data", "PAR.xlsx"))
pear <-  read_excel(here("Data", "PEAR.xlsx"), sheet = 2)
general_herding <- read_rds(here("Outputs", "artifacts_general_herding.rds"))
general_herding_tbl <- general_herding$models$models_rol

# Cleaning -----------------------------------------------------------------
par_tbl <- 
  par %>% 
  rename(Date = ...1) %>% 
  mutate(Date = str_replace_all(Date, "M", "-")) %>% 
  mutate(Date = parse_date_time(Date, orders = "Ym"))


pear_tbl <- 
  pear %>% 
  rename(Date = yearmonth) %>% 
  mutate(Date = parse_date_time(Date, orders = "Ym"))


# EDA ---------------------------------------------------------------
par_tbl %>% skim()
pear_tbl %>% skim()

# Converting to daily ---------------------------------------------------------------
par_daily_tbl <- 
  general_herding_tbl %>% 
  filter(Date >= ymd("1941-07-01")) %>%
  ungroup() %>%
  filter(Category == "All industries") %>% 
  left_join(par_tbl, by = c("Date" = "Date")) %>% 
  fill(PAR, .direction = "downup") %>% 
  dplyr::select(Date, PAR) %>% 
  mutate(PAR_high_dummy = ifelse(PAR > quantile(PAR, 0.6667), 1, 0)) %>% 
  mutate(PAR_low_dummy = ifelse(PAR < quantile(PAR, 0.3333), 1, 0))
  
par_daily_tbl %>% skim()  

pear_daily_tbl <- 
  general_herding_tbl %>% 
  filter(Date >= ymd("1981-04-01")) %>%
  ungroup() %>%
  filter(Category == "All industries") %>%
  left_join(pear_tbl, by = c("Date" = "Date")) %>% 
  fill(PEAR, .direction = "downup") %>%
  dplyr::select(Date, PEAR) %>% 
  mutate(PEAR_high_dummy = ifelse(PEAR > quantile(PEAR, 0.6667), 1, 0)) %>%
  mutate(PEAR_low_dummy = ifelse(PEAR < quantile(PEAR, 0.3333), 1, 0))

# Graphing ---------------------------------------------------------------
combined_tbl <- 
  par_daily_tbl %>% 
  left_join(pear_daily_tbl, by = c("Date" = "Date"))

combined_gg <- 
  combined_tbl %>% dplyr::select(Date, PAR, PEAR) %>%  fx_plot(variables_color = 2) + labs(y = "Rating (%)")

combined_gg

# Export ---------------------------------------------------------------
artifacts_presidential_ratings <- list (
  data = list(
    par_tbl = par_tbl,
    pear_tbl = pear_tbl,
    par_daily_tbl = par_daily_tbl,
    pear_daily_tbl = pear_daily_tbl
  ),
  graph = list(
    combined_gg = combined_gg
  )
)

write_rds(artifacts_presidential_ratings, file = here("Outputs", "artifacts_presidential_ratings.rds"))


