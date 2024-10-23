# Description
# Fama French factors data cleaning by Xolani Sibande 17 September 2022

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

options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
fama_french <- read_csv(here("Data", "F_F_Research_Data_Factors_daily.csv"), skip = 4)

# Cleaning -----------------------------------------------------------------
fama_french_tbl <- 
  fama_french %>% 
  rename("Date" = ...1) %>% 
  mutate(Date = parse_date_time(Date, "Ymd")) 

# EDA --------------------------------------------------------
fama_french_tbl %>% skim()

# Graphing ---------------------------------------------------------------
fama_french_gg <- 
  fama_french_tbl %>% 
  fx_plot(variables_color = 5)

# Export ---------------------------------------------------------------
artifacts_fama_french <- list (
  data = list(
    fama_french_tbl = fama_french_tbl
  ),
  graphs = list(
    fama_french_gg = fama_french_gg
  )
  
)

write_rds(artifacts_fama_french, file = here("Outputs", "artifacts_fama_french.rds"))


