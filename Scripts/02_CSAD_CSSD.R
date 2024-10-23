# Description
# Calculation of CSAD using weighted returns by Xolani Sibande 20 October 2022

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

# 0.0 Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "cross_deviations_functions.R"))
crisis_periods <- function(data){
  data %>% 
    mutate(Date = as.Date(Date)) %>% 
    mutate(Crisis = 
             ifelse(Date > as.Date("1929-01-01") & Date < as.Date("1939-12-31"), "Great Depression",
                    ifelse(Date > as.Date("1997-01-01") & Date < as.Date("2003-12-31"), "Dot-com Bubble",
                           ifelse(Date > as.Date("2007-01-01") & Date < as.Date("2009-12-31"), "Financial Crisis", 
                                  ifelse(Date > as.Date("2020-01-01") & Date < as.Date("2021-12-31"), "Covid Crisis", "No Crisis")))
             )) %>% 
    relocate(Crisis, .after = Date) 
}

# 1.0 Import -------------------------------------------------------------
returns <- read_rds(here("Outputs", "artifacts_returns_data.rds"))
equal_returns_tbl <- 
  returns$full_data$equal_returns_tbl

equal_returns_crisis_tbl <- 
  equal_returns_tbl %>% 
  crisis_periods()

# 2.0 All industries ----------------------------------------------------------

## 2.1 CSAD --------------------------------------------------------------------
results_all_industries_csad_tbl <- cross_deviations_tbl(data = equal_returns_tbl)
results_all_industries_crisis_csad_tbl <- 
  group_cross_deviations_tbl(equal_returns_crisis_tbl)

## 2.2 Graphing ---------------------------------------------------------------
results_all_industries_gg <- 
  results_all_industries_csad_tbl %>%
  results_gg() +
  labs(title = "All industries") 

  

results_all_industries_group_crisis_gg <- 
  results_all_industries_crisis_csad_tbl %>% 
  group_results_gg() +
  labs(title = "All industries") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() 
  )
  
# 3.0 Consumable_group ----------------------------------------------------

## 3.1 Data ----------------------------------------------------------------
returns_consumables_group_tbl <- 
  returns$groups$Consumer_durables_nondurables_wholesale_retail_some_services_group_equal_tbl  # including some NAs in some categories %>% 
returns_consumables_group_crisis_tbl <- 
  returns_consumables_group_tbl %>% 
  crisis_periods()

## 3.2 CSAD and CSSD -------------------------------------------------------
results_consumables_group_csad_tbl <- cross_deviations_tbl(data = returns_consumables_group_tbl)
results_consumables_group_crisis_csad_tbl <- 
  group_cross_deviations_tbl(returns_consumables_group_crisis_tbl)

## 3.3 Graphing ------------------------------------------------------------
results_consumables_group_gg <- 
  results_consumables_group_csad_tbl %>% 
  results_gg() +
  labs(title = "Consumables")

results_consumables_group_crisis_gg <- 
  results_consumables_group_crisis_csad_tbl %>% 
  group_results_gg() +
  labs(title = "Consumables") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()
  )

# 5.0 Manuf_group ---------------------------------------------------------

## 5.1 Data ----------------------------------------------------------------
returns_manuf_group_tbl <- 
returns$groups$manufacturing_energy_utilities_group_equal_tbl
returns_manuf_group_crisis_tbl <- 
  returns_manuf_group_tbl %>% 
  crisis_periods()

## 5.2 CSAD and CSSD -------------------------------------------------------
results_manuf_group_csad_tbl <- cross_deviations_tbl(data = returns_manuf_group_tbl)
results_manuf_group_crisis_csad_tbl <- 
  group_cross_deviations_tbl(returns_manuf_group_crisis_tbl)

## 5.3 Graphing ------------------------------------------------------------
results_manuf_group_gg <- 
  results_manuf_group_csad_tbl %>% 
  results_gg() +
  labs(title = "Manufacturing")

results_manuf_group_crisis_gg <- 
  results_manuf_group_crisis_csad_tbl %>% 
  group_results_gg() +
  labs(title = "Manufacturing") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()
  )


# 6.0 Bus_group -----------------------------------------------------------

## 6.1 Data ----------------------------------------------------------------
returns_bus_group_tbl <- 
returns$groups$business_equipment_telephone_and_television_transmission_group_equal_tbl
returns_bus_group_crisis_tbl <- 
  returns_bus_group_tbl %>% 
  crisis_periods()

## 6.2 CSAD and CSSD -------------------------------------------------------
results_bus_group_csad_tbl <- cross_deviations_tbl(data = returns_bus_group_tbl)
results_bus_group_crisis_csad_tbl <- group_cross_deviations_tbl(returns_bus_group_crisis_tbl)

## 6.3 Graphing ------------------------------------------------------------
results_bus_group_gg <- 
  results_bus_group_csad_tbl %>% 
  results_gg() +
  labs(title = "Business services")

results_bus_group_crisis_gg <- 
  results_bus_group_crisis_csad_tbl %>% 
  group_results_gg() +
  labs(title = "Business services") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()
  )

# 7.0 Health_group --------------------------------------------------------

# 7.1 Data ----------------------------------------------------------------
returns_health_group_tbl <- 
returns$groups$healthcare_medical_equipment_and_drugs_group_equal_tbl 
returns_health_group_crisis_tbl <- 
  returns_health_group_tbl %>% 
  crisis_periods()

# 7.2 CSAD and CSSD -------------------------------------------------------
results_health_group_csad_tbl <- cross_deviations_tbl(data = returns_health_group_tbl)
results_health_group_crisis_csad_tbl <- group_cross_deviations_tbl(returns_health_group_crisis_tbl)

# 7.3 Graphing ------------------------------------------------------------
results_health_group_gg <- 
  results_health_group_csad_tbl %>% 
  results_gg() +
  labs(title = "Health")

results_health_group_crisis_gg <- 
  results_health_group_crisis_csad_tbl %>% 
  group_results_gg() +
  labs(title = "Health") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.text.x = element_blank()
  )

# 8.0 Mines_group  --------------------------------------------------------------

## 8.1 Data ----------------------------------------------------------------
returns_mines_group_tbl <- 
returns$groups$mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl
returns_mines_group_crisis_tbl <- 
  returns$groups$mines_constr_bldmt_trans_hotels_bus_serv_entertainment_finance__group_equal_tbl %>% 
  crisis_periods()

## 8.2 CSAD and CSSD -------------------------------------------------------
results_mines_group_csad_tbl <- cross_deviations_tbl(data = returns_mines_group_tbl)
results_mines_group_crisis_csad_tbl <- group_cross_deviations_tbl(returns_mines_group_crisis_tbl)

## 8.3 Graphing ------------------------------------------------------------
results_mines_group_gg <- 
  results_mines_group_csad_tbl %>% 
  results_gg() +
  labs(title = "Other")

results_mines_group_crisis_gg <- 
  results_mines_group_crisis_csad_tbl %>% 
  group_results_gg() +
  labs(title = "Other") +
  theme(strip.text.x = element_blank())
  

# 9. Combined -------------------------------------------------------------
results_all_gg <- 
  results_all_industries_gg / 
  results_consumables_group_gg / 
  results_health_group_gg /
  results_manuf_group_gg /
  results_bus_group_gg /
  results_mines_group_gg 

results_all_crisis_gg <- 
  results_all_industries_group_crisis_gg / 
  results_consumables_group_crisis_gg / 
  results_health_group_crisis_gg /
  results_manuf_group_crisis_gg /
  results_bus_group_crisis_gg /
  results_mines_group_crisis_gg 

# Export ---------------------------------------------------------------
artifacts_csad_cssd <- list (
  csad_no_crisis = list(
    results_all_industries_csad_tbl = results_all_industries_csad_tbl,
    results_consumables_group_csad_tbl = results_consumables_group_csad_tbl,
    # results_durables_group_tbl = results_durables_group_tbl,
    results_manuf_group_csad_tbl = results_manuf_group_csad_tbl,
    results_bus_group_csad_tbl = results_bus_group_csad_tbl,
    results_health_group_csad_tbl = results_health_group_csad_tbl,
    results_mines_group_csad_tbl = results_mines_group_csad_tbl
  ),
  csad_crisis = list(
    results_all_industries_crisis_csad_tbl = results_all_industries_crisis_csad_tbl,
    results_consumables_group_crisis_csad_tbl = results_consumables_group_crisis_csad_tbl,
    # results_durables_group_tbl = results_durables_group_tbl,
    results_manuf_group_crisis_csad_tbl = results_manuf_group_crisis_csad_tbl,
    results_bus_group_crisis_csad_tbl = results_bus_group_crisis_csad_tbl,
    results_health_group_crisis_csad_tbl = results_health_group_crisis_csad_tbl,
    results_mines_group_crisis_csad_tbl = results_mines_group_crisis_csad_tbl
  ),
  graphs = list(
    results_all_gg = results_all_gg,
    results_all_crisis_gg = results_all_crisis_gg
  )
)

write_rds(artifacts_csad_cssd, file = here("Outputs", "artifacts_csad_cssd.rds"))


