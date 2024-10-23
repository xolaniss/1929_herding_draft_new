# Description
# Scrapping presidential terms and creating dummy - November 2023 Xolani Sibande

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

# scapping
library(rvest)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))


# Import -------------------------------------------------------------
terms <- read_html("https://www.enchantedlearning.com/history/us/pres/list.shtml")

# Cleaning -----------------------------------------------------------------
terms_tbl <- 
  terms %>% 
  html_table() %>% 
  pluck(2) %>% 
  dplyr::select(-`Vice-President`, - President) %>% 
  rename(Term = `Term as President`) %>% 
  filter(Party %in% c("Democrat", "Republican")) %>% 
  mutate(Term = str_replace(Term, "2021-", "2021-2023")) %>% 
  separate(Term, sep = "-", into = c("Term_start", "Term_end")) %>% 
  slice(-9) %>% 
  mutate(Term_end = paste0(Term_end, "-12-31")) %>% 
  mutate(Term_start = parse_date_time(Term_start, orders = "Y")) %>% 
  mutate(Term_end = parse_date_time(Term_end, orders = "Ymd")) %>% 
  mutate(Term_end = Term_end %-time% "1 year") 

terms_tbl

# Transformations --------------------------------------------------------
terms_daily_tbl <- tibble(
  Date = seq(as.Date("1913-01-01"), as.Date("2022-12-30"), by = "day"),
  Party = ifelse(
    Date >= as.Date("1913-01-01") &
      Date <= as.Date("1920-12-31"),
    "Democrat",
    ifelse(
      Date >= as.Date("1921-01-01") &
        Date <= as.Date("1932-12-31"),
      "Republican",
      ifelse(
        Date >= as.Date("1933-01-01") &
          Date <= as.Date("1952-12-31"),
        "Democrat",
        ifelse(
          Date >= as.Date("1953-01-01") &
            Date <= as.Date("1960-12-31"),
          "Republican",
          ifelse(
            Date >= as.Date("1961-01-01") &
              Date <= as.Date("1968-12-31"),
            "Democrat",
            ifelse(
              Date >= as.Date("1969-01-01") &
                Date <= as.Date("1976-12-31"),
              "Republican",
              ifelse(
                Date >= as.Date("1977-01-01") &
                  Date <= as.Date("1980-12-31"),
                "Democrat",
                ifelse(
                  Date >= as.Date("1981-01-01") &
                    Date <= as.Date("1992-12-31"),
                  "Republican",
                  ifelse(
                    Date >= as.Date("1993-01-01") &
                      Date <= as.Date("2000-12-31"),
                    "Democrat",
                    ifelse(
                      Date >= as.Date("2001-01-01") &
                        Date <= as.Date("2008-12-31"),
                      "Republican",
                      ifelse(
                        Date >= as.Date("2009-01-01") &
                          Date <= as.Date("2016-12-31"),
                        "Democrat",
                        ifelse(
                          Date >= as.Date("2017-01-01") &
                            Date <= as.Date("2020-12-31"),
                          "Republican",
                          ifelse(
                            Date >= as.Date("2021-01-01") &
                              Date <= as.Date("2022-12-31"),
                            "Democrat",
                            NA
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
) %>% 
  mutate(
    Dummy = ifelse(Party == "Democrat", 1, 0)
  )

# EDA ---------------------------------------------------------------
terms_tbl %>% skim()
terms_daily_tbl %>%  skim()

# Graphing ---------------------------------------------------------------
terms_daily_tbl %>% 
  ggplot(aes(x = Party)) +
  geom_bar()

# Export ---------------------------------------------------------------
artifacts_presidential_terms <- list (
  terms_tbl = terms_tbl,
  terms_daily_tbl = terms_daily_tbl
)

write_rds(artifacts_presidential_terms, file = here("Outputs", "artifacts_presidential_terms.rds"))


