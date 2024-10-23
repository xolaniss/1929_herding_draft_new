# Description
# Estimating volatility using GARCH (1,1) model from the CSRP returns by Xolani Sibande - 2021-09-30

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
library(moments)
library(rugarch)

options(scipen = 999)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
fama_french <- read_rds(here("Outputs", "artifacts_fama_french.rds"))

# Cleaning -----------------------------------------------------------------
market_return_tbl <- 
  fama_french$data$fama_french_tbl %>% 
  mutate(market_return = `Mkt-RF` + RF) %>%
  dplyr::select(Date, market_return) %>% 
  drop_na()

# Graphing --------------------------------------------------------
market_return_gg <- 
  market_return_tbl %>% 
  fx_plot()

# EDA ---------------------------------------------------------------
market_return_tbl %>% skim()

# Tests -------------------------------------------------------------
# density plot for market returns
market_return_density <- 
  market_return_tbl %>% 
  ggplot(aes(x = market_return)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = median(market_return_tbl$market_return), color = "blue", linetype = "dashed") +
  labs(title = "Density plot for market returns", x = "Market returns", y = "Density") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# normality test for market returns
jarque.bera.test(market_return_tbl$market_return)

# test for normality using qqplot
qqnorm(market_return_tbl$market_return)
qqline(market_return_tbl$market_return)

# kurtosis test for market returns
kurtosis(market_return_tbl$market_return)

# skewness test for market returns
skewness(market_return_tbl$market_return)

# arch effects test
Box.test(market_return_tbl$market_return, type="Ljung-Box", lag = 12)

# Augmented Dickey-Fuller test for stationarity
adf.test(market_return_tbl$market_return, alternative = "stationary")

# sGARCH ---------------------------------------------------------------
# specify GARCH model
spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                  mean.model=list(armaOrder=c(0,0)),
                  distribution.model="std", )

# fit GARCH model
fit = ugarchfit(spec, market_return_tbl$market_return)
fit

# diagnostic plots
plot(fit, which = "all")

# create sigma tibble
volatility_tbl <- sigma(fit) %>%  
  as_tibble() %>%  
  mutate(Date = market_return_tbl$Date) %>% 
  # rename V1 to sigma
  rename(`Volatility of market returns (GARCH(1,1))` = V1) %>%
  # relocate sigma to after date
  relocate(Date, .before = `Volatility of market returns (GARCH(1,1))`)
  

# plot sigma tibble
volatility_gg <- 
  volatility_tbl %>% 
  fx_plot() +
  labs(y = "Volatility") +
  theme(plot.title = element_text(hjust = 0.5))

volatility_gg

# Export ---------------------------------------------------------------
artifacts_volatility <- list (
 data = list(
   volatility_tbl = volatility_tbl
   ),
 graphs = list(
   volatility_gg = volatility_gg
 )
)

write_rds(artifacts_volatility, file = here("Outputs", "artifacts_volatility.rds"))


