# Header ------------------------------------------------------------------
#
# Script name:
# Script description:
#
# Author: Justin Tran
#
# Notes:
#
#
#

# Dependencies ------------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}

# Load packages (CRAN)
pacman::p_load(tidyverse, readabs, lubridate, cowplot, shiny, shinyWidgets, DT)

# Load packages (Github)
pacman::p_load_current_gh()

# Personal package with useful functions
try(devtools::install_github("justinduytran/R-package",
                             upgrade = FALSE))

# @@ SCRIPT @@ ------------------------------------------------------------

location <- "Sydney"
CPI_subset <- "All groups CPI"
index_period <- dmy(paste(1, 3, 2008, sep = "-"))

# For input
target_rate_percent <- 2.5
target_rate_upper_percent <- 3
target_rate_lower_percent <- 2

# Convert inputs to rates
target_rate <- 1+target_rate_percent/100
target_rate_upper <- 1+target_rate_upper_percent/100
target_rate_lower <- 1+target_rate_lower_percent/100

# CPI data is quarterly
interval <- 4
# Calculate effective interest rate per quarter
effective_target_rate <- target_rate^(1/interval)
effective_target_rate_upper <- target_rate_upper^(1/interval)
effective_target_rate_lower <- target_rate_lower^(1/interval)

CPI <- readabs::read_abs("6401.0",9)

# Filter according to location and subset
CPI_filtered <- CPI |>
  readabs::separate_series() |>
  filter(series_2 == CPI_subset) |>
  filter(series_3 == location) |>
  select(date, value)

CPI_targeted <- CPI_filtered |>
  # Index to specified starting date
  mutate(index = value / CPI_filtered$value[CPI_filtered$date == index_period]*100) |>
  # Set up quarterly compounding periods
  mutate(elasped_quarters = (year(date) - year(index_period))*4 + (month(date) - month(index_period))/3) |> 
  mutate(inflation_target = (effective_target_rate)^elasped_quarters * 100) |> 
  mutate(inflation_target_upper_bound = (effective_target_rate_upper)^elasped_quarters * 100) |> 
  mutate(inflation_target_lower_bound = (effective_target_rate_lower)^elasped_quarters * 100)
  
figure <- CPI_targeted |> 
  ggplot(aes(x = date, y = index, ymax = `inflation_target_upper_bound`, ymin = `inflation_target_lower_bound`)) +
  geom_ribbon(fill = "#EEEEEE") +
  geom_line(mapping = aes(y = inflation_target), colour = "darkgrey", lty = 5) +
  geom_line() + 
  theme_cowplot() +
  scale_x_date(limits = c(index_period, max(CPI_targeted$date))) +
  scale_y_continuous(limits = c(90, NA))
figure
