######################################################################
## Project: Manila Sensors Air Pollution Study
## Script purpose: Analyze backward trajectories leading to Manila sensors
## Date: September 2020
## Author: Hubert Thieriot (hubert@energyandcleanair.org)
######################################################################

# Installing packages (only need to run once)
install.packages(c("tibble","dplyr","tidyr"))
if(!require(rcrea)){
  devtools::install_github("https://github.com/energyandcleanair/rcrea", force=T)
}

# Load packages
require(tibble)
require(rcrea)
require(dplyr)
require(tidyr)

# Prepare
source('./utils.plots.R')
dir.create("results", showWarnings = F)

# Read measurements
stations <- read.stations()
meas <- read.measurements()

# Filtering out values above thresholds
indicators <- c("pm25", "pm10", "aqi.cn", "aqi.us")
limit <- tibble(indicator=c("pm25", "pm10", "aqi.cn", "aqi.us"),
                limit=c(500, 800, 300, 300))
meas <- meas %>%
  left_join(limit) %>%
  filter(indicator %in% indicators,
         value < limit)

# Plot
plot.ts(n_days=7)
plot.ts(n_days=30)
