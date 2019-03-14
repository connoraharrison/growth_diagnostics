###############################
#  Jordan Growth Diagnostics  #
###############################

###############################
#
# Author: Connor Harrison
# Date: March 12, 2019
# Source: https://data.worldbank.org/country/jordan
#
###############################

# Load Packages
library(tidyverse)
library(readr)
library(readxl)
library(ggthemes)

# Load Data - Jordan Development Indicators
wb_jordan <- read_excel("API_JOR_DS2_en_excel_v2_10474098.xls", range = "A4:BK1603")

# Load Data - Gdp Growth by Country
world_gdp <- read_excel("API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_10473736.xls", range = "A4:BK268")

#################
# Data Cleaning #
#################

### Reshape Data

# Annual GDP Growth: Reformat from Wide to Long
gdp_long <- gather(world_gdp, Year, Value, `1960`:`2018`)

# WDI: Reformat from Wide to Long
wb_jordan_long <- gather(wb_jordan, Year, Value, `1960`:`2018`)

### Keep peer group data








