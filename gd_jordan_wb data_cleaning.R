###############################
#  Jordan Growth Diagnostic   #
###############################

###############################
#
# Author: Connor Harrison
# Date: March 12, 2019
# Sources: https://data.worldbank.org/country/jordan
#        : https://data.worldbank.org/indicator/ny.gdp.pcap.cd
#        : http://worldpopulationreview.com/countries/countries-by-gdp/
#        : http://worldpopulationreview.com/countries/
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

# Load Data - GDP per capita
gdp_pc <- read_excel("API_NY.GDP.PCAP.KD_DS2_en_excel_v2_10473669.xls", range = "A4:BK268")

#################
# Data Cleaning #
#################

### Reshape Data

# Annual GDP Growth: Reformat from Wide to Long
gdp_growth_long <- gather(world_gdp, Year, Value, `1960`:`2018`)

# WDI: Reformat from Wide to Long
wb_jordan_long <- gather(wb_jordan, Year, Value, `1960`:`2018`)

# GDP PC: Reformat Wide to Long
gdp_pc_long <- gather(gdp_pc, Year, Value, `1960`:`2018`)

### Peer Group Option 1: Similar per capita GDP in 2018

  ###########################
  # By similar GDP Per capita
  # Source: http://worldpopulationreview.com/countries/countries-by-gdp/
  # Sri Lanka: 4,741
  # Samoa: 4,625
  # Georgia: 4,465
  # Jordan: 4,448
  # Mongolia: 4,354
  # Armenia: 4,323
  # Indonesia: 4,277
  ############################

### Peer Group Option 2: Regional neighbors w/ population, cultural, and resource similarities

  ###########################
  # By Region and Population
  # Source: http://worldpopulationreview.com/countries/
  # Azerbaijan
  # Lebanon
  # Indonesia
  # Egypt
  # Georgia
  # Morocco
  # Jordan
  ############################

gdp_jordan_peer1 <- filter(gdp_pc_long, `Country Name`=="Sri Lanka" | `Country Name`=="Samoa" | `Country Name`=="Georgia"
                          | `Country Name`=="Jordan" | `Country Name`=="Mongolia" | `Country Name`=="Armenia" | 
                            `Country Name`=="Indonesia")


gdp_jordan_peer2 <- filter(gdp_pc_long, `Country Name`=="Azerbaijan" | `Country Name`=="Lebanon" | `Country Name`=="Indonesia"
                          | `Country Name`=="Jordan" | `Country Name`=="Egypt" | `Country Name`=="Georgia" | 
                            `Country Name`=="Morocco")

  # Varying Levels of missing data. Collection begins at different periods.

# Jordan GDP Growth
gdp_jordan <- filter(gdp_pc_long, `Country Name` == "Jordan")
ggplot(data = gdp_jordan,
       mapping = aes(x = as.numeric(Year), y = Value)) +
  geom_line(color = "dodgerblue3", size = 2, alpha = 1) +
  scale_x_continuous(limits = c(1970, 2020), breaks = c(1970, 1980, 1990, 2000, 2010, 2020)) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Jordan's Growth Over Time",
     subtitle = "Per Capita GDP in constant 2010 US Dollars",
      caption = "Source = World Bank Open Data")

# GDP Growth Rate Plot 
#
#ggplot(data = gdp_growth_long, 
#       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
#  geom_line(size = 1.5, alpha = 0.5) +
#  theme_fivethirtyeight() +
#  facet_wrap(~`Country Name`) +
#  theme(legend.position = 'none') +
#  labs(title = )
  
# 1. GDP Per Capita 
    # Peer 1
# 1 Graph
ggplot(data = gdp_jordan_peer1, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank()) +
  scale_color_brewer(name = "", palette = "Dark2") +
  labs(title = "Growth Over Time",
       subtitle = "Per Capita GDP for Jordan and selected Peers", 
       caption = "Source = World Bank Open Data")

# Faceted
ggplot(data = gdp_jordan_peer1, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none', panel.grid.major.x = element_blank()) +
  scale_color_brewer(name = "", palette = "Dark2") +
  facet_wrap(~`Country Name`) +
  labs(title = "Growth Over Time",
       subtitle = "Per Capita GDP for Jordan and selected Peers", 
       caption = "Source = World Bank Open Data")


    # Peer 2
# 1 Graph
ggplot(data = gdp_jordan_peer2, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  theme_fivethirtyeight() +
  scale_color_brewer(name = "", palette = "Dark2") +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Growth Over Time",
       subtitle = "Per Capita GDP for Jordan and selected Peers", 
       caption = "Source = World Bank Open Data")

# Individual Country Graph
ggplot(data = gdp_jordan_peer2, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  theme_fivethirtyeight() +
  facet_wrap(~`Country Name`) +
  theme(legend.position = 'none', panel.grid.major.x = element_blank()) +
  labs(title = "Growth Over Time",
       subtitle = "Per Capita GDP for Jordan and selected Peers", 
       caption = "Source = World Bank Open Data")

###############################
# Development Indicators
###############################

# Adult Literacy Rate


# Government Expenditure on Education as % of GDP

# Urban Population (% of Total)
jordan_urban <- filter(wb_jordan,`Indicator Code`== "SP.URB.TOTL.IN.ZS")
jordan_urban_l <- gather(jordan_urban, Year, Value, `1960`:`2018`)

ggplot(data = jordan_urban_l,
       mapping =aes(x = as.numeric(Year), y = Value)) +
         geom_line(color = "dodgerblue3", size = 2, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none', panel.grid.major.x = element_blank()) +
  labs(title = "Jordanian Urbanization",
       subtitle = "Percentage of the Population Living in Urban Areas, 1960-2018", 
       caption = "Source: World Bank Open Data")

# Adult Literacy (Lots of Missing Data)
jordan_lit <- filter(wb_jordan,`Indicator Code`== "SE.ADT.LITR.ZS")
jordan_lit_l <- gather(jordan_lit, Year, Value, `1960`:`2018`)

ggplot(data = jordan_lit_l,
       mapping =aes(x = as.numeric(Year), y = Value)) +
  geom_line(color = "dodgerblue3", size = 2, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none', panel.grid.major.x = element_blank()) +
  labs(title = "Jordanian Literacy",
       subtitle = "Adult Literacy Rate, Age 15+, 1960-2018", 
       caption = "Source: World Bank Open Data")


# Child Mortality
jordan_cm <- filter(wb_jordan,`Indicator Code`== "SH.DYN.NMRT")
jordan_cm_l <- gather(jordan_cm, Year, Value, `1960`:`2018`)

ggplot(data = jordan_cm_l,
       mapping =aes(x = as.numeric(Year), y = Value)) +
  geom_line(color = "dodgerblue3", size = 2, alpha = 1) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none', panel.grid.major.x = element_blank()) +
  labs(title = "Jordanian Infant Mortality",
       subtitle = "Neonatal Mortality Rate per 1,000 Live Births, 1960-2018", 
       caption = "Source: World Bank Open Data")



####
# Indicator Peer Comparisons
#

urban <- read_excel("API_SP.URB.TOTL.IN.ZS_DS2_en_excel_v2_10473911.xls", range = "A4:BK268")
urban_peer <- filter(urban, `Country Name`=="Azerbaijan" | `Country Name`=="Lebanon" | `Country Name`=="Indonesia"
                     | `Country Name`=="Jordan" | `Country Name`=="Egypt" | `Country Name`=="Georgia" | 
                       `Country Name`=="Morocco")
urban_peer_l <- gather(urban_peer, Year, Value, `1960`:`2018`)

# % Urban over time for peer group
ggplot(data = urban_peer_l, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  facet_wrap(~`Country Name`) +
 theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(), legend.position = "") +
  scale_color_brewer(name = "", palette = "Dark2") +
  labs(title = "Urbanization Comparison",
       subtitle = "Percent of the Population living in Urban Areas; Peer Group 2", 
       caption = "Source = World Bank Open Data")

# Fertility Comparison: Peer Group 2
fertility <- read_excel("API_SP.DYN.TFRT.IN_DS2_en_excel_v2_10473791.xls", range = "A4:BK268")
fertility_peer <- filter(fertility, `Country Name`=="Azerbaijan" | `Country Name`=="Lebanon" | `Country Name`=="Indonesia"
                     | `Country Name`=="Jordan" | `Country Name`=="Egypt" | `Country Name`=="Georgia" | 
                       `Country Name`=="Morocco")
fertility_peer_l <- gather(fertility_peer, Year, Value, `1960`:`2018`)

# Fertility Graph: Peer Group 2
ggplot(data = fertility_peer_l, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  facet_wrap(~`Country Name`) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(), legend.position = "") +
  scale_color_brewer(name = "", palette = "Dark2") +
  labs(title = "Fertility Comparison",
       subtitle = "Births Per Woman; Peer Group 2", 
       caption = "Source = World Bank Open Data")


# Literacy Comparison: Peer Group 2
literacy <- read_excel("API_SE.ADT.LITR.ZS_DS2_en_excel_v2_10474127.xls", range = "A4:BK268")
literacy_peer <- filter(literacy, `Country Name`=="Azerbaijan" | `Country Name`=="Lebanon" | `Country Name`=="Indonesia"
                         | `Country Name`=="Jordan" | `Country Name`=="Egypt" | `Country Name`=="Georgia" | 
                           `Country Name`=="Morocco")
literacy_peer_l <- gather(literacy_peer, Year, Value, `1960`:`2018`)

# Adult Literacy Graph: Peer Group 2
ggplot(data = literacy_peer_l, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  facet_wrap(~`Country Name`) +
  scale_x_continuous(limits = c(2000, 2020), breaks = c(2000, 2010, 2020)) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(), legend.position = "") +
  scale_color_brewer(name = "", palette = "Dark2") +
  labs(title = "Literacy Comparison",
       subtitle = "Percent of Literate Adults, Ages 15+; Peer Group 2", 
       caption = "Source = World Bank Open Data")



# Manufacturing as share of GDP Comparison: Peer Group 2
manuf <- read_excel("API_NV.IND.MANF.ZS_DS2_en_excel_v2_10474606.xls", range = "A4:BK268")
manuf_peer <- filter(manuf, `Country Name`=="Azerbaijan" | `Country Name`=="Lebanon" | `Country Name`=="Indonesia"
                        | `Country Name`=="Jordan" | `Country Name`=="Egypt" | `Country Name`=="Georgia" | 
                          `Country Name`=="Morocco")
manuf_peer_l <- gather(manuf_peer, Year, Value, `1960`:`2018`)

# Manufacturing Value Add Graph: Peer Group 2
ggplot(data = manuf_peer_l, 
       mapping = aes(x = as.numeric(Year), y = Value, group=`Country Name`, color = `Country Name`)) +
  geom_line(size = 1.5, alpha = 0.7) +
  facet_wrap(~`Country Name`) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(), legend.position = "") +
  scale_color_brewer(name = "", palette = "Dark2") +
  labs(title = "Manufacturing Comparison",
       subtitle = "Value Added of the Manufacturing Sector\nas % of GDP; Peer Group 2", 
       caption = "Source = World Bank Open Data")

# Filter GDP
gdp_peer <- filter(world_gdp, `Country Name`=="Azerbaijan" | `Country Name`=="Lebanon" | `Country Name`=="Indonesia"
                           | `Country Name`=="Jordan" | `Country Name`=="Egypt" | `Country Name`=="Georgia" | 
                             `Country Name`=="Morocco")

# Bind Data for Scatter Plots
scatter <- bind_rows(fertility_peer, urban_peer, gdp_peer, manuf_peer)
scatter_peer_l <- gather(scatter, Year, Value, `1960`:`2018`)
