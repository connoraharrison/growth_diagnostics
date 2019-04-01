### Jordan Growth Diagnostic Indicators

library(tidyverse)
library(ggthemes)
library(readr)
gdp <- read_csv("data_records.1553542397.csv")

# GDP over time
ggplot(data = gdp, 
       mapping = aes(x = as.numeric(year), y = value, group=country_name, color = country_name)) +
  geom_line(size = 1.5, alpha = 0.7) +
  labs(title = "Growth Over Time",
       subtitle = "Per Capita GDP for Jordan and selected Peers", 
       caption = "Source = World Bank Open Data")

### High Cost of Finance

# Ration of Externawl Debt to GDP

gdp_to_external_debt <- read_csv("gdp_to_external_debt.csv")
gdp_ext_debt_long <- spread(gdp_to_external_debt, key = country_name, value = value)

# Interest Rates
library(readr)
int_rates <- read_csv("int_rates.csv")
int_rates_wide <- spread(int_rates, key = series_name, value = value)
int_rates_2 <- select(int_rates_wide, is.na(`GDP growth(annual percentage)`))

int_jordan <- filter(int_rates, country_name=="Jordan")
int_only <- filter(int_rates, series_name=="Real interest rate (percentage)")

# Peer Group Interest Rates
ggplot(data = int_only, 
       mapping = aes(x = as.numeric(year), y = value)) +
  geom_line(aes(color=country_name), size = 1.5, alpha = 0.7) +
  theme_fivethirtyeight() +
  labs(title = "Interest Rates",
       subtitle = "Comparison of Real Interest Rates across the Peer Group",
       caption = "Source = USAID Inclusive Growth Diagnostic Indicators") +
theme(panel.grid.major.x = element_blank())

# Interest Rate v. Gdp
ggplot(data = int_jordan, 
  mapping = aes(x = as.numeric(year), y = value)) +
  geom_line(aes(color=series_name), size = 1.5, alpha = 0.7) +
  theme_fivethirtyeight() +
  theme(legend.position = "none") +
  labs(title = "Interest Rate and Growth",
       caption = "Source = USAID Inclusive Growth Diagnostic Indicators")

# Per Capita GDP
ggplot(data = gdp, 
       mapping = aes(x = as.numeric(year), y = value)) +
  geom_line(aes(color=country_name), size = 1.5, alpha = 0.7) +
  theme(panel.grid.major.x = element_blank()) +
  theme_fivethirtyeight() +
  labs(title = "Growth Comparison",
       subtitle = "Comparison of Per Capita GDP across the Peer Group",
       caption = "Source = USAID Inclusive Growth Diagnostic Indicators")
credit2 <- mutate(credit, log(value))
credit3 <- select(credit2, series="series_name", year="year", value=`log(value)`)

# Invest v. GDP
ggplot(data = credit3, 
       mapping = aes(x = as.numeric(year), y = value)) +
  geom_line(aes(color=series), size = 1.5, alpha = 0.7) +
  theme(panel.grid.major.x = element_blank()) +
  theme_fivethirtyeight() +
  labs(title = "Price Constraint",
       subtitle = "Comparison of Real Interest Rate and Investment Levels",
       caption = "Source = USAID Inclusive Growth Diagnostic Indicators")

# Log Interest Rates v. Log Domestic Credit
ggplot(data = credit3, 
       mapping = aes(x = as.numeric(year), y = value)) +
  geom_line(aes(color=series), size = 1.5, alpha = 0.7) +
  theme(panel.grid.major.x = element_blank()) +
  theme_fivethirtyeight() +
  labs(title = "Price Constraint",
       subtitle = "Comparison of Real Interest Rate and Investment Levels",
       caption = "Source = USAID Inclusive Growth Diagnostic Indicators")
