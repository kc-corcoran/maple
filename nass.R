#' Inspect NASS data
#' 

library(tidyverse)

data = read.csv('All Public Maple Syrup Data.csv') %>%
       filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
       filter(Data.Item == 'MAPLE SYRUP - YIELD, MEASURED IN GALLONS / TAP')


ggplot(data, aes(x=Year, y=Value, group=State, color=State)) + geom_line()
