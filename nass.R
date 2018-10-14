#' Inspect NASS data
#' 
#' Census years: 1997, 2002, 2007, 2012
#' Survey years: 
#' Note: There was a census of production at the state level for 2014, 2015, and 2016

library(tidyverse)
library(padr) # Time series interpolation
library(zoo)
source('Combine NOAA Data.R')

data = read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
       filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
       filter(Data.Item == 'MAPLE SYRUP - YIELD, MEASURED IN GALLONS / TAP') %>%
       filter(State == 'VERMONT')

read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
  filter(Program == 'SURVEY' & Geo.Level == 'STATE')
#' The number of taps by county as a proportion of the total
taps.by.county = read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
  filter(Program == 'CENSUS' & Geo.Level == 'COUNTY' & State=='VERMONT') %>%
  filter(Data.Item == 'MAPLE SYRUP - NUMBER OF TAPS') %>%
  mutate(Year = make_datetime(Year)) %>%
  mutate(Value = as.numeric(gsub(',','',Value))) %>%
  group_by(Year, County) %>%
  summarize(taps = sum(Value)) %>%
  mutate(share = taps / sum(taps))

#ggplot(aes(x=Year, y=share, group=County, fill=County)) + geom_bar(position='fill', stat='identity')
  


# ggplot(data, aes(x=Year, y=taps, group=State, color=State)) + geom_bar(position='fill')


read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
  filter(Program == 'CENSUS' & Geo.Level == 'STATE' & State=='VERMONT') %>%
  
  select(Year)


#' Import NOAA: number of freezings weighted by county
#' 


#' Insert missing years into number of taps by county
freezing.scores = taps.by.county %>%
  group_by(County) %>%
  pad('year', by='Year', end_val=make_datetime(2018)) %>%
  mutate(share = na.approx(share, rule=2), taps = na.approx(taps, rule=2)) %>% # rule=2 state.approx: extrapolate max value
#' The "Freezing score" for the state of vermont, by year
  mutate(WINTER.YEAR = year(Year)) %>%
  inner_join(below.freezing, by=c(WINTER.YEAR='WINTER.YEAR', County='COUNTY')) %>%
#' We're going to take all the stats and combine them
#' into a state-wide metric  
  gather(key='key', value='value', -Year : -WINTER.YEAR) %>%
  group_by(WINTER.YEAR, key) %>%
  summarize(value = sum(value * share)) %>%
  ungroup() %>%
  mutate(key = paste0('COUNTY.WEIGHTED.',key)) %>%
  spread(key, value) 

#' Join the freezing score w/ the yield
data = read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
  filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
  filter(State == 'VERMONT') %>%
  filter(Data.Item == 'MAPLE SYRUP - YIELD, MEASURED IN GALLONS / TAP') %>%
  mutate(Year = make_datetime(Year)) %>%
  mutate(WINTER.YEAR = year(Year)) %>%
  mutate(Value = as.numeric(as.character(Value))) %>%
  select(WINTER.YEAR, Yield=Value) %>%
  inner_join(freezing.scores, by='WINTER.YEAR')
pairs(~ Yield +
        COUNTY.WEIGHTED.AVE.MAX +
        COUNTY.WEIGHTED.N.DEFROST +
        COUNTY.WEIGHTED.N.VERY.FREEZING, data)
plot(data)

write.csv(data, file='output/training.v2.csv', row.names=F)
  
#' Use tidyr to turn data into wide format
df4 = read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
  filter(Program == 'SURVEY' & Period == 'YEAR' & Geo.Level == 'STATE') %>%
  select(Year, State, Data.Item, Value) %>%
  spread(Data.Item, Value)
