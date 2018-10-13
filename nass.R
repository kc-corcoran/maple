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

# Insert missing years into number of taps by county
df = taps.by.county %>%
  group_by(County) %>%
  pad('year', by='Year', end_val=make_datetime(2018)) %>%
  mutate(share = na.approx(share, rule=2), taps = na.approx(taps, rule=2)) # rule=2 state.approx: extrapolate max value

#' The "Freezing score" for the state of vermont, by year
df2 = df %>%
  mutate(WINTER.YEAR = year(Year)) %>%
  inner_join(below.freezing, by=c(WINTER.YEAR='WINTER.YEAR', County='COUNTY')) %>%
  mutate(Freeze.Score = share * N.FREEZING) %>%
  group_by(WINTER.YEAR) %>%
  summarize(Total.Freeze.Score = sum(Freeze.Score))

#' Join the freezing score w/ the yield
df3 = read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
  filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
  filter(State == 'VERMONT') %>%
  filter(Data.Item == 'MAPLE SYRUP - YIELD, MEASURED IN GALLONS / TAP') %>%
  mutate(Year = make_datetime(Year)) %>%
  mutate(WINTER.YEAR = year(Year)) %>%
  select(WINTER.YEAR, Yield=Value) %>%
  inner_join(df2, by='WINTER.YEAR')
  

