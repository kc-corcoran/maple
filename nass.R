#' Inspect NASS data
#' 
#' Census years: 1997, 2002, 2007, 2012
#' Survey years: 
#' Note: There was a census of production at the state level for 2014, 2015, and 2016

library(tidyverse)
library(padr) # Time series interpolation
library(zoo)
source('Combine NOAA Data.R')

maple.raw = read.csv('Raw Data/All Public Maple Syrup Data.csv',
                     stringsAsFactors=F,
                     strip.white=T,
                     na.strings=c('', ' ','(NA)','(D)')) # (D) means too small to count, but positive, or something

total.maple = maple.raw %>%
  filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
  filter(Data.Item == 'MAPLE SYRUP - PRODUCTION, MEASURED IN GALLONS') %>%
  mutate(Value = as.numeric(gsub(',','',Value))) %>%
  group_by(State) %>%
  summarize(Total.Maple.Production = sum(Value))

maple.states = unique(total.maple$State)

data = read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
       filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
       filter(Data.Item == 'MAPLE SYRUP - YIELD, MEASURED IN GALLONS / TAP') %>%
       filter(State == 'VERMONT')

# read.csv('Raw Data/All Public Maple Syrup Data.csv') %>%
#   filter(Program == 'SURVEY' & Geo.Level == 'STATE')

#' The number of taps by county as a proportion of the total
taps.by.county = maple.raw %>%
  filter(Program == 'CENSUS' & Geo.Level == 'COUNTY') %>%
  filter(Data.Item == 'MAPLE SYRUP - NUMBER OF TAPS') %>%
  mutate(Year = make_datetime(Year)) %>%
  mutate(Value = as.numeric(gsub(',','',Value))) %>%
  group_by(Year, State, County) %>%
  summarize(taps = sum(Value)) %>%
  mutate(share = taps / sum(taps, na.rm=T)) %>%
  na.omit()

#ggplot(aes(x=Year, y=share, group=County, fill=County)) + geom_bar(position='fill', stat='identity')
  


# ggplot(data, aes(x=Year, y=taps, group=State, color=State)) + geom_bar(position='fill')


#' Import NOAA: number of freezings weighted by county
#' 


#' Insert missing years into number of taps by county
freezing.scores = taps.by.county %>%
  filter(State=='VERMONT') %>%
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


#' ## Production by county
library(rgeos)
library(maptools)

library(maps)
library(ggmap)

map = map_data('county', region='vermont') %>%
  mutate(region=subregion)


maple.raw %>%
  filter(Program == 'CENSUS' & Geo.Level == 'COUNTY') %>%
  filter(Data.Item == 'MAPLE SYRUP - NUMBER OF TAPS') %>%
  filter(State == 'VERMONT' & Year == 2012) %>%
  mutate(Value = as.numeric(gsub(',','',Value))) %>%
  ggplot(aes(fill=Value)) + geom_map(aes(map_id=tolower(County)), map=map, color='white') +
  scale_fill_gradient(low = '#0000', high = "#00CC00") +
  expand_limits(x=map$long, y=map$lat) +
  theme_nothing()
