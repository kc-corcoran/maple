#' Find all stations within the counties with maple taps
#' This will be used to narrow down the raw GHCN data
library(tidyverse)
library(readr)

source('Combine NOAA Data.R')
source('nass.R')




#' The states with substantive maple production
keep.states = total.maple %>%
  filter(!is.na(Total.Maple.Production)) %>%
  filter(Total.Maple.Production > quantile(Total.Maple.Production, .5, na.rm=T)) %>%
  select(State)

#' The (state,county) pairs with reported taps
state.county.with.taps = maple.raw %>%
  filter(Program == 'CENSUS' & Geo.Level == 'COUNTY') %>%
  filter(Data.Item == 'MAPLE SYRUP - NUMBER OF TAPS') %>%
  filter(State %in% keep.states$State) %>%
  select(State, County) %>%
  unique()

#' The weather stations in those counties
keep.maple.stations = state.county.with.taps %>%
  left_join(stations.metadata, by=c(State='State', County='COUNTY')) %>%
  left_join(ghcnd.stations, by='COOPID') %>%
  filter(!is.na(GHCNID)) %>%
  select(State, County, GHCNID)

#' Only keep observations of the given stations
callback = function(df.chunk, pos) {
  df.chunk %>% inner_join(keep.maple.stations, by='GHCNID')
}
df = read_csv_chunked('Raw Data/ghcnd.daily.2000-2018.csv',
                 col_names=c('GHCNID', 'DATE', 'KEY', 'VALUE'),
                 col_types=cols(DATE=col_date(format = '%Y%m%d')),
                 DataFrameCallback$new(callback),
                 chunk_size = 1e5)

#' Data looks weird, taking a look
df2 = df %>%
  mutate(WINTER.YEAR = ifelse(month(DATE) < 6, year(DATE), year(DATE) + 1)) %>%
  filter(State=='VERMONT' & County=='FRANKLIN') %>%
  filter(KEY=='TMAX') %>%
  inner_join(ghcnd.stations, by='GHCNID') 
  group_by(WINTER.YEAR, State, County, GHCNID) %>% summarize(AVE.VAL=mean(VALUE, na.rm=T))

#' Average all the stations in the county together
daily.county.weather = df %>%
  group_by(DATE, State, County, KEY) %>%
  summarize(VALUE=head(VALUE, n=1)) %>% #na.rm=T)) %>%
  ungroup() %>%
  mutate(KEY=paste0('CNTY.', KEY)) %>%
  mutate(WINTER.YEAR = ifelse(month(DATE) < 6, year(DATE), year(DATE) + 1)) 

#' Some basic statistic: just the average of each thing throughout the year
winter.county.scores.1 = daily.county.weather %>%
  group_by(State, County, WINTER.YEAR, KEY) %>%
  summarize(VALUE=mean(VALUE, na.rm=T)) %>%
  ungroup() %>%
  mutate(KEY=paste0('AVE.', KEY)) %>%
  spread(key='KEY', value='VALUE')

#' Some more advanced stats (note: units in metric, and degrees are tenths)
winter.county.scores.2 = daily.county.weather %>%
  spread(key='KEY', value='VALUE') %>%
  group_by(State, County, WINTER.YEAR) %>%
  summarize(N.FREEZING = sum(CNTY.TMIN < 0, na.rm=T),
          N.VERY.FREEZING = sum(CNTY.TMIN < -100, na.rm=T),
          N.STAYS.FREEZING = sum(CNTY.TMAX < 0, na.rm=T),
          N.STAYS.VERY.FREEZING = sum(CNTY.TMAX < -100, na.rm=T),
          N.FREEZE.THAW = sum(CNTY.TMIN < 0 & CNTY.TMAX >= 0, na.rm=T),
          N.FREEZE.VERY.THAW = sum(CNTY.TMIN < 0 & CNTY.TMAX >= 100, na.rm=T),
          N.DEFROST = sum(lag(CNTY.TMAX, order_by=DATE) < 50 & CNTY.TMIN >= -50, na.rm=T))

#' Some more stats that should be counted
# winter.county.scores.3 = daily.county.weather %>%
#   spread(key='KEY', value='VALUE') %>%
#   group_by(State, County, WINTER.YEAR) %>%
#   summarize(N.VERY.RAINY = sum(TMIN < 0, na.rm=T))


#' ## NASS data

#' Insert missing years into number of taps by county
freezing.scores.2 = taps.by.county %>%
  group_by(State, County) %>%
  pad('year', by='Year', end_val=make_datetime(2018)) %>%
  mutate(share = na.approx(share, rule=2), taps = na.approx(taps, rule=2)) %>% # rule=2 state.approx: extrapolate max value
  #' The "Freezing score" for each state, by year
  mutate(WINTER.YEAR = year(Year)) %>%
  inner_join(winter.county.scores.1, by=c('WINTER.YEAR', 'State', 'County')) %>%
  inner_join(winter.county.scores.2, by=c('WINTER.YEAR', 'State', 'County')) %>%
  #' We're going to take all the stats and combine them
  #' into a state-wide metric  
  gather(key='key', value='value', -Year : -WINTER.YEAR) %>%
  group_by(State, WINTER.YEAR, key) %>%
  summarize(value = sum(value * share)) %>%
  ungroup() %>%
  mutate(key = paste0('WTD.', key)) %>%
  spread(key, value) 

#' Join the freezing score w/ the yield
data.2 = maple.raw %>%
  filter(Program == 'SURVEY' & Geo.Level == 'STATE') %>%
  filter(Data.Item == 'MAPLE SYRUP - YIELD, MEASURED IN GALLONS / TAP') %>%
  mutate(Year = make_datetime(Year)) %>%
  mutate(WINTER.YEAR = year(Year)) %>%
  mutate(Value = as.numeric(as.character(Value))) %>%
  select(WINTER.YEAR, Yield=Value, State) %>%
  inner_join(freezing.scores.2, by=c('WINTER.YEAR', 'State')) %>%
#' Remove empty columns  
  select_if(~sum(!is.na(.)) > 0)

pairs(~ Yield +
        WTD.AVE.CNTY.PRCP +
        WTD.AVE.CNTY.SNOW +
        WTD.AVE.CNTY.TMIN +
        WTD.N.DEFROST, data.2 %>% filter(State=='VERMONT'))

write.csv(data.2, file='output/training.v3.csv', row.names=F)

#' Adjust for the overall upward trend in yield
# yield.trend = lm(log(Yield) ~ WINTER.YEAR, data.2)
yield.trend = lm(Yield ~ WINTER.YEAR, data.2)

df4 = data.2 %>%
  mutate(Yield = Yield - predict.lm(yield.trend, newdata=.))

# log(Yield) ~ WINTER.YEAR * 0.03688993 - 75.59780881
# Yield ~ exp(WINTER.YEAR * 0.03688993) * exp(-75.59780881)
pairs(~ Yield +
        WTD.AVE.CNTY.PRCP +
        WTD.AVE.CNTY.SNOW +
        WTD.AVE.CNTY.TMIN +
        WTD.N.DEFROST, df4)
write.csv(df4, file='output/training.v4.csv', row.names=F)

