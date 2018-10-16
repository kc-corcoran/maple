library(tidyverse)
library(lubridate)
library(reshape)

df1 <- read.csv('Raw Data/1479393.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df2 <- read.csv('Raw Data/1479918.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df3 <- read.csv('Raw Data/1479932.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df4 <- read.csv('Raw Data/1479937.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df5 <- read.csv('Raw Data/1479939.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df6 <- read.csv('Raw Data/1479973.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df7 <- read.csv('Raw Data/1480962.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df8 <- read.csv('Raw Data/1480968.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))
df9 <- read.csv('Raw Data/1480972.csv', stringsAsFactors = FALSE, colClasses = c(DATE = "Date"))

all <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)

unique(all$STATION)

#levels(all$NAME)
#levels(all$STATION)
#unique(all$STATION)
#glimpse(all)
#all %>% mutate_if(is.factor, as.character) -> all

all$TMIN_ATTRIBUTES <- NULL
all$TMAX_ATTRIBUTES <- NULL
all$TOBS_ATTRIBUTES <- NULL
all$TOBS <- NULL
all$TAVG_ATTRIBUTES <- NULL
all$TAVG <- NULL




glimpse(all)

keep_stations <- all[which(all$STATION %in% c('USC00437054','USC00434120','USC00432769','USC00432773','USC00435542','USC00436893','USC00436995','USW00014742','USC00439591','US1VTWH0012','USC00437607','USC00438556','USC00436335','USC00437612','USC00435416','USW00094705')),]
glimpse(keep_stations)

keep_stations <- keep_stations[which(!(keep_stations$STATION == 'USC00432773' & keep_stations$DATE <= '2017-07-11')),]
glimpse(keep_stations)

keep_stations <- keep_stations[which(!(keep_stations$STATION == 'US1VTWH0012' & keep_stations$DATE <= '2012-03-31')),]
glimpse(keep_stations)

keep_stations <- keep_stations[which(keep_stations$DATE >'1992-01-01'),]

table(keep_stations$STATION, strftime(keep_stations$DATE,format='%Y'))['USC00434120',]

table(keep_stations$STATION, strftime(keep_stations$DATE,format='%Y'))



nrow(all[which(all$DATE == '2016-01-01'),])

#' A metadata file of the stations and their counties
metadata.columns = c(STNIDNUM    =  8, -1,
                     RECTYPE     =  2, -1,
                     COOPID      =  6, -1,
                     CLIMDIV     =  2, -1,
                     WBANID      =  5, -1,
                     WMOID       =  5, -1,
                     FAAID       =  4, -1,
                     NWSID       =  5, -1,
                     ICAOID      =  4, -1,
                     COUNTRYNAME = 20, -1,
                     STATEPROV   =  2, -1,
                     COUNTY      = 30)

#' State codes
states = read.fwf('Raw Data/ghcnd-states.txt', widths=c(2, -1, 1e2), col.names=c('STATEPROV','State'), header=F)

#' Full station names (For looking up the station identifier...
#' MSHR has the COOPID, NOT the  "GHCN Daily Identification Number". But they 
#' are related, for example: If the GHCN number is ACW00011604, then the
#' COOPID appears to be 011604---the last six characters)

# IV. FORMAT OF "ghcnd-stations.txt"
# 
# ------------------------------
# Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEVATION    32-37   Real
# STATE        39-40   Character
# NAME         42-71   Character
# GSN FLAG     73-75   Character
# HCN/CRN FLAG 77-79   Character
# WMO ID       81-85   Character
# ------------------------------
ghcnd.stations.columns = c(GHCNID       = 11, -1,
                           LATITUDE     =  8, -1,
                           LONGITUDE    =  9, -1,
                           ELEVATION    =  6, -1,
                           STATE        =  2, -1,
                           NAME         = 40, -1,
                           'GSN FLAG'   =  3, -1,
                           'HCN/CRN FLAG' = 3, -1,
                           'WMO ID'      = 5)
ghcnd.stations = read.fwf('Raw Data/ghcnd-stations.txt',
                          comment.char='', # This file contains a # symbol
                          widths = ghcnd.stations.columns,
                          col.names = names(ghcnd.stations.columns)[names(ghcnd.stations.columns)!=''],
                          stringsAsFactors=F,
                          header=F) %>%
  mutate(COOPID = str_sub(GHCNID, -6))

stations.metadata = read.fwf('Raw Data/mshr_standard.txt',
                             widths=metadata.columns,
                             col.names=names(metadata.columns)[names(metadata.columns)!=''],
                             stringsAsFactors=F,
                             colClasses=rep('character', 2),
                             header=F,
                             skip=1) %>%
#' Only first occurance of any COOPID
  filter(!duplicated(COOPID)) %>%
#' Artifact of fixed-width metadata file
  mutate(COUNTY = str_trim(COUNTY)) %>%
  left_join(states, by='STATEPROV')
  
write.csv(stations.metadata, file='Raw Data/mshr_standard.csv', row.names=F)

#' Append the county of each station
keep_stations %<>% mutate(COOPID = str_sub(STATION, -6)) %>%
#' dplyr join in the metadata, for the counties
  left_join(stations.metadata, by='COOPID') %>%
#' Also, use the winter year
  mutate(WINTER.YEAR = ifelse(month(DATE) < 6, year(DATE), year(DATE) + 1))
  
#' The number of temperature readings, per year, for each COUNTY
table(keep_stations$COUNTY, strftime(keep_stations$DATE,format='%Y'))

#' Franklin County produces the most maple syrup, so needs the highest coverage
table(strftime(keep_stations$DATE[str_trim(keep_stations$COUNTY)=='FRANKLIN'], format='%Y%m'))

#' The number of days with below freezing temperatures for the *winter ending in the given year*
below.freezing = keep_stations %>%
  group_by(COUNTY, WINTER.YEAR) %>%
  summarize(N.FREEZING = sum(TMIN < 32, na.rm=T),
            N.ABOVE.FREEZING = sum(TMAX > 32, na.rm=T),
            N.VERY.FREEZING = sum(TMIN < 22, na.rm=T),
            N.STAYS.FREEZING = sum(TMAX < 32, na.rm=T),
            N.STAYS.VERY.FREEZING = sum(TMAX < 22, na.rm=T),
            N.FREEZE.THAW = sum(TMIN < 32 & TMAX >= 32, na.rm=T),
            N.FREEZE.VERY.THAW = sum(TMIN < 32 & TMAX >= 40, na.rm=T),
            AVE.MIN = mean(TMIN, na.rm=T),
            AVE.MAX = mean(TMAX, na.rm=T),
            N.DEFROST = sum(lag(TMAX, order_by=DATE) < 35 & TMIN >= 30, na.rm=T))

df3=keep_stations %>% filter(COUNTY=='FRANKLIN')
