library(dplyr)
library(reshape)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

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
