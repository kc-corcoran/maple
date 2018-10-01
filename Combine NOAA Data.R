library(dplyr)
library(reshape)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

df1 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1479393.csv')
df2 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1479918.csv')
df3 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1479932.csv')
df4 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1479937.csv')
df5 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1479939.csv')
df6 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1479973.csv')
df7 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1480962.csv')
df8 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1480968.csv')
df9 <- read.csv('/Users/casey.corcoran/Documents/maple/Raw Data/1480972.csv')

all <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9)

levels(all$NAME)
levels(all$STATION)
unique(all$STATION)
glimpse(all)
all %>% mutate_if(is.factor, as.character) -> all

all$TMIN_ATTRIBUTES <- NULL
all$TMAX_ATTRIBUTES <- NULL
all$TOBS_ATTRIBUTES <- NULL
all$TOBS <- NULL
all$TAVG_ATTRIBUTES <- NULL
all$TAVG <- NULL


glimpse(all)

nrow(all[which(all$DATE == '2016-01-01'),])
