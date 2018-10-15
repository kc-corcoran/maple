#!/bin/bash

# usage ./extract.NOAA.sh 2000 2018

echo Downloading NOAA data for years $1--$2

cd "Raw Data"

for year in `seq $1 $2`
do
  curl -O ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/$year.csv.gz &&
  gunzip $year.csv
done


