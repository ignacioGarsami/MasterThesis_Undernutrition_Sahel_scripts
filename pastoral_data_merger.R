#
# Mixed model data merger
# -> Script to create an initiadata set to study pastoral information and relations with malnutrition and biomass.
# -> Explore all the different available data and collect only the interesting information.
# Author: Ignacio Garcia Sanchez-Migallon
# Date: 03/06/2020
# Version: 0.2v
#

rm(list=ls())

library(raster)
library(sf)
library(tidyverse)
library(reshape)
library(dplyr)
library(naniar)
library(readxl)
library(writexl)

################################################################################################
#Time span of the different data sources:
#2019 and 2020, every two months 
#
#################################################################################################

# Data loading ------------------------------------------------------------


setwd("C:/Users/IgnacioGarcia/Desktop/UNIVERSIDAD/Statistics for data science/Master thesis/data")

#Pastoral data
pastoral = read.csv('Pastoral_data/sites_geosahel_2019.csv', sep = ';',na.strings=c("","NA"))

pastoral = pastoral %>%
  dplyr::rename(
    'admin0Name' = 'ï..admin0Name',
  )


#Biomass data

biomass =  readxl::read_xlsx('Biomass_data/bio_adm2-2019-formatted.xlsx')

biomass = subset(biomass, select=c('admin0Name','admin1Name','admin2Name','AREA','Mean','2019'))

data = biomass %>% right_join(pastoral, by = c('admin0Name',"admin1Name",'admin2Name'))

data$biomass_anomaly = (data$`2019` -  data$Mean)
data$biomass_anomaly_perc = (data$`2019` -  data$Mean ) / data$Mean


#write_xlsx(data,'Pastoral_data/pastoral_data.xlsx')


# Raster data ------------------------------------------------------------


setwd("C:/Users/IgnacioGarcia/Desktop/UNIVERSIDAD/Statistics for data science/Master thesis/data")

library(gstat)
library(tidyverse)
library(fasterize)
library(tiff)
library(raster)


data_biomass =  st_read('Biomass_data/BIO_ADM2')
data_pastoral = st_read('Pastoral_data/sites_geosahel_octNov2019')

data_biomass$BIO_2019 = data_biomass$BIO_2019

raster_biomass = raster(data_biomass, res = 1/100)
raster_pastoral = raster(data_pastoral, res = 1/100)

#Use of fasterize instead of rasterize because it is WAY faster
rasterize_biomass = fasterize(data_biomass, raster_biomass, field = 'BIO_2019')

data = data_pastoral %>% st_join(data_biomass)

#Pastoral data
pastoral = read.csv('Pastoral_data/sites_geosahel_2019.csv', sep = ';',na.strings=c("","NA"))

pastoral = pastoral %>%
  dplyr::rename(
    'admin0Name' = 'ï..admin0Name',
  )


#Biomass data

biomass =  readxl::read_xlsx('Biomass_data/bio_adm2-2019-formatted.xlsx')

biomass = subset(biomass, select=c('admin0Name','admin1Name','admin2Name','AREA','Mean','2019'))

data = biomass %>% right_join(pastoral, by = c('admin0Name',"admin1Name",'admin2Name'))

data$biomass_anomaly = (data$`2019` -  data$Mean)
data$biomass_anomaly_perc = (data$`2019` -  data$Mean ) / data$Mean


#write_xlsx(data,'Pastoral_data/pastoral_data.xlsx')


