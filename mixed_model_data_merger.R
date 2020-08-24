#
# Mixed model data merger
# -> Script to create an initial longuitudinal data set to study biomass and trends in malnutrition.
# -> Explore all the different available data and collect only the interesting information.
# Author: Ignacio Garcia Sanchez-Migallon
# Date: 11/05/2020
# Version: 0.2v
#

rm(list=ls())

library(raster)
library(sf)
library(tidyverse)
library(reshape)
library(dplyr)
library(naniar)

################################################################################################
#Time span of the different data sources:
# Vulnerability -> 1998-2018
# WorldBank -> 1998-2014
#Biomass -> 1998-2019
#
#Chosen temporal threshold -> 1998-2018
#Biomass of 2019 might be used as a test of the model, as 2019 data can probably be easily found.
#################################################################################################

# Data loading ------------------------------------------------------------


setwd("C:/Users/IgnacioGarcia/Desktop/UNIVERSIDAD/Statistics for data science/Master thesis/data")

#Biomass data
biomass =  readxl::read_xlsx('Biomass_data/bio_adm2-2019-formatted.xlsx')

#Malnutrition status data

#Source: Global data lab WeightXheight
vulnerability_stunted = read.csv('Vulnerability_data/GDL-Percentage-of-stunted-children-data-formatted.csv', sep = ',', check.names = FALSE)
vulnerability_wasted = read.csv('Vulnerability_data/GDL-Percentage-of-wasted-children-data-formatted.csv', sep = ',', check.names = FALSE)
vulnerability_under_five_mortality = read.csv('Vulnerability_data/GDL-Under-five-mortality-rate-data-formatted.csv', sep = ',', check.names = FALSE)

#Source: World Bank
worldBank = read.csv('World_bank_SubMalnutrition_data/worldBank_malnutrition.csv', sep = ',')

#Source: Unicef



#Source: Action Against Hunger (HUM Data)

hum_wasting = read.csv('Hum_data/sahel_countries_global_nutrition_admin1_summary_latest_surveys.csv', sep = ';' , check.names = FALSE)



# Vulnerability formatting ------------------------------------------------



#We transform the columns of years to rows


vulnerability_stunted = melt(data = vulnerability_stunted, id.vars = c('Country','Region'), measure.vars = c('1998','1999','2000','2001','2002','2003','2004',
                                                                                                             '2005','2006','2007','2008','2009','2010','2011','2012','2013','2014',
                                                                                                             '2015','2016','2017','2018'))

vulnerability_wasted = melt(data = vulnerability_wasted, id.vars = c('Country','Region'), measure.vars = c('1998','1999','2000','2001','2002','2003','2004',
                                                                                                             '2005','2006','2007','2008','2009','2010','2011','2012','2013','2014',
                                                                                                             '2015','2016','2017','2018'))

vulnerability_under_five_mortality = melt(data = vulnerability_under_five_mortality, id.vars = c('Country','Region'), measure.vars = c('1998','1999','2000','2001','2002','2003','2004',
                                                                                                             '2005','2006','2007','2008','2009','2010','2011','2012','2013','2014',
                                                                                                             '2015','2016','2017','2018'))

#Merge into one data frame

vulnerability_data = vulnerability_stunted
vulnerability_data$Stunted = vulnerability_stunted$value
vulnerability_data$value = NULL


vulnerability_data = cbind(vulnerability_data, vulnerability_wasted$value)
vulnerability_data$Wasted = vulnerability_data$`vulnerability_wasted$value`
vulnerability_data$`vulnerability_wasted$value` = NULL

vulnerability_data = cbind(vulnerability_data, vulnerability_under_five_mortality$value)
vulnerability_data$Mortality = vulnerability_data$`vulnerability_under_five_mortality$value`
vulnerability_data$`vulnerability_under_five_mortality$value` = NULL

vulnerability_data$Year = vulnerability_data$variable
vulnerability_data$variable = NULL

vulnerability_data = data.frame(vulnerability_data)

vulnerability_data = vulnerability_data %>%
  dplyr::rename(
    'admin1Name' = Region,
    'admin0Name' = Country
  )


# Biomass formatting ------------------------------------------------------

biomass_data = subset(biomass, select=c('admin0Name','admin1Name','AREA','1998','1999','2000','2001','2002',
                                        '2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013',
                                        '2014','2015','2016','2017','2018','2019'))
biomass_data = data.frame(biomass_data)

biomass_data$admin0Name = factor(biomass_data$admin0Name)
biomass_data$admin1Name = factor(biomass_data$admin1Name)

#First we add all the admin 2 into admin1
biomass_data = biomass_data %>%
                group_by(admin0Name, admin1Name) %>%
                  summarise(AREA = sum(AREA),X1998 = sum(X1998),X1999 = sum(X1999),X2000 = sum(X2000),X2001 = sum(X2001),X2002 = sum(X2002),
                            X2003 = sum(X2003),X2004 = sum(X2004),X2005 = sum(X2005),X2005 = sum(X2005),X2006 = sum(X2006),
                            X2007 = sum(X2007),X2008 = sum(X2008),X2009 = sum(X2009),X2010 = sum(X2010),X2011 = sum(X2011),
                            X2012 = sum(X2012),X2013 = sum(X2013),X2014 = sum(X2014),X2015 = sum(X2015),X2016 = sum(X2016),
                            X2017 = sum(X2017),X2018 = sum(X2018),X2019 = sum(X2019))

mean_biomass = c()
biomass_data = data.frame(biomass_data)

for (i in biomass_data$admin1Name){
  mean_biomass = c(mean_biomass, mean(unlist(biomass_data[biomass_data$admin1Name == i,4:24])))
}

biomass_data$Mean = mean_biomass

#renaming so that the years become integers before the melt
biomass_data = biomass_data %>% 
  dplyr::rename(
    'Area' = AREA,
    '1998' = X1998,
    '1999' = X1999,
    '2000' = X2000,
    '2001' = X2001,
    '2002' = X2002,
    '2003' = X2003,
    '2004' = X2004,
    '2005' = X2005,
    '2006' = X2006,
    '2007' = X2007,
    '2008' = X2008,
    '2009' = X2009,
    '2010' = X2010,
    '2011' = X2011,
    '2012' = X2012,
    '2013' = X2013,
    '2014' = X2014,
    '2015' = X2015,
    '2016' = X2016,
    '2017' = X2017,
    '2018' = X2018,
    '2019' = X2019
  )

biomass_data = melt(data = biomass_data, id.vars = c('admin0Name','admin1Name', 'Mean','Area'), measure.vars = c('1998','1999','2000','2001',
                                                                                                                              '2002','2003','2004',
                                                                                                                              '2005','2006','2007','2008','2009','2010',
                                                                                                                              '2011','2012','2013','2014',
                                                                                                                              '2015','2016','2017','2018','2019'))

biomass_data = biomass_data %>%
  dplyr::rename(
    'Year' = variable,
    'Biomass' = value
  )

#Still, a conversion of chad regions to 'Zones' is necessary:
# Zone 1 -> N'Djamena	
# Zone 2 -> Borkou, Ennedi-Est, Ennedi Ouest, Tibesti, Kanem, Barh-El-Gazel, Lac
# Zone 3 -> Guera, Batha, Salamat
# Zone 4 -> Ouaddai, Sila, Wadi Fira
# Zone 5 -> Chari-Baguirmi, Hadjer-Lamis, 
# Zone 6 -> Mayo-Kebbi Ouest, Mayo Kebbi Est
# Zone 7 -> Logone Occidental, Logone Oriental, Tandjile
# Zone 8 -> Moyen-Chari, Mandoul

chad = biomass_data[biomass_data$admin0Name == 'Chad',]

zone_1 = subset(chad, admin1Name == "N'Djamena")
zone_1[,2] = 'Zone 1'

zone_2 = subset(chad, admin1Name %in% c('Borkou', 'Ennedi-Est', 'Ennedi Ouest', 'Tibesti', 'Kanem', 'Barh-El-Gazel', 'Lac'))
zone_2 = zone_2 %>%
  group_by(Year) %>%
    summarise(admin0Name = 'Chad', admin1Name = 'Zone 2', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

zone_3 = subset(chad, admin1Name %in% c('Guera', 'Batha', 'Salamat'))
zone_3 = zone_3 %>%
  group_by(Year) %>%
  summarise(admin0Name = 'Chad', admin1Name = 'Zone 3', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

zone_4 = subset(chad, admin1Name %in%  c('Ouaddai', 'Sila', 'Wadi Fira'))
zone_4 = zone_4 %>%
  group_by(Year) %>%
  summarise(admin0Name = 'Chad', admin1Name = 'Zone 4', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

zone_5 = subset(chad, admin1Name %in%  c('Chari-Baguirmi', 'Hadjer-Lamis'))
zone_5 = zone_5 %>%
  group_by(Year) %>%
  summarise(admin0Name = 'Chad', admin1Name = 'Zone 5', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

zone_6 = subset(chad, admin1Name %in%  c('Mayo-Kebbi Ouest', 'Mayo Kebbi Est'))
zone_6 = zone_6 %>%
  group_by(Year) %>%
  summarise(admin0Name = 'Chad', admin1Name = 'Zone 6', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

zone_7 = subset(chad, admin1Name %in%  c('Logone Occidental', 'Logone Oriental', 'Tandjile'))
zone_7 = zone_7 %>%
  group_by(Year) %>%
  summarise(admin0Name = 'Chad', admin1Name = 'Zone 7', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

zone_8 = subset(chad, admin1Name %in%  c('Moyen-Chari', 'Mandoul'))
zone_8 = zone_8 %>%
  group_by(Year) %>%
  summarise(admin0Name = 'Chad', admin1Name = 'Zone 8', Area = sum(Area),Mean = mean(Mean), Biomass = sum(Biomass))

chad = rbind(zone_1,zone_2,zone_3,zone_4,zone_5,zone_6,zone_7,zone_8)

biomass_data = biomass_data[-which(biomass_data$admin0Name == 'Chad'),]
biomass_data = rbind(biomass_data,chad)

# WorldBank formatting ----------------------------------------------------

worldBank_wasting = worldBank[worldBank$SeriesName == 'Prevalence of wasting; weight for height (% of children under 5)',]
worldBank_wasting$admin0Name = as.factor(worldBank_wasting$admin0Name)
worldBank_wasting$admin1Name = as.factor(worldBank_wasting$admin1Name)

worldBank_stunting = worldBank[worldBank$SeriesName == 'Prevalence of stunting; height for age (% of children under 5)',]
worldBank_stunting$admin0Name = as.factor(worldBank_stunting$admin0Name)
worldBank_stunting$admin1Name = as.factor(worldBank_stunting$admin1Name)

worldBank_wasting = subset(worldBank_wasting, select=c('admin0Name','admin1Name','X1998.YR1998.','X1999.YR1999.','X2000.YR2000.',
                                                       'X2001.YR2001.','X2002.YR2002.','X2003.YR2003.','X2004.YR2004.','X2005.YR2005.',
                                                       'X2006.YR2006.','X2007.YR2007.','X2008.YR2008.','X2009.YR2009.','X2010.YR2010.',
                                                       'X2011.YR2011.','X2012.YR2012.','X2013.YR2013.','X2014.YR2014..'))


worldBank_wasting = worldBank_wasting %>% 
  dplyr::rename(
    '1998' = X1998.YR1998.,
    '1999' = X1999.YR1999.,
    '2000' = X2000.YR2000.,
    '2001' = X2001.YR2001.,
    '2002' = X2002.YR2002.,
    '2003' = X2003.YR2003.,
    '2004' = X2004.YR2004.,
    '2005' = X2005.YR2005.,
    '2006' = X2006.YR2006.,
    '2007' = X2007.YR2007.,
    '2008' = X2008.YR2008.,
    '2009' = X2009.YR2009.,
    '2010' = X2010.YR2010.,
    '2011' = X2011.YR2011.,
    '2012' = X2012.YR2012.,
    '2013' = X2013.YR2013.,
    '2014' = X2014.YR2014..
  )


worldBank_wasting = melt(data = worldBank_wasting, id.vars = c('admin0Name','admin1Name'), measure.vars = c('1998','1999','2000','2001',
                                                                                                                 '2002','2003','2004',
                                                                                                                 '2005','2006','2007','2008','2009','2010',
                                                                                                                 '2011','2012','2013','2014'))
worldBank_wasting = worldBank_wasting %>%
  dplyr::rename(
    'Year' = variable,
    'Wasted' = value
  )


worldBank_stunting = subset(worldBank_stunting,select=c('admin0Name','admin1Name','X1998.YR1998.','X1999.YR1999.','X2000.YR2000.',
                                                        'X2001.YR2001.','X2002.YR2002.','X2003.YR2003.','X2004.YR2004.','X2005.YR2005.',
                                                        'X2006.YR2006.','X2007.YR2007.','X2008.YR2008.','X2009.YR2009.','X2010.YR2010.',
                                                        'X2011.YR2011.','X2012.YR2012.','X2013.YR2013.','X2014.YR2014..'))


worldBank_stunting = worldBank_stunting %>% 
  dplyr::rename(
    '1998' = X1998.YR1998.,
    '1999' = X1999.YR1999.,
    '2000' = X2000.YR2000.,
    '2001' = X2001.YR2001.,
    '2002' = X2002.YR2002.,
    '2003' = X2003.YR2003.,
    '2004' = X2004.YR2004.,
    '2005' = X2005.YR2005.,
    '2006' = X2006.YR2006.,
    '2007' = X2007.YR2007.,
    '2008' = X2008.YR2008.,
    '2009' = X2009.YR2009.,
    '2010' = X2010.YR2010.,
    '2011' = X2011.YR2011.,
    '2012' = X2012.YR2012.,
    '2013' = X2013.YR2013.,
    '2014' = X2014.YR2014..
  )


worldBank_stunting = melt(data = worldBank_stunting, id.vars = c('admin0Name','admin1Name'), measure.vars = c('1998','1999','2000','2001',
                                                                                                            '2002','2003','2004',
                                                                                                            '2005','2006','2007','2008','2009','2010',
                                                                                                            '2011','2012','2013','2014'))

worldBank_stunting = worldBank_stunting %>%
  dplyr::rename(
    'Year' = variable,
    'Stunted' = value
  )

worldBank_data = cbind(worldBank_wasting,worldBank_stunting$Stunted)
worldBank_data = worldBank_data %>%
                 dplyr::rename(
                   'Stunted' = 'worldBank_stunting$Stunted'
                 )

worldBank_data = worldBank_data %>% 
                    mutate(Wasted = na_if(Wasted, ".."),Stunted = na_if(Stunted, ".."))

worldBank_data$Wasted = as.numeric(as.character(worldBank_data$Wasted))
worldBank_data$Stunted = as.numeric(as.character(worldBank_data$Stunted))

# Hum data formatting  -----------------------------------------------------

hum_wasting = hum_wasting %>%
  dplyr::rename(
    'admin0Name' =  '﻿CNTRY_NAME',
    'wasted' = 'GAM Prevalence',
    'admin1Name' = 'ADM1_NAME'
  )

hum_wasting$Rowcacode1 = NULL
hum_wasting$Season = NULL
hum_wasting$`Survey name` = NULL
hum_wasting$Comments = NULL
hum_wasting$CNTRY_CODE = NULL
hum_wasting$ADM1_CODE = NULL


hum_wasting$wasted =  as.numeric(as.character(sub("," , ".", hum_wasting$wasted)))

# Joining of all data -----------------------------------------------------


data = biomass_data %>% right_join(vulnerability_data, by = c('admin0Name',"admin1Name",'Year'))

data$admin1Name = factor(data$admin1Name) 

data = data %>% left_join(worldBank_data, by = c('admin0Name',"admin1Name",'Year'))

data$Year = as.integer(data$Year)

data = data %>% 
  mutate(Wasted = coalesce(Wasted.x,Wasted.y),Stunted = coalesce(Stunted.x,Stunted.y)) %>%
  select(admin0Name, admin1Name,Mean,Area,Biomass,Year,Mortality, Wasted,Stunted)


data = data %>% left_join(hum_wasting, by = c('admin0Name',"admin1Name",'Year'))

data = data %>%
              mutate(Wasted = coalesce(Wasted,wasted)) %>%
                select(admin0Name, admin1Name,Mean,Area,Biomass,Year,Mortality, Wasted,Stunted)

data$Biomass_anomaly = data$Biomass - data$Mean

write.csv(data,'Mixed_model_data/base_data.csv')



