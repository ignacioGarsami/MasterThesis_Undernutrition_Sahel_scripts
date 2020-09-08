
library('SASmixed')
library(nlme)
library(lme4)
library(ggplot2)
library(grid)
library(gridExtra)
library(gstat)
library(tidyverse)
library(fasterize)
library(tiff)
library(raster)
library(sf)
library(spatialreg)
library(spdep)
rm(list=ls())

library(grid)
library(gridExtra)

# setwd("C:/Users/IgnacioGarcia/Desktop/UNIVERSIDAD/Statistics for data science/Master thesis/data")
# 
# data = st_read('Mixed_model_data/base_data_raster')
# 
# data = st_as_sf(data, sf_column_name = 'geometry')
# 
# data_total = data
# 
# data = data[
#   (data$admn0Nm == 'Mauritania' & data$admn1Nm %in% c('Hodh Ech Chargi', 'Hodh El Gharbi', 'Tagant', 'Assaba', 'Brakna',    'Gorgol', 'Guidimakha', 'Trarza', 'Nouakchott') ) |
#     (data$admn0Nm == 'Senegal' & data$admn1Nm %in% c('Saint Louis', 'Louga', 'Matam', 'Dakar', 'Thies', 'Diourbel') ) |
#     (data$admn0Nm == 'Mali' & data$admn1Nm %in% c( 'Tombouctou', 'Gao', 'Kayes', 'Koulikoro', 'Segou', 'Mopti') ) |
#     (data$admn0Nm == 'Niger' & data$admn1Nm %in% c('Agadez') == FALSE ) |
#     (data$admn0Nm == 'Chad' & data$admn1Nm %in% c('Zone 2','Zone 3','Zone 4','Zone 5') ) |
#     (data$admn0Nm == 'Nigeria' & data$admn1Nm %in% c('Sokoto', 'Zamfara','Katsina','Kano','Jigawa','Yobe','Borno') & (data$Wasted > 0 | is.na(data$Wasted))  ) |
#     (data$admn0Nm == 'Burkina Faso' & data$admn1Nm %in% c('Sahel', 'Nord', 'Centre-Nord') )
#   ,]
# 
# 
# data$Bmss_nm = (data$Bmss_nm)
# 
# summary(data)
# 
# #Remove all columns where wasted is NA
# #Remember to change this!
# data_wasted = data[!is.na(data$Wasted) & !is.na(data$Bmss_nm) ,]
# data_stunted = data[!is.na(data$Stunted) & !is.na(data$Bmss_nm) ,]
# data_mrtly = data[!is.na(data$Mortlty) & !is.na(data$Bmss_nm) ,]
# 
# #Use this to test the effect of removingthe very few points with very high wasting values.
# #data = data[data$Wasted <= 30,]

setwd("C:/Users/IgnacioGarcia/Desktop/UNIVERSIDAD/Statistics for data science/Master thesis/data")
data = st_read('Mixed_model_data/base_data_raster')
#data$X = NULL

data = st_as_sf(data, sf_column_name = 'geometry')

data_total = data

summary(data)

#Remove all columns where wasted is NA
#Remember to change this!
data_wasted = data[!is.na(data$Wasted) & !is.na(data$Bmss_nm) ,]
data_stunted = data[!is.na(data$Stunted) & !is.na(data$Bmss_nm) ,]
data_mrtly = data[!is.na(data$Mortlty) & !is.na(data$Bmss_nm) ,]

x <- aggregate(data_wasted, 
               by = list(data_wasted$admn0Nm,data_wasted$admn1Nm),
               FUN = mean,
               warn = FALSE)


x = st_as_sf(x, sf_column_name = 'geometry')
x = st_collection_extract(
  x,
  type = c("POLYGON"),
  warn = FALSE
)

x_total <- aggregate(data_wasted, 
                     by = list(data_wasted$admn0Nm),
                     FUN = mean,
                     warn = FALSE)


x_total = st_as_sf(x_total, sf_column_name = 'geometry')
x_total = st_collection_extract(
  x_total,
  type = c("POLYGON"),
  warn = FALSE
)

world_points<- st_centroid(x)
world_points <- cbind(x, st_coordinates(st_centroid(x$geometry)))

world_points_total<- st_centroid(x_total)
world_points_total <- cbind(x_total, st_coordinates(st_centroid(x_total$geometry)))

#This three librarys are necessary to create the map of the world
library(rnaturalearth)
library("rnaturalearthdata")
library(rgeos)
library("ggrepel")
world <- ne_countries(scale = "medium", returnclass = "sf")

theme_set(theme_bw())

wasting_map = function(year){
  

  wasting_plot = ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = data_wasted[data_wasted$Year == year,], aes(fill =  data_wasted[data_wasted$Year == year,]$Wasted), color = NA) +
    geom_sf(data = world, fill = NA, size = 0.6, color = 'black') +
    scale_fill_viridis_c(alpha = .5, limits = c(0,50)) +
    coord_sf(xlim =c(30, -20), ylim = c(0, 28), expand = FALSE) + 
    xlab('Longitude') + ylab('Latitude') + 
    ggtitle('Acute malnutrition prevalence in W.Africa', subtitle = paste0('By region ,' , toString(year))) + 
    labs(fill = 'Wasting prevalence') + 
    theme( panel.background = element_rect(fill = 'aliceblue'))
    return(wasting_plot)
}

stunting_map = function(year){
  
  stunting_plot = ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = data_stunted[data_stunted$Year == year,], aes(fill =  data_stunted[data_stunted$Year == year,]$Stunted), color = NA) +
    geom_sf(data = world, fill = NA, size = 0.6, color = 'black') +
    scale_fill_viridis_c(alpha = .5, limits = c(0,85)) +
    coord_sf(xlim =c(30, -20), ylim = c(0, 28), expand = FALSE) + 
    xlab('Longitude') + ylab('Latitude') + 
    ggtitle('Chronic malnutrition prevalence in W.Africa', subtitle = paste0('By region ,' , toString(year))) + 
    labs(fill = 'Stunting prevalence') + 
    theme( panel.background = element_rect(fill = 'aliceblue'))
  return(stunting_plot)
}

biomass_map = function(year){
  
  biomass_plot = ggplot(data = world) +
    geom_sf(fill = "antiquewhite1") +
    geom_sf(data = data[data$Year == year,], aes(fill =  data[data$Year == year,]$Bmss_nm), color = NA) +
    geom_sf(data = world, fill = NA, size = 0.6, color = 'black') +
    scale_fill_viridis_c(alpha = .5, limits = c(-185075598,235874800)) +
    coord_sf(xlim =c(30, -20), ylim = c(0, 28), expand = FALSE) + 
    xlab('Longitude') + ylab('Latitude') + 
    ggtitle('Biomass anomaly in W.Africa', subtitle = paste0('By region ,' , toString(year)) ) + 
    labs(fill = 'Biomass anomaly') + 
    theme( panel.background = element_rect(fill = 'aliceblue'))
  return(biomass_plot)
  
}

for(i in 1998:2018){
  plot = arrangeGrob(wasting_map(i),stunting_map(i),biomass_map(i), ncol = 3)
  ggsave(file=paste0('temporal_values_WestAfrica_' , toString(i), '.png'), plot, width = 13, height = 7)
}


