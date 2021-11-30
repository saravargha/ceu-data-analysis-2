##################
## Assignment 2 ##
##################

# clean environment 
rm(list=ls())

# install packages
library(tidyverse)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)
library(modelsummary)
library(gridExtra)
library(data.table)

# load data

hotels_europe <- read_csv('https://osf.io/utwjs/download')
glimpse(hotels_europe)

# look into ratings
summary(hotels_europe$rating)

# substitute missing values with averages
hotels_europe$rating <- replace(hotels_europe$rating, is.na(hotels_europe$rating), mean(hotels_europe$rating, na.rm=TRUE))

# pick Barcelona as actual city and creae binary for 'highly_rated'
hotels_barcelona <- hotels_europe %>% 
  filter(city_actual=='Barcelona') %>%
  mutate(highly_rated = ifelse(rating>=4, 1, 0))

table(hotels_barcelona$highly_rated)
summary(hotels_barcelona$highly_rated)

getwd()

summary(hotels_barcelona$distance)
summary(hotels_barcelona$stars)
summary(hotels_barcelona$rating)
summary(hotels_barcelona$rating_reviewcount)
summary(hotels_barcelona$ratingta)
summary(hotels_barcelona$ratingta_count)

 