# written by aly singleton

#read in required packages
require(readxl)
require(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
library(tidyverse)
library(plm)
library(fixest)
library(geobr)
library(geojsonsf)
library(RColorBrewer)
library(viridis) 
library(cowplot)

#load data incidence data
hfi_thresholds_full <- read.csv("~/Desktop/doctorate/hfi_threshold/hfi_thresholds_w_options.csv")
hfi_thresholds_full$incidence <- (hfi_thresholds_full$dengue_cases + 1)/hfi_thresholds_full$population
#reduce to 2001 and 2019
hfi_thresholds_full <- hfi_thresholds_full[which(hfi_thresholds_full$year %in% c(2001, 2019)),]

#load hfi data
hfi_threshold_count_geo_2001 <- read.csv("~/Downloads/brazil_municipality_gte8_popweighted_2001.csv")
hfi_threshold_count_geo_2001 <- data.frame(hfi_threshold_count_geo_2001[,c(3,6)])
colnames(hfi_threshold_count_geo_2001)[2] <- "2001"
hfi_threshold_count_geo_2019 <- read.csv("~/Downloads/brazil_municipality_gte8_popweighted_2019.csv")
hfi_threshold_count_geo_2019 <- data.frame(hfi_threshold_count_geo_2019[,c(3,6)])
colnames(hfi_threshold_count_geo_2019)[2] <- "2019"
hfi_thresholds_full_w_geo_count_2001_2019 <- full_join(hfi_threshold_count_geo_2001,hfi_threshold_count_geo_2019,by=c("CD_MUN"))
hfi_thresholds_full_w_geo_count_2001_2019$CD_MUN <- as.numeric(substr(hfi_thresholds_full_w_geo_count_2001_2019$CD_MUN, 1, 6))
#make long
hfi_thresholds_full_w_geo_count_2001_2019_long <- hfi_thresholds_full_w_geo_count_2001_2019 %>%
  pivot_longer(cols=c("2001", "2019"), names_to = "year", values_to = "weighted_hfi_exposure")
hfi_thresholds_full_w_geo_count_2001_2019_long$year <- as.numeric(hfi_thresholds_full_w_geo_count_2001_2019_long$year)
write.csv(hfi_thresholds_full_w_geo_count_2001_2019_long, "~/Desktop/doctorate/hfi_threshold/hfi_weighted_2001_2019.csv")

#combine
long_diff_df <- full_join(hfi_thresholds_full,hfi_thresholds_full_w_geo_count_2001_2019_long, by=c("CD_MUN"="CD_MUN", "year"="year"))

#baby models
#straight incidence
long_diff_model <- fepois(incidence ~ i(year, weighted_hfi_exposure, ref = '2001') | year + CD_MUN, vcov = "cluster", data = long_diff_df)
summary(long_diff_model)
#incidence threshold
long_diff_model <- feglm(incidence_300_1 ~ i(year, weighted_hfi_exposure, ref = '2001') | year, vcov = "cluster", 
                         family = "binomial", data = long_diff_df)
summary(long_diff_model)








