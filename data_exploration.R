# written by Alyson Singleton

#read in required packages
require(readxl)
require(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
library(tidyverse)
library(plm)
library(fixest)

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

# load data
case_data_from_elle <- read.csv("~/Desktop/doctorate/hfi_threshold/annual_dengue_case_inci.csv")
hfi_median_wide <- read.csv("~/Desktop/doctorate/hfi_threshold/brazil_municipality_yearly_hfi_median.csv")
hfi_median_wide <- hfi_median_wide[,c(3,6:25)]
colnames(hfi_median_wide) <- c("CD_MUN",seq(2000,2019,1))

hfi_median_long <- hfi_median_wide %>% 
  pivot_longer(cols = c(2:21), 
               names_to = "year", 
               values_to = "hfi_median")
hfi_median_long$year <- as.numeric(hfi_median_long$year)
hfi_median_long$CD_MUN <- str_sub(as.character(hfi_median_long$CD_MUN), end = -2)
hfi_median_long$CD_MUN <- as.numeric(hfi_median_long$CD_MUN)

#join elle's data with hfi
hfi_thresholds_full <- left_join(case_data_from_elle, hfi_median_long, by = c("year" = "year", "CD_MUN" = "CD_MUN"))

#add treatment var (threshold)
hfi_thresholds_full$threshold_dummy <- ifelse(hfi_thresholds_full$hfi_median > 8, 1, 0)

#store dataset for future use
write.csv(hfi_thresholds_full, "~/Desktop/doctorate/hfi_threshold/hfi_thresholds_full.csv")

#load new dataset
hfi_thresholds_full <- read.csv("~/Desktop/doctorate/hfi_threshold/hfi_thresholds_full.csv")

#subset options
#option1: only municipalities that cross the threshold
hfi_thresholds_full_test <- hfi_thresholds_full %>%
  group_by(CD_MUN) %>%
  summarize(min = min(hfi_median),
            max = max(hfi_median))

list <- which(hfi_thresholds_full_test$min<8 & hfi_thresholds_full_test$max>8)
CD_MUN_list <- as.data.frame(hfi_thresholds_full_test[list,1])
hfi_thresholds_full_only_threshold <- hfi_thresholds_full[which(as.numeric(hfi_thresholds_full$CD_MUN) %in% as.numeric(CD_MUN_list$CD_MUN)),]

#run a really basic model
hfi_thresholds_test <- hfi_thresholds_full
hfi_thresholds_test$year <- as.factor(hfi_thresholds_test$year)
hfi_thresholds_test <- hfi_thresholds_test[complete.cases(hfi_thresholds_test),]
hfi_thresholds_test$threshold_dummy <- ifelse(hfi_thresholds_test$hfi_median > 8, 1, 0)

test <- fepois(occ ~ l(threshold_dummy, 0:5) | CD_MUN + year, vcov = "twoway", data = hfi_thresholds_test, panel.id = ~CD_MUN+year)
summary(test)
coefplot(test)

ggplot(hfi_thresholds_test) +
  geom_line(aes(x=year, y=threshold_dummy, group=CD_MUN), alpha=0.1)
  

