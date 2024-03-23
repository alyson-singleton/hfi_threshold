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
hfi_thresholds_full <- hfi_thresholds_full[complete.cases(hfi_thresholds_full),]

####################
#build threshold ops
####################

hfi_thresholds_full$threshold_dummy_gt8 <- ifelse(hfi_thresholds_full$hfi_median > 8, 1, 0)
hfi_thresholds_full$threshold_dummy_gt8_3y <- hfi_thresholds_full$threshold_dummy_gt8
hfi_thresholds_full$threshold_dummy_gt9 <- ifelse(hfi_thresholds_full$hfi_median > 9, 1, 0)
hfi_thresholds_full$threshold_dummy_gt9_3y <- hfi_thresholds_full$threshold_dummy_gt9
hfi_thresholds_full$threshold_dummy_gt10 <- ifelse(hfi_thresholds_full$hfi_median > 10, 1, 0)
hfi_thresholds_full$threshold_dummy_gt10_3y <- hfi_thresholds_full$threshold_dummy_gt10

for (ii in unique(hfi_thresholds_full$CD_MUN)){
  data <- hfi_thresholds_full[which(hfi_thresholds_full$CD_MUN==ii),]
  for (i in 1:length(data$threshold_dummy_gt10)){
    if(i > 2 && i < 18){
      #data$threshold_dummy_gt8_3y[i] <- ifelse(data$threshold_dummy_gt8[i]==1 && data$threshold_dummy_gt8[i+1]==0,0,data$threshold_dummy_gt8[i])
      if(data$threshold_dummy_gt10[i]==1){
        data$threshold_dummy_gt10_3y[i] <- ifelse(((data$threshold_dummy_gt10_3y[i+1]==1 && data$threshold_dummy_gt10_3y[i+2]==1) ||
                                                    (data$threshold_dummy_gt10_3y[i-1]==1 && data$threshold_dummy_gt10_3y[i-2]==1) ||
                                                    (data$threshold_dummy_gt10_3y[i+1]==1 && data$threshold_dummy_gt10_3y[i-1]==1))
                                                 ,1,0)
      }else{
        data$threshold_dummy_gt10_3y[i] <- ifelse(((data$threshold_dummy_gt10_3y[i+1]==0 && data$threshold_dummy_gt10_3y[i+2]==0) ||
                                                    (data$threshold_dummy_gt10_3y[i-1]==0 && data$threshold_dummy_gt10_3y[i-2]==0) ||
                                                    (data$threshold_dummy_gt10_3y[i+1]==0 && data$threshold_dummy_gt10_3y[i-1]==0))
                                                 ,0,1)  
      }
    }
  }
  print(ii)
  hfi_thresholds_full[which(hfi_thresholds_full$CD_MUN==ii),] <- data
}

#test thresholds
ggplot(hfi_thresholds_full) +
  geom_line(aes(x=year, y=hfi_median, group=CD_MUN), alpha=0.05)

ggplot(hfi_thresholds_full) +
  geom_line(aes(x=year, y=threshold_dummy_gt8_3y, group=CD_MUN), alpha=0.05)

##################
#build outcome ops
##################

hfi_thresholds_full$occ_5 <- ifelse(hfi_thresholds_full$dengue_cases > 4, 1, 0)
hfi_thresholds_full$occ_5_1 <- hfi_thresholds_full$occ_5
hfi_thresholds_full$occ_8 <- ifelse(hfi_thresholds_full$dengue_cases > 7, 1, 0)
hfi_thresholds_full$occ_8_1 <- hfi_thresholds_full$occ_8
hfi_thresholds_full$occ_10 <- ifelse(hfi_thresholds_full$dengue_cases > 9, 1, 0)
hfi_thresholds_full$occ_10_1 <- hfi_thresholds_full$occ_10
hfi_thresholds_full$incidence_100k <- hfi_thresholds_full$dengue_cases/hfi_thresholds_full$population*100000
hfi_thresholds_full$incidence_300 <- ifelse(hfi_thresholds_full$incidence_100k > 300, 1, 0)
hfi_thresholds_full$incidence_300_1 <- hfi_thresholds_full$incidence_300
  
for (ii in unique(hfi_thresholds_full$CD_MUN)){
  data <- hfi_thresholds_full[which(hfi_thresholds_full$CD_MUN==ii),]
  for (i in 1:length(data$threshold_dummy_gt8)){
    if(data$occ_10[i]==1){
      for (k in i:length(data$threshold_dummy_gt8)){
        data$occ_10_1[k] <- 1
      }
    }
  }
  hfi_thresholds_full[which(hfi_thresholds_full$CD_MUN==ii),] <- data
  print(ii)
}

ggplot(hfi_thresholds_full) +
  geom_line(aes(x=year, y=incidence_300_1, group=CD_MUN), alpha=0.05)

#store dataset for future use
write.csv(hfi_thresholds_full, "~/Desktop/doctorate/hfi_threshold/hfi_thresholds_w_options.csv")
#load new dataset
hfi_thresholds_full <- read.csv("~/Desktop/doctorate/hfi_threshold/hfi_thresholds_w_options.csv")

#################
#build subset ops
#################

#option1: only municipalities that cross the threshold at some point (n=?)
hfi_thresholds_th_subset <- hfi_thresholds_full %>%
  group_by(CD_MUN) %>%
  summarize(min = min(hfi_median),
            max = max(hfi_median))

list <- which(hfi_thresholds_th_subset$min<8 & hfi_thresholds_th_subset$max>8)
CD_MUN_list <- as.data.frame(hfi_thresholds_th_subset[list,1])
hfi_thresholds_th_subset <- hfi_thresholds_full[which(as.numeric(hfi_thresholds_full$CD_MUN) %in% as.numeric(CD_MUN_list$CD_MUN)),]

ggplot(hfi_thresholds_th_subset) +
  geom_line(aes(x=year, y=hfi_median, group=CD_MUN), alpha=0.05)

ggplot(hfi_thresholds_th_subset) +
  geom_line(aes(x=year, y=threshold_dummy_gt8, group=CD_MUN), alpha=0.05)


###################
#really basic model
###################
hfi_thresholds_test <- hfi_thresholds_full #hfi_thresholds_th_subset or hfi_thresholds_full
hfi_thresholds_test$year <- as.factor(hfi_thresholds_test$year)
#hfi_thresholds_test <- hfi_thresholds_test[complete.cases(hfi_thresholds_test),]
#hfi_thresholds_test$threshold_dummy <- ifelse(hfi_thresholds_test$hfi_median > 8, 1, 0)

test <- fepois(occ_10_1 ~ l(threshold_dummy_gt10_3y, 0:10) | CD_MUN + year, vcov = "cluster", data = hfi_thresholds_test, panel.id = ~CD_MUN+year)
summary(test)
coefplot(test)

ggplot(hfi_thresholds_full) +
  geom_histogram(aes(x=dengue_cases), bins=20) +
  xlim(c(0,100))
library(ggplot2)
  

