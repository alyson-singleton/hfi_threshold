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

#add back in arial font
library(showtext)
font_add("Arial", "/Library/Fonts/Arial.ttf")  # Use the actual file path
showtext_auto()

#ggplot ops
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank())

#load data
hfi_thresholds_full <- read.csv("~/Desktop/doctorate/hfi_threshold/hfi_thresholds_w_options.csv")
hfi_thresholds_full$incidence <- (hfi_thresholds_full$dengue_cases + 1)/hfi_thresholds_full$population

hfi_threshold_count_geo <- read.csv("~/Downloads/brazil_municipality_yearly_hfi_threshold_counts.csv")
hfi_threshold_count_geo <- st_as_sf(data.frame(hfi_threshold_count_geo, geometry=geojson_sf(hfi_threshold_count_geo$.geo)))
hfi_threshold_count_geo_reduced <- data.frame(hfi_threshold_count_geo$CD_MUN, hfi_threshold_count_geo$geometry); colnames(hfi_threshold_count_geo_reduced) <- c("CD_MUN", "geometry")
hfi_threshold_count_geo_reduced$CD_MUN <- as.numeric(substr(hfi_threshold_count_geo_reduced$CD_MUN, 1, 6))

####################
# Create period columns for dengue vars
####################

#create year of first 10 cases column
hfi_thresholds_full$year_of_first_occ_10 <- NA  
hfi_thresholds_full$year_of_first_incidence_300 <- NA  
for (i in unique(hfi_thresholds_full$CD_MUN)){
  print(i)
  data <- hfi_thresholds_full[which(hfi_thresholds_full$CD_MUN==i),]
  data$year_of_first_occ_10 <- ifelse(length(data$year[which(data$occ_10_1==1)])>0, 
                                      min(data$year[which(data$occ_10_1==1)]), 0)
  data$year_of_first_incidence_300 <- ifelse(length(data$year[which(data$incidence_300_1==1)])>0, 
                                      min(data$year[which(data$incidence_300_1==1)]), 0)
  hfi_thresholds_full[which(hfi_thresholds_full$CD_MUN==i),] <- data
}

#add geometries for plotting
hfi_thresholds_full_w_geo <- full_join(hfi_thresholds_full, hfi_threshold_count_geo_reduced, by=c("CD_MUN")) %>%
  st_sf
hfi_thresholds_full_w_geo$year_of_first_occ_10 <- as.character(hfi_thresholds_full_w_geo$year_of_first_occ_10)
hfi_thresholds_full_w_geo$year_of_first_incidence_300 <- as.character(hfi_thresholds_full_w_geo$year_of_first_incidence_300)

#group years into four year periods
hfi_thresholds_full_w_geo <- hfi_thresholds_full_w_geo %>%
  mutate(year_of_first_occ_10_periods = replace(year_of_first_occ_10, year_of_first_occ_10 %in% 
                                                  c("2002", "2003"), "2001"),
         year_of_first_occ_10_periods = replace(year_of_first_occ_10_periods, year_of_first_occ_10_periods %in% 
                                                  c("2005", "2006", "2007"), "2004"),
         year_of_first_occ_10_periods = replace(year_of_first_occ_10_periods, year_of_first_occ_10_periods %in% 
                                                  c("2009", "2010", "2011"), "2008"),
         year_of_first_occ_10_periods = replace(year_of_first_occ_10_periods, year_of_first_occ_10_periods %in% 
                                                  c("2013", "2014", "2015"), "2012"),
         year_of_first_occ_10_periods = replace(year_of_first_occ_10_periods, year_of_first_occ_10_periods %in% 
                                                  c("2017", "2018", "2019"), "2016")) %>%
  mutate(year_of_first_incidence_300_periods = replace(year_of_first_incidence_300, year_of_first_incidence_300 %in% 
                                                  c("2002", "2003"), "2001"),
         year_of_first_incidence_300_periods = replace(year_of_first_incidence_300_periods, year_of_first_incidence_300_periods %in% 
                                                  c("2005", "2006", "2007"), "2004"),
         year_of_first_incidence_300_periods = replace(year_of_first_incidence_300_periods, year_of_first_incidence_300_periods %in% 
                                                  c("2009", "2010", "2011"), "2008"),
         year_of_first_incidence_300_periods = replace(year_of_first_incidence_300_periods, year_of_first_incidence_300_periods %in% 
                                                  c("2013", "2014", "2015"), "2012"),
         year_of_first_incidence_300_periods = replace(year_of_first_incidence_300_periods, year_of_first_incidence_300_periods %in% 
                                                  c("2017", "2018", "2019"), "2016"))

#change zeros to NAs for plotting
hfi_thresholds_full_w_geo$year_of_first_occ_10_periods <- ifelse(hfi_thresholds_full_w_geo$year_of_first_occ_10_periods==0,NA,hfi_thresholds_full_w_geo$year_of_first_occ_10_periods)
hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods <- ifelse(hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods==0,NA,hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods)

#simplify geometries for faster plotting
hfi_thresholds_full_w_geo <- st_simplify(hfi_thresholds_full_w_geo, dTolerance = 75) 

####################
# F1: Map of period of first 10 dengue cases
####################

#plot periods
dengue_occ_10_periods_map <- ggplot() +
  geom_sf(data=hfi_thresholds_full_w_geo, aes(fill=year_of_first_occ_10_periods), colour="lightgrey", linewidth=0.0001) +
  scale_fill_manual(name="Period with\nfirst 10 cases", na.value="grey", 
                    values = brewer.pal(5, "Reds"), labels=c("2001-2003","2004-2007","2008-2011","2012-2015","2016-2019")) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

#save plot to desktop
ggsave("~/Desktop/dengue_occ_10_periods2.pdf", dengue_occ_10_periods_map, width = 7, height=5, units=c("in"))

####################
# F2: Map of period of first 300/100k dengue incidence
####################

#plot periods
dengue_incidence_300_periods_map <- ggplot() +
  geom_sf(data=hfi_thresholds_full_w_geo, aes(fill=year_of_first_incidence_300_periods), colour="lightgrey", linewidth=0.0001) +
  scale_fill_manual(name="Period of\nfirst 300/100k inc", na.value="grey", 
                    values = brewer.pal(5, "Reds"), labels=c("2001-2003","2004-2007","2008-2011","2012-2015","2016-2019")) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

#save plot to desktop
ggsave("~/Desktop/dengue_inc_300_periods.pdf", dengue_incidence_300_periods_map, width = 7, height=5, device="pdf")

####################
# Prep HFI data for plotting
####################
hfi_threshold_count_geo <- hfi_threshold_count_geo[,c(2,3,6:25)] %>%
  st_drop_geometry() %>% 
  pivot_longer(cols = c(3:22), 
               names_to = "year", 
               values_to = "hfi_gt8_count")
hfi_threshold_count_geo$year <- substr(hfi_threshold_count_geo$year, 4, 7)
hfi_threshold_count_geo$year <- as.numeric(hfi_threshold_count_geo$year)
hfi_threshold_count_geo$CD_MUN <- as.numeric(substr(hfi_threshold_count_geo$CD_MUN, 1, 6))
hfi_thresholds_full_w_geo_count <- full_join(hfi_thresholds_full_w_geo, hfi_threshold_count_geo, by=c("CD_MUN", "year"))
  
#create columns for plotting
hfi_thresholds_full_w_geo_count_2019 <- hfi_thresholds_full_w_geo_count[which(hfi_thresholds_full_w_geo_count$year==2019),]%>%st_drop_geometry()
hfi_thresholds_full_w_geo_count_2001 <- hfi_thresholds_full_w_geo_count[which(hfi_thresholds_full_w_geo_count$year==2001),]
hfi_thresholds_full_w_geo_count_2001_2019 <- full_join(hfi_thresholds_full_w_geo_count_2001,as.data.frame(hfi_thresholds_full_w_geo_count_2019[,c(5,33)]),by=c("CD_MUN"))%>%
  st_sf
hfi_thresholds_full_w_geo_count_2001_2019 <- hfi_thresholds_full_w_geo_count_2001_2019[,c(5:6, 32:35)]
colnames(hfi_thresholds_full_w_geo_count_2001_2019)[4:5] <- c("hfi_count_2001", "hfi_count_2019")
hfi_thresholds_full_w_geo_count_2001_2019$prop_2001 <- hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_2001/hfi_thresholds_full_w_geo_count_2001_2019$AREA_KM2
hfi_thresholds_full_w_geo_count_2001_2019$prop_2019 <- hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_2019/hfi_thresholds_full_w_geo_count_2001_2019$AREA_KM2
hfi_thresholds_full_w_geo_count_2001_2019$hfi_relative_change <- hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_2019/hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_2001
hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change <- hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_2019-hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_2001
hfi_thresholds_full_w_geo_count_2001_2019$hfi_prop_change <- hfi_thresholds_full_w_geo_count_2001_2019$prop_2019-hfi_thresholds_full_w_geo_count_2001_2019$prop_2001

hfi_thresholds_full_w_geo_count_2001_2019 <- hfi_thresholds_full_w_geo_count_2001_2019 %>% 
  mutate(hfi_absolute_change_cats = cut_number(hfi_absolute_change, n = 7))
hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats <- as.character(hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats)
hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats <- factor(hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats,
                                                                             levels=unique(hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats)[c(1,6,7,4,5,3,2)])

####################
# F3: Map of HFI count change
####################

hfi_abs_change_map <- ggplot() +
  geom_sf(data=hfi_thresholds_full_w_geo_count_2001_2019, aes(fill=hfi_absolute_change_cats), colour="lightgrey", linewidth=0.0001) +
  scale_fill_manual(name="Change in no. of pixels\nw HFI>8 from 2001-2019", na.value="grey", 
                    values = (brewer.pal(7, "Blues"))) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("~/Desktop/hfi_abs_change_map.pdf", hfi_abs_change_map, device="pdf", width = 7, height=5, units=c("in"))


####################
# F4: Map of HFI proportion change
####################

hfi_thresholds_full_w_geo_count_2001_2019 <- hfi_thresholds_full_w_geo_count_2001_2019 %>% 
  mutate(hfi_prop_change_cats = cut_number(hfi_prop_change, n = 7))
hfi_thresholds_full_w_geo_count_2001_2019$hfi_prop_change_cats <- as.character(hfi_thresholds_full_w_geo_count_2001_2019$hfi_prop_change_cats)
hfi_thresholds_full_w_geo_count_2001_2019$hfi_prop_change_cats <- factor(hfi_thresholds_full_w_geo_count_2001_2019$hfi_prop_change_cats,
                                                                             levels=unique(hfi_thresholds_full_w_geo_count_2001_2019$hfi_prop_change_cats)[c(1,4,2,3,7,5,6)])

hfi_prop_change_map <- ggplot() +
  geom_sf(data=hfi_thresholds_full_w_geo_count_2001_2019, aes(fill=hfi_prop_change_cats), colour="lightgrey", linewidth=0.0001) +
  scale_fill_manual(name="Change in prop of pixels\nw HFI>8 from 2001-2019", na.value="grey", 
                    values = (brewer.pal(7, "Greens"))) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("~/Desktop/hfi_prop_change_map.pdf", hfi_prop_change_map, device="pdf", width = 7, height=5, units=c("in"))

write_sf(hfi_thresholds_full_w_geo_count_2001_2019, "~/Desktop/hfi_thresholds_full_w_geo_count_2001_2019")


####################
# F5: Map of HFI pop weighted count change
####################

hfi_threshold_count_geo_2001 <- read.csv("~/Downloads/brazil_municipality_gte8_popweighted_2001.csv")
hfi_threshold_count_geo_2001 <- st_as_sf(data.frame(hfi_threshold_count_geo_2001, geometry=geojson_sf(hfi_threshold_count_geo_2001$.geo)))
colnames(hfi_threshold_count_geo_2001)[6] <- "hfi_count_weighted_2001"

hfi_threshold_count_geo_2019 <- read.csv("~/Downloads/brazil_municipality_gte8_popweighted_2019.csv")
hfi_threshold_count_geo_2019 <- st_as_sf(data.frame(hfi_threshold_count_geo_2019, geometry=geojson_sf(hfi_threshold_count_geo_2019$.geo)))
hfi_threshold_count_geo_2019 <- hfi_threshold_count_geo_2019%>%st_drop_geometry()
colnames(hfi_threshold_count_geo_2019)[6] <- "hfi_count_weighted_2019"
hfi_thresholds_full_w_geo_count_2001_2019 <- full_join(hfi_threshold_count_geo_2001,as.data.frame(hfi_threshold_count_geo_2019[,c(3,6)]),by=c("CD_MUN"))%>%
  st_sf

#create columns for plotting

hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change <- hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_weighted_2019-hfi_thresholds_full_w_geo_count_2001_2019$hfi_count_weighted_2001
hfi_thresholds_full_w_geo_count_2001_2019 <- hfi_thresholds_full_w_geo_count_2001_2019 %>% 
  mutate(hfi_absolute_change_cats = cut_number(hfi_absolute_change, n = 7))
hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats <- as.character(hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats)
hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats <- factor(hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats,
                                                                             levels=unique(hfi_thresholds_full_w_geo_count_2001_2019$hfi_absolute_change_cats)[c(2,4,6,7,5,1,3)])

hfi_abs_change_map <- ggplot() +
  geom_sf(data=hfi_thresholds_full_w_geo_count_2001_2019, aes(fill=hfi_absolute_change_cats), colour="lightgrey", linewidth=0.0001) +
  scale_fill_manual(name="Change in prop of pop\nexposed to HFI>8\nfrom 2001-2019", na.value="grey", 
                    values = (brewer.pal(7, "Purples"))) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

ggsave("~/Desktop/hfi_abs_change_weighted_map.pdf", hfi_abs_change_map, device="pdf", width = 7, height=5, units=c("in"))
