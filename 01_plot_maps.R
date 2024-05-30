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

#group years into three categories for bivariate plot
hfi_thresholds_full_w_geo <- hfi_thresholds_full_w_geo %>%
  mutate(year_of_first_occ_10_periods3 = replace(year_of_first_occ_10, year_of_first_occ_10 %in% 
                                                   c("2001", "2002", "2003", "2004", "2005", "2006"), 1),
         year_of_first_occ_10_periods3 = replace(year_of_first_occ_10_periods3, year_of_first_occ_10_periods3 %in% 
                                                   c("2007", "2008", "2009", "2010", "2011", "2012"), 2),
         year_of_first_occ_10_periods3 = replace(year_of_first_occ_10_periods3, year_of_first_occ_10_periods3 %in% 
                                                   c("2013", "2014", "2015", "2016", "2017", "2018", "2019"), 3)) %>%
  mutate(year_of_first_incidence_300_periods3 = replace(year_of_first_incidence_300, year_of_first_incidence_300 %in% 
                                                          c("2001", "2002", "2003", "2004", "2005", "2006"), 1),
         year_of_first_incidence_300_periods3 = replace(year_of_first_incidence_300_periods3, year_of_first_incidence_300_periods3 %in% 
                                                          c("2007", "2008", "2009", "2010", "2011", "2012"), 2),
         year_of_first_incidence_300_periods3 = replace(year_of_first_incidence_300_periods3, year_of_first_incidence_300_periods3 %in% 
                                                          c("2013", "2014", "2015", "2016", "2017", "2018", "2019"), 3))

#change zeros to NAs for plotting
hfi_thresholds_full_w_geo$year_of_first_occ_10_periods <- ifelse(hfi_thresholds_full_w_geo$year_of_first_occ_10_periods==0,NA,hfi_thresholds_full_w_geo$year_of_first_occ_10_periods)
hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods <- ifelse(hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods==0,NA,hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods)

hfi_thresholds_full_w_geo$year_of_first_occ_10_periods3 <- ifelse(hfi_thresholds_full_w_geo$year_of_first_occ_10_periods3==0,NA,hfi_thresholds_full_w_geo$year_of_first_occ_10_periods3)
hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods3 <- ifelse(hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods3==0,NA,hfi_thresholds_full_w_geo$year_of_first_incidence_300_periods3)

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


####################
# F6: Bivariate map of HFI pop weighted count change and dengue introduction
####################

####################
#create new dataframe of both dengue and hfi variables
hfi_thresholds_full_w_geo_count_2001_2019$CD_MUN <- as.numeric(substr(hfi_thresholds_full_w_geo_count_2001_2019$CD_MUN, 1, 6))
hfi_thresholds_full_no_geo <- hfi_thresholds_full_w_geo%>%st_drop_geometry()
hfi_thresholds_bivariate_plotting <- full_join(hfi_thresholds_full_w_geo_count_2001_2019,as.data.frame(hfi_thresholds_full_no_geo[,c(5,32,33)]),by=c("CD_MUN"))%>%
  st_sf

####################
#create column that will indicate the 9 different groupings using 1/3 tertiles/quantiles for breaks
# create 3 buckets for gini
quantiles_hfi <- hfi_thresholds_bivariate_plotting %>%
  pull(hfi_absolute_change) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for mean income
quantiles_dengue <- hfi_thresholds_bivariate_plotting %>%
  pull(mean) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readability reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high inequality, high income
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low inequality, high income
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium inequality, medium income
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high inequality, low income
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low inequality, low income
) %>%
  gather("group", "fill")

# cut into groups defined above and join fill
hfi_thresholds_bivariate_plotting %<>%
  mutate(
    hfi_quantiles = cut(
      hfi_absolute_change,
      breaks = quantiles_hfi,
      include.lowest = TRUE
    ),
    dengue_quantiles = year_of_first_incidence_300_periods3,
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(hfi_quantiles), "-",
      as.numeric(dengue_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")

####################
#plot
hfi_thresholds_bivariate_plotting <- st_simplify(hfi_thresholds_bivariate_plotting, dTolerance = 75) 

bivariate_map <- ggplot() +
  geom_sf(data=hfi_thresholds_bivariate_plotting, aes(fill=fill), color="lightgrey", size=0.00001) +
  scale_fill_identity() +
  #scale_fill_manual(name="Change in prop of pop\nexposed to HFI>8\nfrom 2001-2019", na.value="grey", 
  #                  values = (brewer.pal(7, "Purples"))) +
  theme_minimal() +
  no_axis +
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.position='right')

#legend
bivariate_color_scale %<>%
  separate(group, into = c("hfi", "dengue"), sep = " - ") %>%
  mutate(hfi = as.integer(hfi),
         dengue = as.integer(dengue))

bivariate_legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = hfi,
      y = dengue,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher HFI>8 exposure ->",
       y = "More recent dengue ->") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6),
    axis.title.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()

bivariate_map_legend <- ggdraw() +
  draw_plot(bivariate_map, 0, 0, 1, 1) +
  draw_plot(bivariate_legend, 0.05, 0.075, 0.2, 0.2)

ggsave("~/Desktop/bivariate_w_legend.pdf", bivariate_map_legend, device="pdf", width = 6, height=5, dpi=100, units=c("in"))
