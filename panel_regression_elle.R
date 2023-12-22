setwd("/Users/eskinner/Desktop/RESEARCH/Stanford_Postdoc/R/land_use")

library(tidyverse)
library(dplyr)

####ADD AND MERGE INCIDENCE DATA
annual_incidence <- read.csv("annual_incidence_13to20.csv", header = T)
annual_incidence <- annual_incidence %>%  
  mutate(CD_MUN = as.character(CD_MUN)) %>%
  mutate(year = as.character(year))

annual_cases <- read.csv("annual_cases_13to20.csv", header = T)
annual_cases <- annual_cases %>%  
  mutate(year = as.character(year))

hfp_clean <- read.csv("hfp_clean.csv")
hfp_clean <- hfp_clean %>%  
  mutate(CD_MUN = as.character(CD_MUN)) %>%
  mutate(year = as.character(year))

pop_clean <- read.csv("pop_clean.csv")
pop_clean <- pop_clean %>%  
  mutate(CD_MUN = as.character(CD_MUN)) %>%
  mutate(year = as.character(year))

hfp_cases <-annual_cases %>% 
  left_join(hfp_clean, by = (c("CD_MUN", "year"))) %>% 
  left_join(pop_clean, by = (c("CD_MUN", "year")))

hfp_cases[is.na(hfp_cases)] <- 0.001

hfp_incidence <-annual_incidence %>% 
  left_join(hfp_clean, by = (c("CD_MUN", "year"))) %>% 
  left_join(pop_clean, by = (c("CD_MUN", "year")))
hfp_incidence[is.na(hfp_incidence)] <- 0.001

#convert year to date format
library(lubridate)
annual_incidence <- transform(annual_incidence, year = as.Date(as.character(year), format = "%Y"))

SIGLA_UF <- pop %>% dplyr::select(CD_MUN, SIGLA_UF)
annual_incidence <-annual_incidence %>% 
  left_join(SIGLA_UF, by = "CD_MUN")
hfp_clean <-hfp_clean %>% 
  left_join(SIGLA_UF, by = "CD_MUN")

ggplot(subset(annual_incidence, CD_MUN %in% c("310180", "311940", "310670", "313055", "313920")), aes(x = year, y = log10(incidence), group=CD_MUN, colour=CD_MUN)) +
  geom_line() + geom_point() + facet_wrap(~pathogen)

ggplot(subset(hfp_clean, CD_MUN %in% c("310180", "311940", "310670", "313055", "313920", "430210", "316292")), aes(x = year, y = index, group=CD_MUN, colour=CD_MUN)) +
  geom_line() + geom_point() 

ggplot(hfp_clean, aes(x = year, y = index, group=CD_MUN)) +
  geom_line() + geom_point() + facet_wrap(~SIGLA_UF)

##summary plot
hfp_cases[hfp_cases == 0] <- NA
hfp_cases<-hfp_cases[!(hfp_cases$year=="2020"),]
hfp_cases<-hfp_cases[!(hfp_cases$CD_MUN=="430000"),]

ggplot(subset(hfp_incidence, pathogen %in% c("c_leish", "chikv", "denv", "malaria", "v_leish", "zika")), aes(x=index, y=log10(incidence))) +
  geom_point(aes(colour = pathogen)) +
  facet_wrap(~ pathogen)    

  
  ggplot(subset(df, dose %in% c("D0.5", "D1")), aes(x = dose, y = len))+
    geom_col(aes(fill = supp), width = 0.7) +
    scale_fill_viridis_d()

#Panel regressions  
install.packages("pglm")
library(pglm)


#cases
#Cutaneous leishmaniasis
c_leish_hfp <- hfp_cases[ which(hfp_cases$pathogen=='c_leish'), ]
c_leish_hfp[c_leish_hfp == 0] <- 0.0001
c_leish_hfp$log_cases <- log10(c_leish_hfp$cases)
c_leish_hfp <- c_leish_hfp[!duplicated(c_leish_hfp[c(2,4)]),]
c_leish_hfp2 <- na.omit(c_leish_hfp)

c_leish_fixed_model <- pglm(cases ~ index + population, data = c_leish_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(c_leish_fixed_model)

#Chikungunya
chik_hfp <- hfp_cases[ which(hfp_cases$pathogen=='chikv'), ]
chik_hfp[chik_hfp == 0] <- 0.0001
chik_hfp$log_cases <- log10(chik_hfp$cases)
chik_hfp <- chik_hfp[!duplicated(chik_hfp[c(2,4)]),]

chik_fixed_model <- pglm(cases ~ index + population, data = chik_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(chik_fixed_model)

#Dengue
denv_hfp <- hfp_cases[ which(hfp_cases$pathogen=='denv'), ]
denv_hfp[denv_hfp == 0] <- 0.0001
denv_hfp$log_cases <- log10(denv_hfp$cases)
denv_hfp <- denv_hfp[!duplicated(denv_hfp[c(2,4)]),]

denv_fixed_model <- pglm(cases ~ index + population, data = denv_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(denv_fixed_model)

#Malaria
malaria_hfp <- hfp_cases[ which(hfp_cases$pathogen=='malaria'), ]
malaria_hfp[malaria_hfp == 0] <- 0.0001
malaria_hfp$log_cases <- log10(malaria_hfp$cases)
malaria_hfp <- malaria_hfp[!duplicated(malaria_hfp[c(2,4)]),]
malaria_hfp2 <- na.omit(malaria_hfp)

malria_fixed_model <- pglm(cases ~ index + population, data = malaria_hfp2, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(malria_fixed_model)

#Visceral leishmaniasis
v_leish_hfp <- hfp_cases[ which(hfp_cases$pathogen=='v_leish'), ]
v_leish_hfp[v_leish_hfp == 0] <- 0.0001
v_leish_hfp$log_cases <- log10(v_leish_hfp$cases)
v_leish_hfp <- v_leish_hfp[!duplicated(v_leish_hfp[c(2,4)]),]

v_leish_fixed_model <- pglm(cases ~ index + population, data = v_leish_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson())
summary(v_leish_fixed_model)

#Yellow fever virus
yfv_hfp <- hfp_cases[ which(hfp_cases$pathogen=='yfv'), ]
yfv_hfp[yfv_hfp == 0] <- 0.0001
yfv_hfp$log_cases <- log10(yfv_hfp$cases)
yfv_hfp <- yfv_hfp[!duplicated(yfv_hfp[c(2,4)]),]

yfv_fixed_model <- pglm(cases ~ index + population, data = yfv_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(yfv_fixed_model)

#Zika
zika_hfp <- hfp_cases[ which(hfp_cases$pathogen=='zika'), ]
zika_hfp[zika_hfp == 0] <- 0.0001
zika_hfp$log_cases <- log10(zika_hfp$cases)
zika_hfp <- zika_hfp[!duplicated(zika_hfp[c(2,4)]),]

zika_fixed_model <- pglm(cases ~ index + population, data = zika_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(zika_fixed_model)


#incidence
#Cutaneous leishmaniasis
c_leish_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='c_leish'), ]
c_leish_hfp[c_leish_hfp == 0] <- 0.0001
c_leish_hfp$log_incidence <- log10(c_leish_hfp$incidence)
c_leish_hfp <- c_leish_hfp[!duplicated(c_leish_hfp[c(2,3)]),]

c_leish_fixed_model <- pglm(log_incidence ~ index + population, data = c_leish_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(c_leish_fixed_model)

#Chikungunya
chik_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='chikv'), ]
chik_hfp[chik_hfp == 0] <- 0.0001
chik_hfp$log_incidence <- log10(chik_hfp$incidence)
chik_hfp <- chik_hfp[!duplicated(chik_hfp[c(2,3)]),]

chik_fixed_model <- pglm(log_incidence ~ index + population, data = chik_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(chik_fixed_model)

#Dengue
denv_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='denv'), ]
denv_hfp[denv_hfp == 0] <- 0.0001
denv_hfp$log_incidence <- log10(denv_hfp$incidence)
denv_hfp <- denv_hfp[!duplicated(denv_hfp[c(2,3)]),]

denv_fixed_model <- pglm(log_incidence ~ index + population, data = denv_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(denv_fixed_model)

#Malaria
malaria_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='malaria'), ]
malaria_hfp[malaria_hfp == 0] <- 0.0001
malaria_hfp$log_incidence <- log10(malaria_hfp$incidence)
malaria_hfp <- malaria_hfp[!duplicated(malaria_hfp[c(2,3)]),]

malria_fixed_model <- pglm(log_incidence ~ index + population, data = malaria_hfp, model = "within", index = c("CD_MUN", "year"), family = poisson)
summary(malria_fixed_model)

#Visceral leishmaniasis
v_leish_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='v_leish'), ]
v_leish_hfp[v_leish_hfp == 0] <- 0.0001
v_leish_hfp$log_incidence <- log10(v_leish_hfp$incidence)
v_leish_hfp <- v_leish_hfp[!duplicated(v_leish_hfp[c(2,3)]),]

v_leish_fixed_model <- plm(log_incidence ~ index + population, data = v_leish_hfp, model = "within", index = c("CD_MUN", "year"))
summary(v_leish_fixed_model)

#Yellow fever virus
yfv_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='yfv'), ]
yfv_hfp[yfv_hfp == 0] <- 0.0001
yfv_hfp$log_incidence <- log10(yfv_hfp$incidence)
yfv_hfp <- yfv_hfp[!duplicated(yfv_hfp[c(2,3)]),]

yfv_fixed_model <- plm(log_incidence ~ index + population, data = yfv_hfp, model = "within", index = c("CD_MUN", "year"))
summary(yfv_fixed_model)

#Zika
zika_hfp <- hfp_incidence[ which(hfp_incidence$pathogen=='zika'), ]
zika_hfp[zika_hfp == 0] <- 0.0001
zika_hfp$log_incidence <- log10(zika_hfp$incidence)
zika_hfp <- zika_hfp[!duplicated(zika_hfp[c(2,3)]),]

zika_fixed_model <- plm(log_incidence ~ index + population, data = zika_hfp, model = "within", index = c("CD_MUN", "year"))
summary(zika_fixed_model)

###find duplicate rows
c_leish_hfp2$unique_id <- paste(c_leish_hfp2$CD_MUN,c_leish_hfp$year) # concatenate to make unique ID
c_leish_hfp$duplicate = duplicated(c_leish_hfp$unique_id) # generate the duplicate variable
subset(c_leish_hfp, duplicate=="TRUE") # find the duplicate



