
rm(list = ls())
library(tidyverse)
library(readxl)

# data_r10 <- read.csv("C:/Users/lamw/Documents/AR6_Scenarios_Database_R10_regions_v1.1.csv")
# data_r6 <- read.csv("C:/Users/lamw/Documents/AR6_Scenarios_Database_R6_regions_v1.1.csv")
data_r5 <- read.csv("C:/Users/lamw/ownCloud/Resources/sources/AR6 scenario database/AR6_Scenarios_Database_R5_regions_v1.1.csv")
data_r1 <- read.csv("C:/Users/lamw/ownCloud/Resources/sources/AR6 scenario database/AR6_Scenarios_Database_World_v1.1.csv")

metadata <- read_excel("C:/Users/lamw/ownCloud/Resources/sources/AR6 scenario database/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx",sheet=2)
metadata_category <- metadata %>%
  filter(Category %in% c("C1","C2","C3"))

## filter by variables

vars <- data.frame(var=unique(data_r1$Variable))

vars <- vars %>%
  mutate(keep=0) %>%
  mutate(keep=ifelse(grepl("Carbon Sequestration",var),1,0)) %>%
  mutate(keep=ifelse(grepl("Emissions",var),1,keep)) %>%
  mutate(keep=ifelse(grepl("Population",var),1,keep)) %>%
  mutate(keep=ifelse(grepl("Primary Energy",var),1,keep)) %>% 
  arrange(var)

vars <- vars %>% 
  arrange(var) %>% 
#  mutate(keep=1) %>% 
  mutate(keep=ifelse(grepl("BC",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("C2F6",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("CF4",var),0,keep)) %>% 
  # #mutate(keep=ifelse(grepl("|CO",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("HFC",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("NH3",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("NOx",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("OC",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("PFC",var),0,keep)) %>% 
  # mutate(keep=ifelse(grepl("SF6",var),0,keep)) %>% 
  mutate(keep=ifelse(grepl("Sulfur",var),0,keep)) %>% 
  mutate(keep=ifelse(grepl("VOC",var),0,keep)) %>% 
  mutate(keep=ifelse(grepl("Investment",var),0,keep)) %>% 
  mutate(keep=ifelse(grepl("Price",var),0,keep)) %>% 
  mutate(keep=ifelse(grepl("Capacity",var),0,keep)) %>% 
  mutate(keep=ifelse(grepl("Capital Cost",var),0,keep)) %>% 
  filter(keep==1)
  

data_r1 <- left_join(data_r1,vars,by=c("Variable"="var")) %>% filter(keep==1)
data_r5 <- left_join(data_r5,vars,by=c("Variable"="var")) %>% filter(keep==1)
# data_r6 <- left_join(data_r6,vars,by=c("Variable"="var")) %>% filter(keep==1)
# data_r10 <- left_join(data_r10,vars,by=c("Variable"="var")) %>% filter(keep==1)


## filter by C1-C3 categories

scenario_categories <- read.csv("Data/scenario_categories.csv")

data_r1 <- left_join(data_r1,scenario_categories %>% select(Model=model,Scenario=scenario,Category),by = c("Model", "Scenario")) %>% 
  filter(Category %in% c("C1","C2","C3"))

data_r5 <- left_join(data_r5,scenario_categories %>% select(Model=model,Scenario=scenario,Category),by = c("Model", "Scenario")) %>% 
  filter(Category %in% c("C1","C2","C3"))

# data_r6 <- left_join(data_r6,scenario_categories %>% select(Model=model,Scenario=scenario,Category),by = c("Model", "Scenario")) %>% 
#   filter(Category %in% c("C1","C2","C3"))
# 
# data_r10 <- left_join(data_r10,scenario_categories %>% select(Model=model,Scenario=scenario,Category),by = c("Model", "Scenario")) %>% 
#   filter(Category %in% c("C1","C2","C3"))


## gather years

data_r1 <- gather(data_r1,year,value,X1995:X2100)
data_r1$year <- gsub("X","",data_r1$year)

data_r5 <- gather(data_r5,year,value,X1990:X2100)
data_r5$year <- gsub("X","",data_r5$year)

# data_r6 <- gather(data_r6,year,value,X1990:X2100)
# data_r6$year <- gsub("X","",data_r6$year)
# 
# data_r10 <- gather(data_r10,year,value,X1990:X2100)
# data_r10$year <- gsub("X","",data_r10$year)

## tidy up

data_r1 <- data_r1 %>% 
  select(model=Model,scenario=Scenario,category=Category,var=Variable,unit=Unit,region=Region,year,value) %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year>=2010)

data_r5 <- data_r5 %>% 
  select(model=Model,scenario=Scenario,category=Category,var=Variable,unit=Unit,region=Region,year,value) %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year>=2010)

## infill the years

data_r1 <- data_r1 %>%
  filter(year>=2020) %>%
  group_by(model,scenario,category,var,unit,region) %>%
  mutate(value=zoo::na.approx(value,na.rm=FALSE))

data_r5 <- data_r5 %>%
  filter(year>=2020) %>%
  group_by(model,scenario,category,var,unit,region) %>%
  mutate(value=zoo::na.approx(value,na.rm=FALSE))

## save

save(data_r1,data_r5,file="Data/scenario_data.RData")
