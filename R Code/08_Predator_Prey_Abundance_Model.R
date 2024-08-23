## -----------------------------------------------------------------------------
## Title: Analyze predator and prey abundance in rice farms
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-22
##
## Description:
## 1. Organize the predator and prey data
## 2. Analyze the effects of farm type, crop stage, and year on predator abundance
## 3. Analyze the effects of farm type, crop stage, and year on rice herbivore abundance 
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(glmmTMB)
library(DHARMa)
library(performance)
library(lmtest)
library(car)
library(sjPlot)
library(emmeans)
library(multcomp)
library(grid)
library(ggplotify)
library(ggeffects)
library(officer)


# Import files -----------------------------------------------------------------
forest_cover <- read_csv("Data_raw/forest_cover.csv")
arthropod_abd_2017_raw <- read_xlsx("Data_raw/arthropod_abd_2017.xlsx", sheet = 1)
arthropod_abd_2018_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
arthropod_abd_2019_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)

############################### Code starts here ###############################

# 1. Organize the predator and prey data ---------------------------------------
### Arthropod abundance data
predator <- c("ARA", "TET", "COC")
rice_herb <- c("DEL", "CIC", "PEN", "ALY", "LYG")

predator_prey_abd_2017_clean <- arthropod_abd_2017_raw %>%
  select(Farm_ID = Farm, Stage, Family = Family.ID, Abundance = Count) %>%
  filter(Stage != "Seedling") %>%
  mutate(Family = toupper(Family)) %>%
  filter(Family %in% c(predator, rice_herb)) %>%
  mutate(Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% predator ~ "Predator")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate(Year = 2017)

predator_prey_abd_2018_clean <- arthropod_abd_2018_raw %>%
  select(Date, Farm_ID = Farm, Family = Family.abbr, Abundance) %>%
  filter(Date != "20180402") %>%
  mutate(Farm_ID = str_remove(Farm_ID, pattern = "-"),
         Family = toupper(Family)) %>%
  filter(Family %in% c(rice_herb, predator)) %>%
  mutate(Stage = case_when(Date == "20180506" ~ "Tillering", 
                           Date == "20180608" ~ "Flowering",
                           Date == "20180629" ~ "Ripening"),
         Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% predator ~ "Predator")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate(Year = 2018)

predator_prey_abd_2019_clean <- arthropod_abd_2019_raw %>%
  select(Date, Farm_ID = Farm, Family = Family.abbr, Abundance) %>%
  mutate(Farm_ID = str_remove(Farm_ID, pattern = "-"),
         Family = toupper(Family)) %>%
  filter(Family %in% c(rice_herb, predator)) %>%
  mutate(Stage = case_when(Date == "20190513" ~ "Tillering", 
                           Date == "20190620" ~ "Flowering",
                           Date == "20190702" ~ "Ripening"),
         Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% predator ~ "Predator")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate(Year = 2019)

predator_prey_abd_clean <- bind_rows(predator_prey_abd_2017_clean, 
                                 predator_prey_abd_2018_clean,
                                 predator_prey_abd_2019_clean) %>%
  select(Year, Farm_ID, Stage, Source, Abundance) %>%
  mutate(Year = as.character(Year)) %>% 
  mutate(Farmtype = str_extract(Farm_ID, pattern = "O|C"),
         Farmtype = if_else(Farmtype == "O", "Organic", "Conventional")) %>% 
  pivot_wider(names_from = "Source", values_from = "Abundance") %>% 
  left_join(forest_cover, by = join_by("Farm_ID"))


# 2. Effects of farm type, crop stage, and year on predator abundance ----------






# 3. Effects of farm type, crop stage, and year on prey abundance --------------


