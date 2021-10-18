## -----------------------------------------------------------------------------
## Title: Relative abundances of rice herbivore families/genera in the study farms
##
## Author: Gen-Chang Hsu
##
## Date: 2021-10-18
##
## Description: 
##   (1) Summarize the relative abundances of the rice herbivore families/genera
##       at the flowering and ripening stage in the three study years
##
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(janitor)


# Import files -----------------------------------------------------------------
Abd_2017 <- read.csv("Data_raw/arthropod_abd_2017.csv")
Abd_2018 <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
Abd_2019 <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)


############################### Code starts here ###############################

### Rice herbivore families in the study years
Rice_family_2017 <- c("Cic", "Del", "Lyg", "Pen", "Hes", "Pyl", "Nym")
Abd_2017Rice_family_2018 <- c("Cic", "Del", "Lyg", "Pen", "Hes", "Pyl", "Pyr", "Aly")
Rice_family_2019 <- c("Cic", "Del", "Lyg", "Pen", "Hes", "Pyl", "Pyr", "Aly",
                      "Agr", "Cor", "Mir", "Ric")

Rice_herb_data_2017 <- Abd_2017 %>% 
  filter(Family.ID %in% Rice_family_2017) %>%
  select(Year, Stage, Family = Family.ID, Count) %>%
  mutate(Family = case_when(!(Family %in% c("Cic", "Del", "Lyg", "Pen")) ~ "Others",
                               TRUE ~ Family)) %>%
  filter(Stage %in% c("Flowering", "Ripening")) %>%
  mutate(Year = as.factor(Year))
  

Rice_herb_data_2018 <- Abd_2018 %>% 
  filter(Family.abbr %in% Rice_family_2018) %>%
  select(Date, Family.abbr, Abundance) %>%
  mutate(Family.abbr = case_when(!(Family.abbr %in% c("Cic", "Del", "Lyg", "Pen")) ~ "Others",
                               TRUE ~ Family.abbr)) %>%
  mutate(Year = str_sub(Date, start = 1, end = 4),
         Date = str_sub(Date, start = 5, end = 8)) %>%
  filter(Date %in% c("0608", "0629")) %>%
  mutate(Stage = case_when(Date == "0608" ~ "Flowering",
                           Date == "0629" ~ "Ripening")) %>%
  select(Year, Stage, Family = Family.abbr, Count = Abundance)


Rice_herb_data_2019 <- Abd_2019 %>% 
  mutate(Family.abbr = str_to_title(Family.abbr)) %>%
  filter(Family.abbr %in% Rice_family_2019) %>%
  select(Date, Family.abbr, Abundance) %>%
  mutate(Family.abbr = case_when(!(Family.abbr %in% c("Cic", "Del", "Lyg", "Pen")) ~ "Others",
                                 TRUE ~ Family.abbr)) %>%
  mutate(Year = str_sub(Date, start = 1, end = 4),
         Date = str_sub(Date, start = 5, end = 8)) %>%
  filter(Date %in% c("0620", "0702")) %>%
  mutate(Stage = case_when(Date == "0620" ~ "Flowering",
                           Date == "0702" ~ "Ripening")) %>%
  select(Year, Stage, Family = Family.abbr, Count = Abundance)


bind_rows(Rice_herb_data_2017, 
          Rice_herb_data_2018,
          Rice_herb_data_2019) %>%
  group_by(Year, Stage, Family) %>%
  summarise(N = sum(Count)) %>%
  mutate(Prop = round(N/sum(N), 3)*100,
         Prop = paste0(Prop, "%"),
         Family = factor(Family, levels = c("Cic", "Del", "Lyg", "Pen", "Others"))) %>%
  select(-N) %>% 
  full_join(., expand(., Year, Stage, Family)) %>%
  arrange(Year, Stage, Family) %>%
  pivot_wider(names_from = Year, values_from = Prop) %>%
  write_csv("./Output/Data_clean/Rice_herb_rel_abd.csv")
  



