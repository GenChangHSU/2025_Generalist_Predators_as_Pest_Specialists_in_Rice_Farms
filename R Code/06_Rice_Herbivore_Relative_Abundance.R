## -----------------------------------------------------------------------------
## Title: Summary of the prey abundance in the study farms
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-28
##
## Description: 
## 1. Summarize the relative abundance of the rice herbivore families/genera
##    at the flowering and ripening stage in the three study years
## 2. Summarize the abundance of the three prey guilds at the flowering and ripening 
##    stage in the three study years
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(janitor)


# Import files -----------------------------------------------------------------
Abd_2017 <- read_xlsx("Data_raw/arthropod_abd_2017.xlsx", sheet = 1)
Abd_2018 <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
Abd_2019 <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)
arthropod_abd_clean <- read_rds("./Output/Data_clean/arthropod_abd_clean.rds")


############################### Code starts here ###############################

# 1. Summarize the relative abundances of rice herbivore families --------------
### Rice herbivore families in the three study years
Rice_family_2017 <- c("Cic", "Del", "Lyg", "Pen", "Hes", "Pyl", "Nym")
Rice_family_2018 <- c("Cic", "Del", "Lyg", "Pen", "Hes", "Pyl", "Pyr", "Aly")
Rice_family_2019 <- c("Cic", "Del", "Lyg", "Pen", "Hes", "Pyl", "Pyr", "Aly",
                      "Agr", "Cor", "Mir", "Ric")

Rice_herb_data_2017 <- Abd_2017 %>% 
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>% 
  filter(Family.ID %in% Rice_family_2017) %>%
  select(Year, Farm_type, Stage, Family = Family.ID, Count) %>%
  mutate(Family = case_when(!(Family %in% c("Cic", "Del", "Lyg", "Pen")) ~ "Others",
                               TRUE ~ Family)) %>%
  filter(Stage %in% c("Flowering", "Ripening")) %>%
  mutate(Year = as.factor(Year))
  
Rice_herb_data_2018 <- Abd_2018 %>% 
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>% 
  filter(Family.abbr %in% Rice_family_2018) %>%
  select(Date, Farm_type, Family.abbr, Abundance) %>%
  mutate(Family.abbr = case_when(!(Family.abbr %in% c("Cic", "Del", "Lyg", "Pen")) ~ "Others",
                               TRUE ~ Family.abbr)) %>%
  mutate(Year = str_sub(Date, start = 1, end = 4),
         Date = str_sub(Date, start = 5, end = 8)) %>%
  filter(Date %in% c("0608", "0629")) %>%
  mutate(Stage = case_when(Date == "0608" ~ "Flowering",
                           Date == "0629" ~ "Ripening")) %>%
  select(Year, Farm_type, Stage, Family = Family.abbr, Count = Abundance)

Rice_herb_data_2019 <- Abd_2019 %>% 
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>% 
  mutate(Family.abbr = str_to_title(Family.abbr)) %>%
  filter(Family.abbr %in% Rice_family_2019) %>%
  select(Date, Farm_type, Family.abbr, Abundance) %>%
  mutate(Family.abbr = case_when(!(Family.abbr %in% c("Cic", "Del", "Lyg", "Pen")) ~ "Others",
                                 TRUE ~ Family.abbr)) %>%
  mutate(Year = str_sub(Date, start = 1, end = 4),
         Date = str_sub(Date, start = 5, end = 8)) %>%
  filter(Date %in% c("0620", "0702")) %>%
  mutate(Stage = case_when(Date == "0620" ~ "Flowering",
                           Date == "0702" ~ "Ripening")) %>%
  select(Year, Farm_type, Stage, Family = Family.abbr, Count = Abundance)

bind_rows(Rice_herb_data_2017, 
          Rice_herb_data_2018,
          Rice_herb_data_2019) %>%
  group_by(Year, Farm_type, Stage, Family) %>%
  summarise(N = sum(Count)) %>%
  mutate(Prop = round(N/sum(N), 3)*100,
         Prop = paste0(Prop, "%"),
         Family = factor(Family, levels = c("Cic", "Del", "Lyg", "Pen", "Others"))) %>%
  select(-N) %>% 
  pivot_wider(names_from = c(Year, Farm_type), values_from = Prop) %>%
  arrange(Stage, Family) %>%
  write_csv("./Output/Data_clean/Rice_Herbivore_Relative_Abundance.csv")
  

# 2. Summarize the abundance of prey guilds ------------------------------------
arthropod_abd_clean %>% 
  drop_na() %>% 
  mutate(Farmtype = str_extract(Farm_ID, pattern = "O|C"),
         Farmtype = factor(Farmtype, levels = c("O", "C"), labels = c("Organic", "Conventional"))) %>% 
  group_by(Year, Farmtype, Stage, Source) %>% 
  summarize(Mean = round(mean(Abundance, na.rm = T), 1),
            n = n(),
            SE = round(sd(Abundance, na.rm = T)/sqrt(n), 1)) %>% 
  filter(Stage %in% c("Flowering", "Ripening")) %>% 
  ungroup() %>% 
  mutate(SE = as.character(SE),
         SE = if_else(is.na(SE), "", SE)) %>% 
  mutate(Mean_SE = str_c(Mean, SE, sep = " ± ")) %>% 
  select(-Mean, -n, -SE) %>% 
  pivot_wider(names_from = c(Year, Farmtype), values_from = Mean_SE) %>% 
  mutate(Source = fct_relevel(Source, "Rice_herb", "Tour_herb", "Detritivore")) %>% 
  arrange(Stage, Source) %>% 
  write_csv("./Output/Data_clean/Prey_Guild_Abundance.csv")

