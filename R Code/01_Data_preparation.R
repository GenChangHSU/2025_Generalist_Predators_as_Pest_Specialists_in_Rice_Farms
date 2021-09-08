## ------------------------------------------------------------------------
## Title: Data Preparation
##
## Author: Gen-Chang Hsu
##
## Date: 2021-03-09
##
## Description: Tidy up the three years of stable isotope datasets
##
##
## Notes:
##
##
## ------------------------------------------------------------------------
set.seed(123)


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)


# Import files ------------------------------------------------------------
SID_2017_raw <- read_csv("./Data_raw/SID_2017.csv")
SID_2018_raw <- read_csv("./Data_raw/SID_2018.csv")
SID_2019_raw <- read_csv("./Data_raw/SID_2019.csv")


# Code starts here ---------------------------------------------------------

### Reorganize the datasets into an uniform format
SID_2017 <- SID_2017_raw %>%
  filter(Trophic %in% c("Predator", "Herbivore", "Detritivore")) %>%
  unite("Farm_ID", Sur_env, Farm_type, sep = "", remove = F) %>%
  mutate(Farm_ID = str_sub(Farm_ID, start = 1, end = 2),
         Species = str_sub(Species, start = 1, end = 3),
         Species = toupper(Species),
         Stage = plyr::mapvalues(Stage, from = "Heading", to = "Flowering"),
         Year = 2017) %>%
  select(Sample_ID = ID, Family = Species, d13C = d_13C, C_conc = `Conc_C_%`, 
         d15N = d_15N, N_conc = `Conc_N_%`, Farmtype = Farm_type, 
         Stage = Stage, Farm_ID, Year)

SID_2018 <- SID_2018_raw %>%
  mutate(C_conc = C.amount_ug/(Total.amount_mg*1000)*100, 
         N_conc = N.amount_ug/(Total.amount_mg*1000)*100,
         Year = 2018) %>%
  select(Sample_ID = Sample.ID, Family, d13C, C_conc, d15N, N_conc,
         Farmtype, Stage, Farm_ID = Farm.ID, Year)
  
SID_2019 <- SID_2019_raw %>%
  mutate(C_conc = C.amount_ug/(Total.amount_mg*1000)*100, 
         N_conc = N.amount_ug/(Total.amount_mg*1000)*100) %>%
  select(Sample_ID = Sample.ID, Family, d13C, C_conc, d15N, N_conc,
         Farmtype, Stage, Farm_ID = Farm.ID, Year)

SID_all_clean <- bind_rows(SID_2017, SID_2018, SID_2019) %>%
  mutate(Farm = str_c(Farm_ID, Year, sep = "_"))

write_rds(SID_all_clean, "Output/Data_clean/SID_all_clean.rds")


