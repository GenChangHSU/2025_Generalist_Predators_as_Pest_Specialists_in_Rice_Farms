## ------------------------------------------------------------------------
## Title: Relationships between rice herbivore consumption by predators and 
##        various abiotic/biotic factors
## Author: Gen-Chang Hsu
##
## Date: 2021-03-12
##
## Description: Fit beta regression models to examine the relationships between 
## the proportion of rice herbivore consumed in predators' diet and various 
## abiotic (Farmtype, crop stage, year, surrounding forest cover) and biotic factors 
## (relative abundance of rice herbivores) 
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
library(betareg)
library(lmtest)
library(emmeans)
library(car)


# Import files ------------------------------------------------------------
model_out_clean <- readRDS("Output/Data_clean/model_out_clean.rds")
forest_cover <- read_csv("Data_raw/forest_cover.csv")
arthropod_abd_2017_raw <- read_csv("Data_raw/arthropod_abd_2017.csv")
arthropod_abd_2018_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
arthropod_abd_2019_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)  


# Code starts here ---------------------------------------------------------

### Tidy up the arthropod abundance datasets
# Prey guild assignment
rice_herb <- c("DEL", "CIC", "PEN", "ALY", "LYG")
tour_herb <- c("ACR", "CHR")
detritivore <- c("CHI", "SCI", "MUS", "EPH", "EMP", "STR", "CHL", "TER")

arthropod_abd_2017_clean <- arthropod_abd_2017_raw %>%
  select(Farm_ID = Farm, Stage, Family = Family.ID, Abundance = Count) %>%
  filter(Stage != "Seedling") %>%
  mutate(Family = toupper(Family)) %>%
  filter(Family %in% c(rice_herb, tour_herb, detritivore)) %>%
  mutate(Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% tour_herb ~ "Tour_herb",
                            Family %in% detritivore ~ "Detritivore")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup() %>%
  group_by(Farm_ID, Stage) %>%
  mutate(Rel_abd = Abundance/sum(Abundance)) %>%
  mutate(Year = 2017)
  
arthropod_abd_2018_clean <- arthropod_abd_2018_raw %>%
  select(Date, Farm_ID = Farm, Family = Family.abbr, Abundance) %>%
  filter(Date != "20180402") %>%
  mutate(Farm_ID = str_remove(Farm_ID, pattern = "-"),
         Family = toupper(Family)) %>%
  filter(Family %in% c(rice_herb, tour_herb, detritivore)) %>%
  mutate(Stage = case_when(Date == "20180506" ~ "Tillering", 
                           Date == "20180608" ~ "Flowering",
                           Date == "20180629" ~ "Ripening"),
         Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% tour_herb ~ "Tour_herb",
                            Family %in% detritivore ~ "Detritivore")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup() %>%
  group_by(Farm_ID, Stage) %>%
  mutate(Rel_abd = Abundance/sum(Abundance)) %>%
  mutate(Year = 2018)

arthropod_abd_2019_clean <- arthropod_abd_2019_raw %>%
  select(Date, Farm_ID = Farm, Family = Family.abbr, Abundance) %>%
  mutate(Farm_ID = str_remove(Farm_ID, pattern = "-"),
         Family = toupper(Family)) %>%
  filter(Family %in% c(rice_herb, tour_herb, detritivore)) %>%
  mutate(Stage = case_when(Date == "20190513" ~ "Tillering", 
                           Date == "20190620" ~ "Flowering",
                           Date == "20190702" ~ "Ripening"),
         Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% tour_herb ~ "Tour_herb",
                            Family %in% detritivore ~ "Detritivore")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  ungroup() %>%
  group_by(Farm_ID, Stage) %>%
  mutate(Rel_abd = Abundance/sum(Abundance)) %>%
  mutate(Year = 2019)

arthropod_abd_clean <- bind_rows(arthropod_abd_2017_clean, 
                                 arthropod_abd_2018_clean,
                                 arthropod_abd_2019_clean) %>%
  select(Year, Farm_ID, Stage, Source, Abundance, Rel_abd) %>%
  mutate(Year = as.character(Year))

write_rds(arthropod_abd_clean, "Output/Data_clean/arthropod_abd_clean.rds")


### Merge diet proportion dataset, arthropod abundance dataset, and forest cover dataset
rice_herb_consmp_all <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, Rel_abd, Source, Proportion = Mean) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "All") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening")))
  
rice_herb_consmp_spiders <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, Rel_abd, Source, Proportion = Mean) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "Spider") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening")))

rice_herb_consmp_ladybeetles <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, Rel_abd, Source, Proportion = Mean) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "Ladybeetle") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening")))


### Beta regression models for all predators
# Initial model specification
beta_out_all1 <- betareg(Proportion ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                     data = rice_herb_consmp_all)

summary(beta_out_all1)
lrtest(beta_out_all1)  # Test for global significance of the model

plot(fitted(beta_out_all1), residuals(beta_out_all1))  # Fitted vs. residuals plot
plot(beta_out_all1, which = 1)  # Residual vs. indices of observations
plot(beta_out_all1, which = 2)  # Cook's distance plot
plot(beta_out_all1, which = 3)  # Leverage plot
plot(beta_out_all1, which = 5)  # Half-normal plot of residuals
plot(beta_out_all1, which = 6)  # Fitted vs. observed values

large_res_obs_all <- residuals(beta_out_all1) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # Three observations with large residuals

# Refit the model excluding the three observations with large residuals
rice_herb_consmp_all2 <- rice_herb_consmp_all[-large_res_obs_all, ]
beta_out_all2 <- update(beta_out_all1, data = rice_herb_consmp_all2)

summary(beta_out_all2)
lrtest(beta_out_all2)  # Test for global significance of the model

plot(fitted(beta_out_all2), residuals(beta_out_all2))  # Fitted vs. residuals plot
plot(beta_out_all2, which = 1)  # Residual vs. indices of observations
plot(beta_out_all2, which = 2)  # Cook's distance plot
plot(beta_out_all2, which = 3)  # Leverage plot
plot(beta_out_all2, which = 5)  # Half-normal plot of residuals
plot(beta_out_all2, which = 6)  # Fitted vs. observed values

Anova(beta_out_all2)

# Test for interactions between significant factors
beta_out_all3 <- betareg(Proportion ~ Year + Farmtype * Stage + `Forest_cover_%` + Rel_abd, 
                         data = rice_herb_consmp_all2)

summary(beta_out_all3)
lrtest(beta_out_all3)
Anova(beta_out_all3)  # Interaction term not significant (P = 0.36)

# Post-hoc tests for significant factors
multcomp_farmtype_all <- emmeans(beta_out_all3, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_all <- emmeans(beta_out_all3, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")


### Beta regression models for spiders
# Initial model specification
beta_out_spiders1 <- betareg(Proportion ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                         data = rice_herb_consmp_spiders)
summary(beta_out_spiders1)
lrtest(beta_out_spiders1)  # Test for global significance of the model

plot(fitted(beta_out_spiders1), residuals(beta_out_spiders1))  # Fitted vs. residuals plot
plot(beta_out_spiders1, which = 1)  # Residual vs. indices of observations
plot(beta_out_spiders1, which = 2)  # Cook's distance plot
plot(beta_out_spiders1, which = 3)  # Leverage plot
plot(beta_out_spiders1, which = 5)  # Half-normal plot of residuals
plot(beta_out_spiders1, which = 6)  # Fitted vs. observed values

large_res_obs_spiders <- residuals(beta_out_spiders1) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # Three observations with large residuals

# Refit the model excluding the three observations with large residuals
rice_herb_consmp_spiders2 <- rice_herb_consmp_spiders[-large_res_obs_spiders, ]
beta_out_spiders2 <- update(beta_out_spiders1, data = rice_herb_consmp_spiders2)

summary(beta_out_spiders2)
lrtest(beta_out_spiders2)  # Test for global significance of the model

plot(fitted(beta_out_spiders2), residuals(beta_out_spiders2))  # Fitted vs. residuals plot
plot(beta_out_spiders2, which = 1)  # Residual vs. indices of observations
plot(beta_out_spiders2, which = 2)  # Cook's distance plot
plot(beta_out_spiders2, which = 3)  # Leverage plot
plot(beta_out_spiders2, which = 5)  # Half-normal plot of residuals
plot(beta_out_spiders2, which = 6)  # Fitted vs. observed values

Anova(beta_out_spiders2)

# Test for interactions between significant factors
beta_out_spiders3 <- betareg(Proportion ~ Year * Farmtype * Stage + `Forest_cover_%` + Rel_abd, 
                         data = rice_herb_consmp_spiders2)

summary(beta_out_spiders3)
lrtest(beta_out_spiders3)
Anova(beta_out_spiders3)

# Post-hoc tests for significant factors
multcomp_year_spiders <- emmeans(beta_out_spiders3, ~Year) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_spiders <- emmeans(beta_out_spiders3, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_spiders <- emmeans(beta_out_spiders3, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")


### Beta regression models for ladybeetles
# Initial model specification
beta_out_ladybeetles1 <- betareg(Proportion ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                             data = rice_herb_consmp_ladybeetles)
summary(beta_out_ladybeetles1)
lrtest(beta_out_ladybeetles1)  # Test for global significance of the model

plot(fitted(beta_out_ladybeetles1), residuals(beta_out_ladybeetles1))  # Fitted vs. residuals plot
plot(beta_out_ladybeetles1, which = 1)  # Residual vs. indices of observations
plot(beta_out_ladybeetles1, which = 2)  # Cook's distance plot
plot(beta_out_ladybeetles1, which = 3)  # Leverage plot
plot(beta_out_ladybeetles1, which = 5)  # Half-normal plot of residuals
plot(beta_out_ladybeetles1, which = 6)  # Fitted vs. observed values

Anova(beta_out_ladybeetles1)

# Test for interactions between significant factors
beta_out_ladybeetles2 <- betareg(Proportion ~ Year * Farmtype * Stage + `Forest_cover_%` + Rel_abd, 
                                 data = rice_herb_consmp_ladybeetles)

summary(beta_out_ladybeetles2)
lrtest(beta_out_ladybeetles2)
Anova(beta_out_ladybeetles2)

# Post-hoc tests for significant factors
multcomp_year_ladybeetles <- emmeans(beta_out_ladybeetles2, ~Year) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_ladybeetles <- emmeans(beta_out_ladybeetles2, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_ladybeetles <- emmeans(beta_out_ladybeetles2, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")





