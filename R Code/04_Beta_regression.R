## -----------------------------------------------------------------------------
## Title: Relationships between rice herbivore consumption by predators and 
##        various abiotic/biotic factors
##
## Author: Gen-Chang Hsu
##
## Date: 2023-04-29
##
## Description: 
## 1. Prepare the abiotic and biotic factor data for beta regression models
## 2. Fit beta regression models to examine the relationships between the 
##    mean proportion of rice herbivores consumed in predators' diet and various 
##    abiotic (farm type, crop stage, year, surrounding forest cover) and 
##    biotic factors (relative abundance of rice herbivores)
## 3. Fit beta regression models to examine the relationships between the 
##    median proportion of rice herbivores consumed in predators' diet and various 
##    abiotic (farm type, crop stage, year, surrounding forest cover) and 
##    biotic factors (relative abundance of rice herbivores)
## 4. Fit weighted beta regression models to examine the relationships between the 
##    median proportion of rice herbivores consumed in predators' diet and various 
##    abiotic (farm type, crop stage, year, surrounding forest cover) and 
##    biotic factors (relative abundance of rice herbivores)
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readxl)
library(betareg)
library(lmtest)
library(emmeans)
library(car)
library(parameters)


# Import files -----------------------------------------------------------------
model_out_clean <- readRDS("Output/Data_clean/model_out_clean.rds")
forest_cover <- read_csv("Data_raw/forest_cover.csv")
arthropod_abd_2017_raw <- read_xlsx("Data_raw/arthropod_abd_2017.xlsx", sheet = 1)
arthropod_abd_2018_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
arthropod_abd_2019_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)  


############################### Code starts here ###############################

# 1. Prepare the abiotic and biotic factor data for beta regression models -----
### Arthropod abundance data
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

### Combine dietary proportion, arthropod abundance, and forest cover data
rice_herb_consmp_all <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, 
         Rel_abd, Source, Proportion_mean = Mean, Proportion_median = `50%`) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "All") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>% 
  drop_na()
  
rice_herb_consmp_spiders <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, 
         Rel_abd, Source, Proportion_mean = Mean, Proportion_median = `50%`) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "Spider") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>% 
  drop_na()

rice_herb_consmp_ladybeetles <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, 
         Rel_abd, Source, Proportion_mean = Mean, Proportion_median = `50%`) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "Ladybeetle") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>% 
  drop_na()


# 2. Fit beta regression models using mean proportions -------------------------
### All predators
beta_out_all_mean <- betareg(Proportion_mean ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                             data = rice_herb_consmp_all)  # fit the model without interactions

lrtest(beta_out_all_mean)  # the model is globally significant

plot(fitted(beta_out_all_mean), residuals(beta_out_all_mean))  # fitted vs. residuals plot
plot(beta_out_all_mean, which = 1)  # residual vs. indices of observations
plot(beta_out_all_mean, which = 2)  # Cook's distance plot
plot(beta_out_all_mean, which = 3)  # leverage plot
plot(beta_out_all_mean, which = 5)  # half-normal plot of residuals
plot(beta_out_all_mean, which = 6)  # fitted vs. observed values

large_res_obs_all <- residuals(beta_out_all_mean) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # three observations with large residuals

# refit the model without three outliers
beta_out_all_mean_wo_outliers <- update(beta_out_all_mean, data = rice_herb_consmp_all[-large_res_obs_all, ])

lrtest(beta_out_all_mean_wo_outliers)  # the model is globally significant

plot(fitted(beta_out_all_mean_wo_outliers), residuals(beta_out_all_mean_wo_outliers))  # fitted vs. residuals plot
plot(beta_out_all_mean_wo_outliers, which = 1)  # residual vs. indices of observations
plot(beta_out_all_mean_wo_outliers, which = 2)  # Cook's distance plot
plot(beta_out_all_mean_wo_outliers, which = 3)  # leverage plot
plot(beta_out_all_mean_wo_outliers, which = 5)  # half-normal plot of residuals
plot(beta_out_all_mean_wo_outliers, which = 6)  # fitted vs. observed values

AIC(beta_out_all_mean, beta_out_all_mean_wo_outliers)  # the model without outliers is preferred

Anova(beta_out_all_mean_wo_outliers)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_all_mean <- emmeans(beta_out_all_mean_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_all_mean <- emmeans(beta_out_all_mean_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Spiders
beta_out_spiders_mean <- betareg(Proportion_mean ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                 data = rice_herb_consmp_spiders)  # fit the model without interactions

lrtest(beta_out_spiders_mean)  # the model is globally significant

plot(fitted(beta_out_spiders_mean), residuals(beta_out_spiders_mean))  # fitted vs. residuals plot
plot(beta_out_spiders_mean, which = 1)  # residual vs. indices of observations
plot(beta_out_spiders_mean, which = 2)  # Cook's distance plot
plot(beta_out_spiders_mean, which = 3)  # leverage plot
plot(beta_out_spiders_mean, which = 5)  # half-normal plot of residuals
plot(beta_out_spiders_mean, which = 6)  # fitted vs. observed values

large_res_obs_spiders <- residuals(beta_out_spiders_mean) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # three observations with large residuals

# refit the model without three outliers
beta_out_spiders_mean_wo_outliers <- update(beta_out_spiders_mean, data = rice_herb_consmp_spiders[-large_res_obs_spiders, ])

lrtest(beta_out_spiders_mean_wo_outliers)  # the model is globally significant

plot(fitted(beta_out_spiders_mean_wo_outliers), residuals(beta_out_spiders_mean_wo_outliers))  # fitted vs. residuals plot
plot(beta_out_spiders_mean_wo_outliers, which = 1)  # residual vs. indices of observations
plot(beta_out_spiders_mean_wo_outliers, which = 2)  # Cook's distance plot
plot(beta_out_spiders_mean_wo_outliers, which = 3)  # leverage plot
plot(beta_out_spiders_mean_wo_outliers, which = 5)  # half-normal plot of residuals
plot(beta_out_spiders_mean_wo_outliers, which = 6)  # fitted vs. observed values

AIC(beta_out_spiders_mean, beta_out_spiders_mean_wo_outliers)  # the model without outliers is preferred

Anova(beta_out_spiders_mean_wo_outliers)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_spiders_mean <- emmeans(beta_out_spiders_mean_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_spiders_mean <- emmeans(beta_out_spiders_mean_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Ladybeetles
beta_out_ladybeetles_mean <- betareg(Proportion_mean ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                 data = rice_herb_consmp_ladybeetles)  # fit the model without interactions

lrtest(beta_out_ladybeetles_mean)  # the model is globally significant

plot(fitted(beta_out_ladybeetles_mean), residuals(beta_out_ladybeetles_mean))  # fitted vs. residuals plot
plot(beta_out_ladybeetles_mean, which = 1)  # residual vs. indices of observations
plot(beta_out_ladybeetles_mean, which = 2)  # Cook's distance plot
plot(beta_out_ladybeetles_mean, which = 3)  # leverage plot
plot(beta_out_ladybeetles_mean, which = 5)  # half-normal plot of residuals
plot(beta_out_ladybeetles_mean, which = 6)  # fitted vs. observed values

# no obvious outliers
Anova(beta_out_ladybeetles_mean)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_ladybeetles_mean <- emmeans(beta_out_ladybeetles_mean_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_ladybeetles_mean <- emmeans(beta_out_ladybeetles_mean_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")


# 3. Fit beta regression models using median proportions -----------------------
### All predators
beta_out_all_median <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                               data = rice_herb_consmp_all)  # fit the model without interactions

lrtest(beta_out_all_median)  # the model is globally significant

plot(fitted(beta_out_all_median), residuals(beta_out_all_median))  # fitted vs. residuals plot
plot(beta_out_all_median, which = 1)  # residual vs. indices of observations
plot(beta_out_all_median, which = 2)  # Cook's distance plot
plot(beta_out_all_median, which = 3)  # leverage plot
plot(beta_out_all_median, which = 5)  # half-normal plot of residuals
plot(beta_out_all_median, which = 6)  # fitted vs. observed values

large_res_obs_all <- residuals(beta_out_all_median) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # three observations with large residuals

# refit the model without three outliers
beta_out_all_median_wo_outliers <- update(beta_out_all_median, data = rice_herb_consmp_all[-large_res_obs_all, ])

lrtest(beta_out_all_median_wo_outliers)  # the model is globally significant

plot(fitted(beta_out_all_median_wo_outliers), residuals(beta_out_all_median_wo_outliers))  # fitted vs. residuals plot
plot(beta_out_all_median_wo_outliers, which = 1)  # residual vs. indices of observations
plot(beta_out_all_median_wo_outliers, which = 2)  # Cook's distance plot
plot(beta_out_all_median_wo_outliers, which = 3)  # leverage plot
plot(beta_out_all_median_wo_outliers, which = 5)  # half-normal plot of residuals
plot(beta_out_all_median_wo_outliers, which = 6)  # fitted vs. observed values

AIC(beta_out_all_median, beta_out_all_median_wo_outliers)  # the model without outliers is preferred

Anova(beta_out_all_median_wo_outliers)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_all_median <- emmeans(beta_out_all_median_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_all_median <- emmeans(beta_out_all_median_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Spiders
beta_out_spiders_median <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                   data = rice_herb_consmp_spiders)  # fit the model without interactions

lrtest(beta_out_spiders_median)  # the model is globally significant

plot(fitted(beta_out_spiders_median), residuals(beta_out_spiders_median))  # fitted vs. residuals plot
plot(beta_out_spiders_median, which = 1)  # residual vs. indices of observations
plot(beta_out_spiders_median, which = 2)  # Cook's distance plot
plot(beta_out_spiders_median, which = 3)  # leverage plot
plot(beta_out_spiders_median, which = 5)  # half-normal plot of residuals
plot(beta_out_spiders_median, which = 6)  # fitted vs. observed values

large_res_obs_spiders <- residuals(beta_out_spiders_median) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # three observations with large residuals

# refit the model without three outliers
beta_out_spiders_median_wo_outliers <- update(beta_out_spiders_median, data = rice_herb_consmp_spiders[-large_res_obs_spiders, ])

lrtest(beta_out_spiders_median_wo_outliers)  # the model is globally significant

plot(fitted(beta_out_spiders_median_wo_outliers), residuals(beta_out_spiders_median_wo_outliers))  # fitted vs. residuals plot
plot(beta_out_spiders_median_wo_outliers, which = 1)  # residual vs. indices of observations
plot(beta_out_spiders_median_wo_outliers, which = 2)  # Cook's distance plot
plot(beta_out_spiders_median_wo_outliers, which = 3)  # leverage plot
plot(beta_out_spiders_median_wo_outliers, which = 5)  # half-normal plot of residuals
plot(beta_out_spiders_median_wo_outliers, which = 6)  # fitted vs. observed values

AIC(beta_out_spiders_median, beta_out_spiders_median_wo_outliers)  # the model without outliers is preferred

Anova(beta_out_spiders_median_wo_outliers)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_spiders_median <- emmeans(beta_out_spiders_median_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_spiders_median <- emmeans(beta_out_spiders_median_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Ladybeetles
beta_out_ladybeetles_median <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                       data = rice_herb_consmp_ladybeetles)  # fit the model without interactions

lrtest(beta_out_ladybeetles_median)  # the model is globally significant

plot(fitted(beta_out_ladybeetles_median), residuals(beta_out_ladybeetles_median))  # fitted vs. residuals plot
plot(beta_out_ladybeetles_median, which = 1)  # residual vs. indices of observations
plot(beta_out_ladybeetles_median, which = 2)  # Cook's distance plot
plot(beta_out_ladybeetles_median, which = 3)  # leverage plot
plot(beta_out_ladybeetles_median, which = 5)  # half-normal plot of residuals
plot(beta_out_ladybeetles_median, which = 6)  # fitted vs. observed values

# no obvious outliers
Anova(beta_out_ladybeetles_median)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_ladybeetles_median <- emmeans(beta_out_ladybeetles_median_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_ladybeetles_median <- emmeans(beta_out_ladybeetles_median_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")


# 4. Fit weighted beta regression models using median proportions --------------
### All predators
weights_all <- rice_herb_consmp_all %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  select(Year, weights)  # compute the weights

weights_vector_all <- 
  rice_herb_consmp_all %>% 
  left_join(weights_all, by = join_by("Year")) %>% 
  pull(weights)  # weight vector

# fit the model with weights
beta_out_all_median_weighted <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                        data = rice_herb_consmp_all, weights = weights_vector_all)

lrtest(beta_out_all_median_weighted)  # the model is globally significant

plot(fitted(beta_out_all_median_weighted), residuals(beta_out_all_median_weighted))  # fitted vs. residuals plot
plot(beta_out_all_median_weighted, which = 1)  # residual vs. indices of observations
plot(beta_out_all_median_weighted, which = 2)  # Cook's distance plot
plot(beta_out_all_median_weighted, which = 3)  # leverage plot
plot(beta_out_all_median_weighted, which = 5)  # half-normal plot of residuals
plot(beta_out_all_median_weighted, which = 6)  # fitted vs. observed values

large_res_obs_all <- residuals(beta_out_all_median_weighted) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # three observations with large residuals

# refit the model without three outliers
beta_out_all_median_weighted_wo_outliers <- update(beta_out_all_median_weighted,
                                                   data = rice_herb_consmp_all[-large_res_obs_all, ],
                                                   weights = weights_vector_all[-large_res_obs_all])

lrtest(beta_out_all_median_weighted_wo_outliers)  # the model is globally significant

plot(fitted(beta_out_all_median_weighted_wo_outliers), residuals(beta_out_all_median_weighted_wo_outliers))  # fitted vs. residuals plot
plot(beta_out_all_median_weighted_wo_outliers, which = 1)  # residual vs. indices of observations
plot(beta_out_all_median_weighted_wo_outliers, which = 2)  # Cook's distance plot
plot(beta_out_all_median_weighted_wo_outliers, which = 3)  # leverage plot
plot(beta_out_all_median_weighted_wo_outliers, which = 5)  # half-normal plot of residuals
plot(beta_out_all_median_weighted_wo_outliers, which = 6)  # fitted vs. observed values

AIC(beta_out_all_median_weighted, beta_out_all_median_weighted_wo_outliers)  # the model without outliers is preferred

Anova(beta_out_all_median_weighted_wo_outliers)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_all_median_weighted <- emmeans(beta_out_all_median_weighted_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_all_median_weighted <- emmeans(beta_out_all_median_weighted_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

# bias-corrected and accelerated bootstrap 95% CI based on 1000 bootstrap samples
boot_CI_all <- model_parameters(beta_out_all_median_weighted_wo_outliers, bootstrap = TRUE, ci_method = "bcai")

### Spiders
weights_spiders <- rice_herb_consmp_spiders %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  select(Year, weights)  # compute the weights

weights_vector_spiders <- 
  rice_herb_consmp_spiders %>% 
  left_join(weights_spiders, by = join_by("Year")) %>% 
  pull(weights)  # weight vector

# fit the model with weights
beta_out_spiders_median_weighted <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                            data = rice_herb_consmp_spiders, weights = weights_vector_spiders)

lrtest(beta_out_spiders_median_weighted)  # the model is globally significant

plot(fitted(beta_out_spiders_median_weighted), residuals(beta_out_spiders_median_weighted))  # fitted vs. residuals plot
plot(beta_out_spiders_median_weighted, which = 1)  # residual vs. indices of observations
plot(beta_out_spiders_median_weighted, which = 2)  # Cook's distance plot
plot(beta_out_spiders_median_weighted, which = 3)  # leverage plot
plot(beta_out_spiders_median_weighted, which = 5)  # half-normal plot of residuals
plot(beta_out_spiders_median_weighted, which = 6)  # fitted vs. observed values

large_res_obs_spiders <- residuals(beta_out_spiders_median_weighted) %>% 
  abs() %>% 
  sort(decreasing = T) %>%
  names() %>%
  as.numeric() %>%
  .[1:3]  # three observations with large residuals

# refit the model without three outliers
beta_out_spiders_median_weighted_wo_outliers <- update(beta_out_spiders_median_weighted,
                                                   data = rice_herb_consmp_spiders[-large_res_obs_spiders, ],
                                                   weights = weights_vector_spiders[-large_res_obs_spiders])

lrtest(beta_out_spiders_median_weighted_wo_outliers)  # the model is globally significant

plot(fitted(beta_out_spiders_median_weighted_wo_outliers), residuals(beta_out_spiders_median_weighted_wo_outliers))  # fitted vs. residuals plot
plot(beta_out_spiders_median_weighted_wo_outliers, which = 1)  # residual vs. indices of observations
plot(beta_out_spiders_median_weighted_wo_outliers, which = 2)  # Cook's distance plot
plot(beta_out_spiders_median_weighted_wo_outliers, which = 3)  # leverage plot
plot(beta_out_spiders_median_weighted_wo_outliers, which = 5)  # half-normal plot of residuals
plot(beta_out_spiders_median_weighted_wo_outliers, which = 6)  # fitted vs. observed values

AIC(beta_out_spiders_median_weighted, beta_out_spiders_median_weighted_wo_outliers)  # the model without outliers is preferred

Anova(beta_out_spiders_median_weighted_wo_outliers)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_spiders_median_weighted <- emmeans(beta_out_spiders_median_weighted_wo_outliers, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_spiders_median_weighted <- emmeans(beta_out_spiders_median_weighted_wo_outliers, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

# bias-corrected and accelerated bootstrap 95% CI based on 1000 bootstrap samples
boot_CI_spiders <- model_parameters(beta_out_spiders_median_weighted_wo_outliers, bootstrap = TRUE, ci_method = "bcai")

### Ladybeetles
weights_ladybeetles <- rice_herb_consmp_ladybeetles %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  select(Year, weights)  # compute the weights

weights_vector_ladybeetles <- 
  rice_herb_consmp_ladybeetles %>% 
  left_join(weights_ladybeetles, by = join_by("Year")) %>% 
  pull(weights)  # weight vector

# fit the model with weights
beta_out_ladybeetles_median_weighted <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                                data = rice_herb_consmp_ladybeetles, weights = weights_vector_ladybeetles)

lrtest(beta_out_ladybeetles_median_weighted)  # the model is globally significant

plot(fitted(beta_out_ladybeetles_median_weighted), residuals(beta_out_ladybeetles_median_weighted))  # fitted vs. residuals plot
plot(beta_out_ladybeetles_median_weighted, which = 1)  # residual vs. indices of observations
plot(beta_out_ladybeetles_median_weighted, which = 2)  # Cook's distance plot
plot(beta_out_ladybeetles_median_weighted, which = 3)  # leverage plot
plot(beta_out_ladybeetles_median_weighted, which = 5)  # half-normal plot of residuals
plot(beta_out_ladybeetles_median_weighted, which = 6)  # fitted vs. observed values

# no obvious outliers
Anova(beta_out_ladybeetles_median_weighted)  # test for individual factor significance

# post-hoc tests for the significant factors
multcomp_farmtype_ladybeetles_median_weighted <- emmeans(beta_out_ladybeetles_median_weighted, ~Farmtype) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_ladybeetles_median_weighted <- emmeans(beta_out_ladybeetles_median_weighted, ~Stage) %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

# bias-corrected and accelerated bootstrap 95% CI based on 1000 bootstrap samples
boot_CI_ladybeetles <- model_parameters(beta_out_ladybeetles_median_weighted, bootstrap = TRUE, ci_method = "bcai")













