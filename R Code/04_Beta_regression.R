## -----------------------------------------------------------------------------
## Title: Analyze the relationships between rice herbivore consumption by predators 
##        and various abiotic and biotic factors
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-17
##
## Description:
## 1. Prepare the abiotic and biotic factor data for the beta regression models
## 2. Fit weighted beta GLMMs and perform post-hoc tests for both predators
## 3. Fit weighted beta GLMMs and perform post-hoc tests for spiders
## 4. Fit weighted beta GLMMs and perform post-hoc tests for ladybeetles
## 5. Summarize model outputs
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(glmmTMB)
library(lmtest)
library(emmeans)
library(car)
library(parameters)
library(performance)


# Import files -----------------------------------------------------------------
model_out_clean <- readRDS("Output/Data_clean/model_out_clean.rds")
forest_cover <- read_csv("Data_raw/forest_cover.csv")
arthropod_abd_2017_raw <- read_xlsx("Data_raw/arthropod_abd_2017.xlsx", sheet = 1)
arthropod_abd_2018_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
arthropod_abd_2019_raw <- read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)  


############################### Code starts here ###############################

# 1. Prepare the abiotic and biotic factor data for beta regression models -----
### Arthropod abundance data
predator <- c("ARA", "TET", "COC")
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
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, Abundance, 
         Rel_abd, Source, Proportion_mean = Mean, Proportion_median = `50%`) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "All") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>% 
  drop_na() %>% 
  mutate(Pair_ID = str_remove(Farm_ID, pattern = "O|C"))

rice_herb_consmp_spiders <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, Abundance,
         Rel_abd, Source, Proportion_mean = Mean, Proportion_median = `50%`) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "Spider") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>% 
  drop_na() %>% 
  mutate(Pair_ID = str_remove(Farm_ID, pattern = "O|C"))

rice_herb_consmp_ladybeetles <- model_out_clean %>%
  left_join(arthropod_abd_clean, by = c("Year", "Farm_ID", "Stage", "Source")) %>%
  left_join(forest_cover, by = "Farm_ID") %>%
  select(Predator, Year, Farm_ID, Farmtype, Stage, `Forest_cover_%`, Abundance,
         Rel_abd, Source, Proportion_mean = Mean, Proportion_median = `50%`) %>%
  arrange(Predator, Year, Farm_ID) %>%
  filter(Source == "Rice_herb" & Predator == "Ladybeetle") %>%
  mutate(Farmtype = factor(Farmtype, levels = c("Or", "Cv")),
         Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>% 
  drop_na() %>% 
  mutate(Pair_ID = str_remove(Farm_ID, pattern = "O|C"))


# 2. Fit weighted beta GLMMs for both predators --------------------------------
### Get the weight vector
weights_all <- rice_herb_consmp_all %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  select(Year, weights) 

weights_vector_all <- 
  rice_herb_consmp_all %>% 
  left_join(weights_all, by = join_by("Year")) %>% 
  pull(weights)

### Fit the GLMM model with farm ID nested within pair ID as random effects
beta_out_all_median_weighted <- glmmTMB(Proportion_median ~ Year + Farmtype + 
                                          Stage + `Forest_cover_%` + Rel_abd + 
                                          (1|Pair_ID/Farm_ID),
                                        data = rice_herb_consmp_all,
                                        weights = weights_vector_all,
                                        na.action = na.exclude,
                                        family = beta_family(link = "logit"))

### Test for global significance
lrtest(beta_out_all_median_weighted)

### Test for factor significance
summary(beta_out_all_median_weighted)
Anova(beta_out_all_median_weighted)

### Model diagnostics
check_model(beta_out_all_median_weighted)
plot(fitted(beta_out_all_median_weighted), residuals(beta_out_all_median_weighted))  # fitted vs. residuals plot

### Post-hoc tests for the significant factors
multcomp_year_all_median_weighted <- emmeans(beta_out_all_median_weighted, ~Year, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_all_median_weighted <- emmeans(beta_out_all_median_weighted, ~Farmtype, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_all_median_weighted <- emmeans(beta_out_all_median_weighted, ~Stage, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Use absolute abundance as the predictor
beta_out_all_median_weighted_abd <- glmmTMB(Proportion_median ~ Year + Farmtype + 
                                          Stage + `Forest_cover_%` + Abundance + 
                                          (1|Pair_ID/Farm_ID),
                                        data = rice_herb_consmp_all,
                                        weights = weights_vector_all,
                                        na.action = na.exclude,
                                        family = beta_family(link = "logit"))

### Test for global significance
lrtest(beta_out_all_median_weighted_abd)

### Test for factor significance
summary(beta_out_all_median_weighted_abd)
Anova(beta_out_all_median_weighted_abd)


# 3. Fit weighted beta GLMMs for spiders ---------------------------------------
### Get the weight vector
weights_spiders <- rice_herb_consmp_spiders %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  select(Year, weights) 

weights_vector_spiders <- 
  rice_herb_consmp_spiders %>% 
  left_join(weights_spiders, by = join_by("Year")) %>% 
  pull(weights)

### Fit the GLMM model with farm ID nested within pair ID as random effects
beta_out_spiders_median_weighted <- glmmTMB(Proportion_median ~ Year + Farmtype + 
                                          Stage + `Forest_cover_%` + Rel_abd + 
                                          (1|Pair_ID/Farm_ID),
                                        data = rice_herb_consmp_spiders,
                                        weights = weights_vector_spiders,
                                        na.action = na.exclude,
                                        family = beta_family(link = "logit"))

### Test for global significance
lrtest(beta_out_spiders_median_weighted)

### Test for factor significance
summary(beta_out_spiders_median_weighted)
Anova(beta_out_spiders_median_weighted)

### Model diagnostics
check_model(beta_out_spiders_median_weighted)
plot(fitted(beta_out_spiders_median_weighted), residuals(beta_out_spiders_median_weighted))  # fitted vs. residuals plot

### Post-hoc tests for the significant factors
multcomp_year_spiders_median_weighted <- emmeans(beta_out_spiders_median_weighted, ~Year, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_spiders_median_weighted <- emmeans(beta_out_spiders_median_weighted, ~Farmtype, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_spiders_median_weighted <- emmeans(beta_out_spiders_median_weighted, ~Stage, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Use absolute abundance as the predictor
beta_out_spiders_median_weighted_abd <- glmmTMB(Proportion_median ~ Year + Farmtype + 
                                              Stage + `Forest_cover_%` + Abundance + 
                                              (1|Pair_ID/Farm_ID),
                                            data = rice_herb_consmp_spiders,
                                            weights = weights_vector_spiders,
                                            na.action = na.exclude,
                                            family = beta_family(link = "logit"))

### Test for global significance
lrtest(beta_out_spiders_median_weighted_abd)

### Test for factor significance
summary(beta_out_spiders_median_weighted_abd)
Anova(beta_out_spiders_median_weighted_abd)


# 4. Fit weighted beta GLMMs for ladybeetles -----------------------------------
### Get the weight vector
weights_ladybeetles <- rice_herb_consmp_ladybeetles %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  select(Year, weights) 

weights_vector_ladybeetles <- 
  rice_herb_consmp_ladybeetles %>% 
  left_join(weights_ladybeetles, by = join_by("Year")) %>% 
  pull(weights)

### Fit the GLMM model with farm ID nested within pair ID as random effects
beta_out_ladybeetles_median_weighted <- glmmTMB(Proportion_median ~ Year + Farmtype + 
                                              Stage + `Forest_cover_%` + Rel_abd + 
                                              (1|Pair_ID/Farm_ID),
                                            data = rice_herb_consmp_ladybeetles,
                                            weights = weights_vector_ladybeetles,
                                            na.action = na.exclude,
                                            family = beta_family(link = "logit"))

### Test for global significance
lrtest(beta_out_ladybeetles_median_weighted)

### Test for factor significance
summary(beta_out_ladybeetles_median_weighted)
Anova(beta_out_ladybeetles_median_weighted)

### Model diagnostics
check_model(beta_out_ladybeetles_median_weighted)
plot(fitted(beta_out_ladybeetles_median_weighted), residuals(beta_out_ladybeetles_median_weighted))  # fitted vs. residuals plot

### Post-hoc tests for the significant factors
multcomp_year_ladybeetles_median_weighted <- emmeans(beta_out_ladybeetles_median_weighted, ~Year, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_ladybeetles_median_weighted <- emmeans(beta_out_ladybeetles_median_weighted, ~Farmtype, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_ladybeetles_median_weighted <- emmeans(beta_out_ladybeetles_median_weighted, ~Stage, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

### Fit the GLMM model with farm ID nested within pair ID as random effects
beta_out_ladybeetles_median_weighted_abd <- glmmTMB(Proportion_median ~ Year + Farmtype + 
                                                  Stage + `Forest_cover_%` + Abundance + 
                                                  (1|Pair_ID/Farm_ID),
                                                data = rice_herb_consmp_ladybeetles,
                                                weights = weights_vector_ladybeetles,
                                                na.action = na.exclude,
                                                family = beta_family(link = "logit"))

### Test for global significance
lrtest(beta_out_ladybeetles_median_weighted_abd)

### Test for factor significance
summary(beta_out_ladybeetles_median_weighted_abd)
Anova(beta_out_ladybeetles_median_weighted_abd)


# 5. Summary of model results and post-hoc comparisons -------------------------
### LR test of the factor significance
Anova_out_all <- Anova(beta_out_all_median_weighted)
Anova_out_spiders <- Anova(beta_out_spiders_median_weighted)
Anova_out_ladybeetles <- Anova(beta_out_ladybeetles_median_weighted)

map(list(all = Anova_out_all, 
         spiders = Anova_out_spiders, 
         ladybeetles = Anova_out_ladybeetles), function(x){
           x %>% 
             as.data.frame() %>% 
             rownames_to_column(var = "Factor") %>% 
             mutate(Chisq = round(Chisq, 2),
                    `Pr(>Chisq)` = round(`Pr(>Chisq)`, 3))
         }) %>% 
  bind_rows(.id = "Predator") %>% 
  write_csv("Output/Data_clean/betareg_out.csv")

### Post-hoc test for farm type
list(all = multcomp_farmtype_all_median_weighted,
     spiders = multcomp_farmtype_spiders_median_weighted,
     ladybeetles = multcomp_farmtype_ladybeetles_median_weighted) %>% 
  bind_rows(.id = "Predator") %>% 
  mutate(emmean = round(response, 2),
         SE = round(SE, 2),
         asymp.LCL = round(asymp.LCL, 2),
         asymp.UCL = round(asymp.UCL, 2),
         .group = str_trim(.group)) %>% 
  mutate(mean_SE = paste0(emmean, .group, " (±", SE, ")")) %>% 
  select(Predator, Farmtype, mean_SE, asymp.LCL, asymp.UCL) %>% 
  write_csv("Output/Data_clean/Posthoc_farmtype.csv")

### Post-hoc test for crop stage
list(all = multcomp_stage_all_median_weighted,
     spiders = multcomp_stage_spiders_median_weighted,
     ladybeetles = multcomp_stage_ladybeetles_median_weighted) %>% 
  bind_rows(.id = "Predator") %>% 
  mutate(emmean = round(response, 2),
         SE = round(SE, 2),
         asymp.LCL = round(asymp.LCL, 2),
         asymp.UCL = round(asymp.UCL, 2),
         .group = str_trim(.group)) %>% 
  mutate(mean_SE = paste0(emmean, .group, " (±", SE, ")")) %>% 
  select(Predator, Stage, mean_SE, asymp.LCL, asymp.UCL) %>% 
  write_csv("Output/Data_clean/Posthoc_stage.csv")




