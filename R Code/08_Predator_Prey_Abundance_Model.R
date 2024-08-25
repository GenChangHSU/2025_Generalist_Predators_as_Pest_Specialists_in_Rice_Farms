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


# Model summary and plot functions from the package "sjPlot" -------------------
model_summary <- function(model, transform_estimate) {
  tab_model(model,
            auto.label = T,
            show.est = T,
            show.se = T,
            show.ci = 0.95,
            show.stat = T,
            show.p = T,
            show.reflvl = F,
            col.order = c("est", "se", "ci", "stat", "p"),
            transform = transform_estimate,
            show.zeroinf = T,
            string.ci = "95% CI",
            string.se = "SE",
            string.stat = "Test statistic",
            string.p = "P")
}

model_forest_plot <- function(model, transform_estimate) {
  plot_model(model, 
             sort.est = T,
             transform = transform_estimate,
             show.values = T,
             show.p = T,
             value.offset = 0.3,
             vline.color = "red")
}


# A function for extracting GL(M)M results -------------------------------------
extract_model_results_glmm <- function(models){
  
  model_dfs <- map(models, function(x){
    x %>% 
      Anova() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Predictor") %>% 
      rename(P = `Pr(>Chisq)`) %>% 
      mutate(n = x$modelInfo$nobs, .before = "Predictor") %>% 
      mutate(Chisq = if_else(Chisq > 0.05, round(Chisq, 1), round(Chisq, 2)),
             P = case_when(P > 0.01 ~ as.character(round(P, 2)),
                           P < 0.01 & P > 0.001 ~ as.character(round(P, 3)),
                           P < 0.001 ~ "< 0.001"))
  })
  
  model_results <- model_dfs %>% 
    bind_rows(.id = "Model_response")
  
  return(model_results)
}


# ggplot theme -----------------------------------------------------------------
my_ggtheme <- 
  theme(
    # axis
    axis.text.x = element_text(size = 14, color = "black", margin = margin(t = 3)),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 8)),
    axis.ticks.length.x = unit(0.18, "cm"),
    axis.ticks.length.y = unit(0.15, "cm"),
    
    # plot
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.background = element_rect(colour = "transparent"),
    
    # panel
    panel.background = element_rect(fill = "transparent"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # legend
    legend.position = "right",
    legend.spacing.x = unit(0.2, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.key.size = unit(0.5, "line"),
    legend.key = element_blank(),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.box.just = "center",
    legend.justification = c(0.5, 0.5),
    legend.title.align = 0.5,
    legend.background = element_rect(fill = "transparent", size = 0.25, linetype = "solid", colour = "black"),
    
    # facet strip
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = 13, hjust = 0.5)
  )


# Import files -----------------------------------------------------------------
forest_cover <- read_csv("Data_raw/forest_cover.csv")
arthropod_abd_2017_raw <- readxl::read_xlsx("Data_raw/arthropod_abd_2017.xlsx", sheet = 1)
arthropod_abd_2018_raw <- readxl::read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 1)
arthropod_abd_2019_raw <- readxl::read_xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheet = 2)


############################### Code starts here ###############################

# 1. Organize the predator and prey data ---------------------------------------
### Arthropod abundance data
predator <- c("ARA", "TET", "COC")
rice_herb <- c("DEL", "CIC", "PEN", "ALY", "LYG")

predator_prey_abd_2017_clean <- arthropod_abd_2017_raw %>%
  dplyr::select(Farm_ID = Farm, Stage, Family = Family.ID, Abundance = Count) %>%
  filter(Stage != "Seedling") %>%
  mutate(Family = toupper(Family)) %>%
  filter(Family %in% c(predator, rice_herb)) %>%
  mutate(Source = case_when(Family %in% rice_herb ~ "Rice_herb", 
                            Family %in% predator ~ "Predator")) %>%
  group_by(Farm_ID, Stage, Source) %>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate(Year = 2017)

predator_prey_abd_2018_clean <- arthropod_abd_2018_raw %>%
  dplyr::select(Date, Farm_ID = Farm, Family = Family.abbr, Abundance) %>%
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
  dplyr::select(Date, Farm_ID = Farm, Family = Family.abbr, Abundance) %>%
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
  dplyr::select(Year, Farm_ID, Stage, Source, Abundance) %>%
  mutate(Year = as.character(Year)) %>% 
  mutate(Farmtype = str_extract(Farm_ID, pattern = "O|C"),
         Farmtype = if_else(Farmtype == "O", "Organic", "Conventional")) %>% 
  pivot_wider(names_from = "Source", values_from = "Abundance") %>% 
  left_join(forest_cover, by = join_by("Farm_ID")) %>% 
  mutate(Predator = if_else(is.na(Predator), 0, Predator),
         Rice_herb = if_else(is.na(Rice_herb), 0, Rice_herb)) %>% 
  mutate(Pair_ID = str_remove(Farm_ID, pattern = "O|C")) %>% 
  ungroup()

### Weights
weights <- predator_prey_abd_clean %>% 
  count(Year) %>% 
  mutate(prop = n/sum(n),
         prop_n = n*prop,
         weights = prop*sum(n)/sum(prop_n)) %>% 
  dplyr::select(Year, weights) 

weights_vec <- 
  predator_prey_abd_clean %>% 
  left_join(weights, by = join_by("Year")) %>% 
  pull(weights)


# 2. Effects of farm type, crop stage, and year on predator abundance ----------
### (1) Test overdispersion
predator_abundance_poisson <- glmmTMB(Predator ~ Farmtype + Stage + Year + `Forest_cover_%` + (1|Pair_ID/Farm_ID),
                                      data = predator_prey_abd_clean,
                                      family = "poisson",
                                      weights = weights_vec,
                                      na.action = na.omit)

predator_abundance_nb <- glmmTMB(Predator ~ Farmtype + Stage + Year + `Forest_cover_%` + (1|Pair_ID/Farm_ID),
                                 data = predator_prey_abd_clean,
                                 family = "nbinom2",
                                 weights = weights_vec,
                                 na.action = na.omit)

lrtest(predator_abundance_poisson, predator_abundance_nb)
AIC(predator_abundance_poisson, predator_abundance_nb)

### (2) Model diagnostics
plot(simulateResiduals(predator_abundance_nb))

### (3) Model significance
predator_abundance_null <- glmmTMB(Predator ~ 1,
                                 data = predator_prey_abd_clean,
                                 family = "nbinom2",
                                 weights = weights_vec,
                                 na.action = na.omit)

lrtest(predator_abundance_null, predator_abundance_nb)

### (4) Model summary
summary(predator_abundance_nb)
model_summary(predator_abundance_nb, transform_estimate = "exp")
model_forest_plot(predator_abundance_nb, transform_estimate = "exp")
Anova(predator_abundance_nb, type = 2)

### (5) emmeans
multcomp_year_predator_abundance_nb <- emmeans(predator_abundance_nb, ~Year, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_predator_abundance_nb <- emmeans(predator_abundance_nb, ~Farmtype, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_predator_abundance_nb <- emmeans(predator_abundance_nb, ~Stage, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")


# 3. Effects of farm type, crop stage, and year on rice herbivore abundance ----
### (1) Test overdispersion
rice_herb_abundance_poisson <- glmmTMB(Rice_herb ~ Farmtype + Stage + Year + `Forest_cover_%` + (1|Pair_ID/Farm_ID),
                                      data = predator_prey_abd_clean,
                                      family = "poisson",
                                      weights = weights_vec,
                                      na.action = na.omit)

rice_herb_abundance_nb <- glmmTMB(Rice_herb ~ Farmtype + Stage + Year + `Forest_cover_%` + (1|Pair_ID/Farm_ID),
                                 data = predator_prey_abd_clean,
                                 family = "nbinom2",
                                 weights = weights_vec,
                                 na.action = na.omit)

lrtest(rice_herb_abundance_poisson, rice_herb_abundance_nb)
AIC(rice_herb_abundance_poisson, rice_herb_abundance_nb)

### (2) Model diagnostics
plot(simulateResiduals(rice_herb_abundance_nb))

### (3) Model significance
rice_herb_abundance_null <- glmmTMB(Rice_herb ~ 1,
                                   data = predator_prey_abd_clean,
                                   family = "nbinom2",
                                   weights = weights_vec,
                                   na.action = na.omit)

lrtest(rice_herb_abundance_null, rice_herb_abundance_nb)

### (4) Model summary
summary(rice_herb_abundance_nb)
model_summary(rice_herb_abundance_nb, transform_estimate = "exp")
model_forest_plot(rice_herb_abundance_nb, transform_estimate = "exp")
Anova(rice_herb_abundance_nb, type = 2)

### (5) emmeans
multcomp_year_rice_herb_abundance_nb <- emmeans(rice_herb_abundance_nb, ~Year, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_farmtype_rice_herb_abundance_nb <- emmeans(rice_herb_abundance_nb, ~Farmtype, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_rice_herb_abundance_nb <- emmeans(rice_herb_abundance_nb, ~Stage, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")


