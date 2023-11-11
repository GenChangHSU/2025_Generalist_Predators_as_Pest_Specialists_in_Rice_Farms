library(tidyverse)
library(glmmTMB)
library(betareg)
library(lmtest)
library(emmeans)
library(car)
library(parameters)


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
beta_out_all_median_weighted <- glmmTMB(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd + (1|Farm_ID), 
                 data = rice_herb_consmp_all, 
                 weights = weights_vector_all,
                 family = beta_family(link = "logit"))

beta_out_all_median_weighted <- betareg(Proportion_median ~ Year + Farmtype + Stage + `Forest_cover_%` + Rel_abd, 
                                        data = rice_herb_consmp_all, weights = weights_vector_all)

lrtest(beta_out_all_median_weighted)  # the model is globally significant
lrtest(model)


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
Anova(model)

# post-hoc tests for the significant factors
multcomp_farmtype_all_median_weighted <- emmeans(beta_out_all_median_weighted_wo_outliers, ~Farmtype, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")
multcomp_stage_all_median_weighted <- emmeans(beta_out_all_median_weighted_wo_outliers, ~Stage, type = "response") %>%
  multcomp::cld(alpha = 0.05, Letters = letters, adjusj = "tukey")

# bias-corrected and accelerated bootstrap 95% CI based on 1000 bootstrap samples
boot_CI_all <- model_parameters(beta_out_all_median_weighted_wo_outliers, bootstrap = TRUE, ci_method = "bcai")
