## -----------------------------------------------------------------------------
## Title: Create a plot of hypothetical pest consumption by predators
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-24
##
## Description:
## 1. Create a plot of hypothetical pest consumption by predators for figure 1d
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)


############################### Code starts here ###############################

# 1. A plot of hypothetical pest consumption by predators for figure 1d --------
fig1d_data <- tibble(farmtype = rep(c("Organic", "Conventional"), each = 3),
                     cropstage = rep(c("Tillering", "Flowering", "Ripening"), 2),
                     mean = c(0.2, 0.7, 0.90, 0.23, 0.83, 0.94),
                     se = c(0.07, 0.07, 0.07, 0.07, 0.07, 0.07)) %>% 
  mutate(farmtype = fct_relevel(farmtype, "Organic", "Conventional"),
         cropstage = fct_relevel(cropstage, "Tillering", "Flowering", "Ripening"))

ggplot(fig1d_data) + 
  geom_point(aes(x = cropstage, y = mean)) + 
  geom_errorbar(aes(x = cropstage, ymin = mean - se, ymax = mean + se), width = 0) + 
  geom_line(aes(x = cropstage, y = mean, group = 1)) + 
  geom_vline(xintercept = 0.4, linewidth = 1) + 
  facet_grid(. ~ farmtype) + 
  labs(x = "Crop stage", y = "Pest consumption") + 
  theme_classic(base_size = 9) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "black"),
        axis.line.y = element_blank(),
        panel.spacing = unit(0.5, "lines"),
        axis.title.x = element_text(margin = margin(t = 6)))

ggsave("./Output/Figures/Figure1d.tiff", width = 2.5, height = 1.5, dpi = 600, device = "tiff")








