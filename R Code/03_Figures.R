## -----------------------------------------------------------------------------
## Title: Visualization of Predators' Diet Compositions
##
## Author: Gen-Chang Hsu
##
## Date: 2023-04-28
##
## Description: 
## 1. Create line charts of dietary proportions both predators, spiders, and ladybeetles.
## 2. Create line charts of rice herbivore consumption by both predators, spiders, and ladybeetles.
## 3. Create line charts of relative abundances of prey sources over crop stages.
## 4. Create line charts of mean number of predators over crop stages.
## 5. Create density plots of the posterior draws in the predator model.
## 6. Create stable isotope biplot of rice plant and prey sources.
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(xlsx)
library(readxl)
library(ggpubr)


# ggplot theme -----------------------------------------------------------------
my_theme <- 
  theme(# Axis
        axis.text.x = element_text(size = 12, color = "black", margin = margin(t = 3)),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.title.y = element_text(size = 15, margin = margin(r = 8)),
        axis.ticks.length.x = unit(0.2, "cm"),
        
        # Plot
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
        plot.background = element_rect(colour = "transparent"),
        
        # Panel
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        # Legend
        legend.position = c(1, 1),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.key.size = unit(1.2, "line"),
        legend.key = element_blank(),
        legend.text = element_text(size = 10, margin = margin(0, 10, 0, -5)),
        legend.text.align = 0,
        legend.box.just = "center",
        legend.justification = c(0.5, 0.5),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "transparent"),
        
        # Facet strip
        strip.background = element_rect(fill = "transparent"),
        strip.text = element_text(size = 12, hjust = 0.5)
        )


# Import files -----------------------------------------------------------------
SID_all_clean <- readRDS("Output/Data_clean/SID_all_clean.rds")
model_out_clean <- readRDS("Output/Data_clean/model_out_clean.rds")
Posterior_draws_predator <- readRDS("Output/Data_clean/Posterior_draws_predator.rds")
Abd_2017 <- read.xlsx("Data_raw/arthropod_abd_2017.xlsx", sheetIndex = 1)
Abd_2018 <- read.xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheetIndex = 1)
Abd_2019 <- read.xlsx("Data_raw/arthropod_abd_2018_2019.xlsx", sheetIndex = 2)
Rice_data <- read_csv("./Data_raw/SID_2017.csv") %>% 
  filter(Species == "Os")


############################### Code starts here ###############################

# 1. Line charts of dietary proportions of predators ---------------------------
### Create a data frame for panel labels
label1 <- data.frame(Predator = c("All", "Spider", "Ladybeetle", "All", "Spider", "Ladybeetle"),
                     Farmtype = c("Or", "Or", "Or", "Cv", "Cv", "Cv"),   
                     Source = rep("Rice_herb", 6), 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(1.12, 1.12, 1.12, 1, 1, 1),
                     Label = c("(a)", "(b)", "(c)", "", "", "")) %>%
  mutate(Predator = factor(Predator, levels = unique(Predator), ordered = T),
         Farmtype = factor(Farmtype, levels = unique(Farmtype), ordered = T))

### Three years pooled
model_out_clean %>% 
  group_by(Predator, Farmtype, Stage, Source) %>%
  summarise(Proportion = mean(`50%`, na.rm = T),
            n = n(),
            SD = sd(`50%`),
            SE = SD/sqrt(n)) %>%
  ggplot(aes(x = Stage, y = Proportion, color = Source, shape = Source, group = Source)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) + 
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                position = position_dodge(0.1), 
                width = 0.3) +
  facet_grid(Predator~Farmtype, labeller = as_labeller(c("Or" = "Organic", 
                                                         "Cv" = "Conventional",
                                                         "All" = "Both predators",
                                                         "Spider" = "Spiders",
                                                         "Ladybeetle" = "Ladybeetles"))) + 
  geom_text(data = label1, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Proportion of prey sources in the diet (mean ± SE)") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values = c(16, 15, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"))
 
ggsave("Output/Figures/Diet_proportion.tiff", width = 6, height = 7, dpi = 600)

### 2017
label1 <- data.frame(Predator = c("All", "Spider", "Ladybeetle", "All", "Spider", "Ladybeetle"),
                     Farmtype = c("Or", "Or", "Or", "Cv", "Cv", "Cv"),   
                     Source = rep("Rice_herb", 6), 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(1.12, 1.12, 1.12, 1, 1, 1),
                     Label = c("(a)", "(b)", "(c)", "", "", "")) %>%
  mutate(Predator = factor(Predator, levels = unique(Predator), ordered = T),
         Farmtype = factor(Farmtype, levels = unique(Farmtype), ordered = T))

model_out_clean %>% 
  filter(Year == 2017) %>%
  group_by(Predator, Farmtype, Stage, Source) %>%
  summarise(Proportion = mean(`50%`, na.rm = T),
            n = n(),
            SD = sd(`50%`),
            SE = SD/sqrt(n)) %>%
  ggplot(aes(x = Stage, y = Proportion, color = Source, shape = Source, group = Source)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) + 
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                position = position_dodge(0.1), 
                width = 0.3) +
  facet_grid(Predator~Farmtype, labeller = as_labeller(c("Or" = "Organic", 
                                                         "Cv" = "Conventional",
                                                         "All" = "Both predators",
                                                         "Spider" = "Spiders",
                                                         "Ladybeetle" = "Ladybeetles"))) + 
  geom_text(data = label1, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Proportion of prey sources in the diet (mean ± SE)") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values = c(16, 15, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"),
        legend.margin = margin(b = -5),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5))) + 
  labs(title = "2017")

ggsave("Output/Figures/Diet_proportion_2017.tiff", width = 6, height = 7, dpi = 600)

### 2018
label2 <- data.frame(Predator = c("All", "Spider", "Ladybeetle", "All", "Spider", "Ladybeetle"),
                     Farmtype = c("Or", "Or", "Or", "Cv", "Cv", "Cv"),   
                     Source = rep("Rice_herb", 6), 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(1.12, 1.12, 1.12, 1, 1, 1),
                     Label = c("(d)", "(e)", "(f)", "", "", "")) %>%
  mutate(Predator = factor(Predator, levels = unique(Predator), ordered = T),
         Farmtype = factor(Farmtype, levels = unique(Farmtype), ordered = T))

model_out_clean %>% 
  filter(Year == 2018) %>%
  group_by(Predator, Farmtype, Stage, Source) %>%
  summarise(Proportion = mean(`50%`, na.rm = T),
            n = n(),
            SD = sd(`50%`),
            SE = SD/sqrt(n)) %>%
  ggplot(aes(x = Stage, y = Proportion, color = Source, shape = Source, group = Source)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) + 
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                position = position_dodge(0.1), 
                width = 0.3) +
  facet_grid(Predator~Farmtype, labeller = as_labeller(c("Or" = "Organic", 
                                                         "Cv" = "Conventional",
                                                         "All" = "Both predators",
                                                         "Spider" = "Spiders",
                                                         "Ladybeetle" = "Ladybeetles"))) + 
  geom_text(data = label2, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Proportion of prey sources in the diet (mean ± SE)") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values = c(16, 15, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"),
        legend.margin = margin(b = -5),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5))) + 
  labs(title = "2018")

ggsave("Output/Figures/Diet_proportion_2018.tiff", width = 6, height = 7, dpi = 600)

### 2019
label3 <- data.frame(Predator = c("All", "Spider", "Ladybeetle", "All", "Spider", "Ladybeetle"),
                     Farmtype = c("Or", "Or", "Or", "Cv", "Cv", "Cv"),   
                     Source = rep("Rice_herb", 6), 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(1.12, 1.12, 1.12, 1, 1, 1),
                     Label = c("(g)", "(h)", "(i)", "", "", "")) %>%
  mutate(Predator = factor(Predator, levels = unique(Predator), ordered = T),
         Farmtype = factor(Farmtype, levels = unique(Farmtype), ordered = T))

model_out_clean %>% 
  filter(Year == 2019) %>%
  group_by(Predator, Farmtype, Stage, Source) %>%
  summarise(Proportion = mean(`50%`, na.rm = T),
            n = n(),
            SD = sd(`50%`),
            SE = SD/sqrt(n)) %>%
  ggplot(aes(x = Stage, y = Proportion, color = Source, shape = Source, group = Source)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) + 
  geom_errorbar(aes(ymin = Proportion - SE, ymax = Proportion + SE), 
                position = position_dodge(0.1), 
                width = 0.3) +
  facet_grid(Predator~Farmtype, labeller = as_labeller(c("Or" = "Organic", 
                                                         "Cv" = "Conventional",
                                                         "All" = "Both predators",
                                                         "Spider" = "Spiders",
                                                         "Ladybeetle" = "Ladybeetles"))) + 
  geom_text(data = label3, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Proportion of prey sources in the diet (mean ± SE)") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values = c(16, 15, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"),
        legend.margin = margin(b = -5),
        plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 5))) + 
  labs(title = "2019")

ggsave("Output/Figures/Diet_proportion_2019.tiff", width = 6, height = 7, dpi = 600)

model_out_clean %>% 
  group_by(Year, Farmtype, Stage, Predator, Source) %>%
  summarise(Proportion = mean(`50%`, na.rm = T),
            Proportion = round(Proportion, 2),
            n = n(),
            SD = sd(`50%`),
            SE = SD/sqrt(n),
            SE = round(SE, 2)) %>%
  ungroup() %>%
  mutate(Proportion2 = ifelse(!is.na(SE), paste(Proportion, "±", SE), Proportion)) %>%
  select(-Proportion, -SD, -SE) %>%
  pivot_wider(names_from = Source, values_from = Proportion2) %>%
  relocate(n, .after = Detritivore) %>%
  write_csv("Output/Data_clean/Proportion.csv")


# 2. Line charts of rice herbivore consumption by predators --------------------
label2 <- data.frame(Predator = c("All", "Spider", "Ladybeetle", "All", "Spider", "Ladybeetle"),
                     Farmtype = c("Or", "Or", "Or", "Cv", "Cv", "Cv"),   
                     Year = "2017", 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(1.12, 1.12, 1.12, 1, 1, 1),
                     Label = c("(a)", "(b)", "(c)", "", "", "")) %>%
  mutate(Predator = factor(Predator, levels = unique(Predator), ordered = T),
         Farmtype = factor(Farmtype, levels = unique(Farmtype), ordered = T))

model_out_clean %>% 
  filter(Source == "Rice_herb") %>%
  group_by(Predator, Farmtype, Stage, Year) %>%
  summarise(Proportion = mean(`50%`, na.rm = T)) %>%
  ggplot(aes(x = Stage, y = Proportion, linetype = Year, shape = Year, color = Year, group = Year)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) + 
  facet_grid(Predator~Farmtype, labeller = as_labeller(c("Or" = "Organic", 
                                                         "Cv" = "Conventional",
                                                         "All" = "Both predators",
                                                         "Spider" = "Spiders",
                                                         "Ladybeetle" = "Ladybeetles"))) + 
  geom_text(data = label2, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Proportion of rice herbivores consumed in the diet") +
  scale_color_manual(values = c("#00BA38", "#00BA38", "#00BA38"), 
                     labels = c("2017", "2018", "2019"), 
                     name = "") +
  scale_shape_manual(values = c(16, 16, 16), 
                     labels = c("2017", "2018", "2019"), 
                     name = "") +
  scale_linetype_manual(values = c(6, 2, 1),
                        labels = c("2017", "2018", "2019"), 
                        name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"))

ggsave("Output/Figures/Rice_herb_consumption.tiff", width = 6, height = 7, dpi = 600)


# 3. Line charts of prey relative abundance over crop stage --------------------
### Clean the datasets
Abd_2017_clean <- Abd_2017 %>% 
  select(1:4) %>%
  mutate(Year = 2017) %>%
  mutate(Crop_stage = factor(Stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.ID,
                                Rice_herb = c("Del", "Cic", "Pen", "Aly", "Lyg", "Pyr", "Hes", "Pyl"),
                                Tourist_herb = c("Acr", "Chr"),
                                Detritivore = c("Chi", "Sci", "Mus", "Eph", "Emp", "Str", "Chl", "Ter"),
                                Predator = c("Ara", "Coc", "Tet"),
                                other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Predator", "Rice_herb", "Tourist_herb", "Detritivore", "Others"), ordered = T)) %>%
  select(Year, Farm_type, Crop_stage, Trophic, Count)

Abd_2018_clean <- Abd_2018 %>%
  select(1:4) %>%
  mutate(Year = 2018) %>%
  mutate(Crop_stage = plyr::mapvalues(Date, from = c("20180402", "20180506", "20180608", "20180629"), to = c("Seedling", "Tillering", "Flowering", "Ripening"))) %>%
  mutate(Crop_stage = factor(Crop_stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.abbr.,
                                Rice_herb = c("Del", "Cic", "Pen", "Aly", "Lyg", "Pyr", "Hes", "Pyl"),
                                Tourist_herb = c("Acr", "Chr"),
                                Detritivore = c("Chi", "Sci", "Mus", "Eph", "Emp", "Str", "Chl", "Ter"),
                                Predator = c("Ara", "Coc", "Tet"),
                                other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Predator", "Rice_herb", "Tourist_herb", "Detritivore", "Others"), ordered = T)) %>%
  select(Year, Farm_type, Crop_stage, Trophic, Count = Abundance)

Abd_2019_clean <- Abd_2019 %>%
  select(1:4) %>%
  mutate(Year = 2019) %>%
  mutate(Family.abbr. = str_to_title(Family.abbr.)) %>%
  mutate(Crop_stage = plyr::mapvalues(Date, from = c("20190513", "20190620", "20190702"), to = c("Tillering", "Flowering", "Ripening"))) %>%
  mutate(Crop_stage = factor(Crop_stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.abbr.,
                                Rice_herb = c("Del", "Cic", "Pen", "Aly", "Lyg", "Pyr", "Hes", "Pyl"),
                                Tourist_herb = c("Acr", "Chr"),
                                Detritivore = c("Chi", "Sci", "Mus", "Eph", "Emp", "Str", "Chl", "Ter"),
                                Predator = c("Ara", "Coc", "Tet"),
                                other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Predator", "Rice_herb", "Tourist_herb", "Detritivore", "Others"), ordered = T)) %>%
  select(Year, Farm_type, Crop_stage, Trophic, Count = Abundance)

Abd_all <- bind_rows(Abd_2017_clean, Abd_2018_clean, Abd_2019_clean) %>%
  na.omit()

### Plot
label1 <- data.frame(Year = c(2017, 2018, 2019, 2017, 2018, 2019),
                     Farm_type = c("Organic", "Organic", "Organic", "Conventional", "Conventional", "Conventional"),   
                     Trophic = rep("Rice_herb", 6), 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(1.12, 1.12, 1.12, 1, 1, 1),
                     Label = c("(a)", "(b)", "(c)", "", "", "")) %>%
  mutate(Year = factor(Year, levels = unique(Year), ordered = T),
         Farm_type = factor(Farm_type, levels = unique(Farm_type), ordered = T))

Abd_all %>% 
  mutate(Year = factor(Year)) %>% 
  filter(Trophic %in% c("Rice_herb", "Tourist_herb", "Detritivore")) %>%
  dplyr::group_by(Year, Farm_type, Crop_stage, Trophic) %>%
  dplyr::summarise(n = sum(Count)) %>%
  dplyr::mutate(Proportion = n/sum(n)) %>% 
  mutate(Proportion = case_when(
    Year == 2018 & Crop_stage == "Seedling" & Trophic == "Detritivore" ~ 0.95,
    T ~ Proportion)) %>% 
  ggplot(aes(x = Crop_stage, y = Proportion, color = Trophic, shape = Trophic, group = Trophic)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) +
  facet_grid(Year~Farm_type) + 
  geom_text(data = label1, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Relative abundances of prey sources") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values = c(16, 15, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        axis.title.x = element_text(margin = margin(t = 2)),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"))

ggsave("Output/Figures/Rel_abd.tiff", width = 6.2, height = 7.5, dpi = 600)


# 4. Line charts of the abundance of predators over crop stages ----------------
### Clean the datasets
Abd_2017_clean_predator <- Abd_2017 %>% 
  select(1:4) %>%
  mutate(Year = 2017) %>%
  mutate(Crop_stage = factor(Stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.ID, 
                                Spider = c("Ara", "Tet"),
                                Ladybeetle = "Coc", 
                                other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Spider", "Ladybeetle", "Others"), ordered = T)) %>%
  select(Year, Farm_type, Crop_stage, Trophic, Count)

Abd_2018_clean_predator <- Abd_2018 %>%
  select(1:4) %>%
  mutate(Year = 2018) %>%
  mutate(Crop_stage = plyr::mapvalues(Date, from = c("20180402", "20180506", "20180608", "20180629"), to = c("Seedling", "Tillering", "Flowering", "Ripening"))) %>%
  mutate(Crop_stage = factor(Crop_stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.abbr.,
                                Spider = c("Ara", "Tet"),
                                Ladybeetle = "Coc", 
                                other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Spider", "Ladybeetle", "Others"), ordered = T)) %>%
  select(Year, Farm_type, Crop_stage, Trophic, Count = Abundance)

Abd_2019_clean_predator <- Abd_2019 %>%
  select(1:4) %>%
  mutate(Year = 2019) %>%
  mutate(Family.abbr. = str_to_title(Family.abbr.)) %>%
  mutate(Crop_stage = plyr::mapvalues(Date, from = c("20190513", "20190620", "20190702"), to = c("Tillering", "Flowering", "Ripening"))) %>%
  mutate(Crop_stage = factor(Crop_stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = plyr::mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.abbr.,
                                Spider = c("Ara", "Tet"),
                                Ladybeetle = "Coc", 
                                other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Spider", "Ladybeetle", "Others"), ordered = T)) %>%
  select(Year, Farm_type, Crop_stage, Trophic, Count = Abundance)

Abd_all_predator <- bind_rows(Abd_2017_clean_predator, 
                              Abd_2018_clean_predator, 
                              Abd_2019_clean_predator) %>%
  na.omit()


### Plot
label2 <- data.frame(Year = c(2017, 2018, 2019, 2017, 2018, 2019),
                     Farm_type = c("Organic", "Organic", "Organic", "Conventional", "Conventional", "Conventional"),   
                     Trophic = rep("Spider", 6), 
                     x = c(1, 1, 1, 1, 1, 1), 
                     y = c(6.3, 8.3, 30.3, 6, 8, 30),
                     Label = c("(a)", "(b)", "(c)", "", "", "")) %>%
  mutate(Year = factor(Year, levels = unique(Year), ordered = T),
         Farm_type = factor(Farm_type, levels = unique(Farm_type), ordered = T))

Abd_all_predator %>% 
  filter(Trophic %in% c("Spider", "Ladybeetle")) %>%
  mutate(Year = factor(Year, levels = unique(Year), ordered = T)) %>% 
  dplyr::group_by(Year, Farm_type, Crop_stage, Trophic) %>%
  dplyr::summarise(n_predator = sum(Count),
                   n_farm = n(),
                   mean = mean(Count),
                   SD = sd(Count),
                   SE = SD/sqrt(n_farm)) %>%
  ggplot(aes(x = Crop_stage, y = mean, color = Trophic, shape = Trophic, group = Trophic)) +
  geom_line(position = position_dodge(0.1), size = 1.2) +
  geom_point(position = position_dodge(0.1), size = 3) +
  geom_errorbar(aes(x = Crop_stage, ymin = mean-SE, ymax = mean+SE, color = Trophic), 
                position = position_dodge(0.1), inherit.aes = F,
                width = 0.35, size = 0.8) + 
  facet_grid(Year~Farm_type, scales = "free_y") + 
  geom_text(data = label2, aes(x = x, y = y, label = Label), 
            size = 5, color = "black", nudge_x = -0.5, vjust = -0.7) +
  coord_cartesian(clip = "off") +
  xlab("Crop stage") +
  ylab("Number of predators (mean ± SE)") +
  scale_color_manual(values = c("#009E73", "#E69F00"), labels = c("Spiders", "Ladybeetles"), name = "") +
  scale_shape_manual(values = c(16, 15), labels = c("Spiders", "Ladybeetles"), name = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        axis.title.x = element_text(margin = margin(t = 2)),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.background.y = element_rect(fill = "grey80"))

ggsave("Output/Figures/abd_predator.tiff", width = 6.2, height = 7.5, dpi = 600)


# 5. Density plots of the posterior draws in the predator model ----------------
### An organic farm in 2017
P_organic <- 
  Posterior_draws_predator$MO_2017 %>% 
  mutate(Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening")),
         Prey_source = factor(Prey_source, levels = c("Rice_herb", "Tour_herb", "Detritivore"))) %>% 
  ggplot() + 
  geom_density(aes(x = Draw, y = after_stat(scaled), color = Prey_source, fill = Prey_source), 
               adjust = 2, size = 0.8, alpha = 0.75) + 
  labs(x = NULL, y = NULL, subtitle = "(a) Organic") +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.25, 0.50, 0.75, 1.0),
                     expand = c(0, 0)) +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_fill_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  facet_grid(~Stage) + 
  my_theme + 
  theme(legend.position = "right",
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.7, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.text = element_text(margin = margin(l = 1.5)),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 10, color = "black", margin = margin(t = 3)),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.ticks.length.x = unit(0.12, "cm")) +
  guides(color = guide_legend(byrow = T))

### A conventional farm in 2017
P_conventional <- 
Posterior_draws_predator$MC_2017 %>% 
  mutate(Stage = factor(Stage, levels = c("Tillering", "Flowering", "Ripening")),
         Prey_source = factor(Prey_source, levels = c("Rice_herb", "Tour_herb", "Detritivore"))) %>% 
  ggplot() + 
  geom_density(aes(x = Draw, y = after_stat(scaled), color = Prey_source, fill = Prey_source), 
               adjust = 2, size = 0.8, alpha = 0.75) + 
  labs(x = NULL, y = NULL, subtitle = "(b) Conventional") +
  scale_x_continuous(limits = c(-0.01, 1.01), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                     labels = c("0", "0.2", "0.4", "0.6", "0.8", "1"),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.1), breaks = c(0, 0.25, 0.50, 0.75, 1.0),
                     expand = c(0, 0)) +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_fill_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  facet_grid(~Stage) + 
  my_theme + 
  theme(legend.position = "right",
        legend.key.width = unit(0.7, "cm"),
        legend.key.height = unit(0.7, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.text = element_text(margin = margin(l = 1.5)),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(size = 10, color = "black", margin = margin(t = 3)),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.ticks.length.x = unit(0.12, "cm")) + 
  guides(color = guide_legend(byrow = T))

### Combined
P_density <- ggarrange(P_organic, P_conventional, nrow = 2, 
                       common.legend = T, legend = "right")

annotate_figure(P_density,
                bottom = text_grob("Posterior draws", size = 15, hjust = 0.85),
                left = text_grob("Scaled density", size = 15, rot = 90))

ggsave("Output/Figures/density_plot.tiff", width = 8, height = 5.2, dpi = 600, device = "tiff")


# 6. Stable isotope biplot of rice plant and prey sources ----------------------
### Stable isotope signatures of rice plant
Rice_SI_summary <- Rice_data %>% 
  group_by(Species) %>% 
  summarise(mean_d13C = mean(d_13C),
            mean_d15N = mean(d_15N),
            n = n(),
            SE_d13C = sd(d_13C)/sqrt(n),
            SE_d15N = sd(d_15N)/sqrt(n))

### Stable isotope signatures of prey sources
rice_herb <- c("DEL", "CIC", "PEN", "ALY", "LYG")
tour_herb <- c("ACR", "CHR")
detritivore <- c("CHI", "SCI", "MUS", "EPH", "EMP", "STR", "CHL", "TER")

Source_SI_summary <- SID_all_clean %>% 
  filter(Stage != "Seedling") %>% 
  mutate(Prey_source = case_when(Family %in% rice_herb ~ "Rice_herb",
                                 Family %in% tour_herb ~ "Tour_herb",
                                 Family %in% detritivore ~ "Detritivore",
                                 T ~ "Non")) %>% 
  filter(Prey_source != "Non") %>% 
  mutate(Prey_source = factor(Prey_source, levels = c("Rice_herb", "Tour_herb", "Detritivore"), ordered = T)) %>% 
  group_by(Prey_source) %>% 
  summarise(mean_d13C = mean(d13C),
            mean_d15N = mean(d15N),
            n = n(),
            SE_d13C = sd(d13C)/sqrt(n),
            SE_d15N = sd(d15N)/sqrt(n))

### SI biplot of rice plant and prey sources
ggplot() +
  geom_point(data = Source_SI_summary, aes(x = mean_d13C, y = mean_d15N, color = Prey_source, shape = Prey_source), size = 2.5) +
  geom_errorbar(data = Source_SI_summary, aes(x = mean_d13C, ymin = mean_d15N-SE_d15N*1.96, ymax = mean_d15N+SE_d15N*1.96, color = Prey_source), width = 0.2, size = 1) +
  geom_errorbarh(data = Source_SI_summary, aes(y = mean_d15N, xmin = mean_d13C-SE_d13C*1.96, xmax = mean_d13C+SE_d13C*1.96, color = Prey_source), height = 0.17, size = 1) +
  geom_point(data = Rice_SI_summary, aes(x = mean_d13C, y = mean_d15N), size = 2.5) +
  geom_errorbar(data = Rice_SI_summary, aes(x = mean_d13C, ymin = mean_d15N-SE_d15N*1.96, ymax = mean_d15N+SE_d15N*1.96), width = 0.2, size = 1) +
  geom_errorbarh(data = Rice_SI_summary, aes(y = mean_d15N, xmin = mean_d13C-SE_d13C*1.96, xmax = mean_d13C+SE_d13C*1.96), height = 0.17, size = 1) +
  my_theme + 
  labs(x = expression(paste(delta^{13}, "C (\u2030)", sep = "")), y = expression(paste(delta^{15}, "N (\u2030)", sep = ""))) +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values = c(16, 15, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  annotate(geom = "text", x = -28.7, y = 5.8, label = "Rice plant", size = 4.5) + 
  theme(legend.position = c(0.28, 0.85))

ggsave("Output/Figures/Biplot.tiff", width = 6, height = 5, dpi = 600)




