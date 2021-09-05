########## Rice Project - Arthropod Abundance ##########
########## Author: Gen-Chang Hsu ##########
library(tidyverse)
library(plyr)
library(xlsx)
library(ggpubr)
library(grid)
library(cowplot)

### Load the abundance data
Abundance <- read.xlsx("Data_raw/Arthropod abundance data sheet.xlsx", header = T, sheetIndex = 1)
Abundance <- Abundance %>% select(1:4) %>%
  mutate(Crop_stage = mapvalues(Date, from = c("20180402", "20180506", "20180608", "20180629"), to = c("Seedling", "Tillering", "Flowering", "Ripening"))) %>%
  mutate(Crop_stage = factor(Crop_stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"), ordered = T)) %>%
  mutate(Farm_type = str_sub(Farm, start = 2, end = 2)) %>%
  mutate(Farm_type = mapvalues(Farm_type, from = c("O", "C"), to = c("Organic", "Conventional"))) %>%
  mutate(Farm_type = factor(Farm_type, levels = c("Organic", "Conventional"), ordered = T)) %>%
  mutate(Trophic = fct_collapse(Family.abbr.,
    Rice_herb = c("Del", "Cic", "Pen", "Aly", "Lyg", "Pyr", "Hes", "Pyl"),
    Tourist_herb = c("Acr", "Chr"),
    Detritivore = c("Chi", "Sci", "Mus", "Eph", "Emp", "Str", "Chl", "Ter"),
    Predator = c("Ara", "Coc", "Tet"),
    other_level = "Others")) %>%
  mutate(Trophic = factor(Trophic, levels = c("Rice_herb", "Tourist_herb", "Detritivore", "Predator", "Others"), ordered = T))


### Summary of the abundance data
Abd_summary <- Abundance %>% dplyr::filter(Trophic != "Others") %>%
  droplevels() %>%
  dplyr::group_by(Farm_type, Crop_stage, Trophic) %>%
  dplyr::summarise(Mean = mean(Abundance),
            SD = sd(Abundance),
            n = n(),
            SE = SD/sqrt(n))


### Abundance figure
a <- text_grob("(a)")
b <- text_grob("(b)")
c <- text_grob("(c)")
d <- text_grob("(d)")

P <- ggplot(data = Abd_summary, aes(x = Crop_stage, y = Mean, color = Trophic, shape = Farm_type)) +
  geom_point(position = position_dodge(width = 0.1), size = 2) +
  geom_line(aes(group = Farm_type, linetype = Farm_type), position = position_dodge(width = 0.1)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0, position = position_dodge(width = 0.1)) +
  facet_wrap(~Trophic, scales = "free_y", labeller = as_labeller(c("Rice_herb" = "Rice herbivore", "Tourist_herb" = "Tourist herbivore", "Detritivore" = "Detritivore", "Predator" = "Predator"))) +
  labs(x = "Crop stage", y = "Number of individuals (Mean \u00B1 SE)") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300", "Black"), guide = F) +
  scale_shape_manual(values = c(16, 1)) +
  scale_y_continuous(limits =  c(0, NA)) +
  geom_point(data = data.frame(x = 1, y = 15, Trophic = "Tourist_herb", Farm_type = "Organic"), aes(x = x, y = y), alpha = 0) +
  geom_point(data = data.frame(x = 1, y = 6, Trophic = "Predator", Farm_type = "Organic"), aes(x = x, y = y), alpha = 0) +
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.title.y = element_text(size = 15, margin = margin(r = 6)),
        plot.margin = rep(unit(0.1,"null"),4),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(color = "black", fill = NA),
        strip.text = element_text(size = 12),
        strip.background = element_blank(),
        legend.spacing.x = unit(0.2, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.65, "cm"),
        legend.key.size = unit(2, "line"),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.box.just = "center",
        legend.justification = c(0, 0.5),
        legend.title.align = 0.5,
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent", size = 0.5, linetype = "solid", colour = "transparent"))

ggdraw(P) +
  draw_plot(a, x = 0.03, y = 0.9, width = 0.1, height = 0.1) +
  draw_plot(b, x = 0.42, y = 0.9, width = 0.1, height = 0.1) +
  draw_plot(c, x = 0.03, y = 0.47, width = 0.1, height = 0.1) +
  draw_plot(d, x = 0.42, y = 0.47, width = 0.1, height = 0.1)

ggsave("Abundance.tiff", width = 10, height = 6, dpi = 600)


### Main taxa
Predator <- Abundance %>% dplyr::filter(Trophic == "Predator") %>%
  droplevels() %>%
  dplyr::group_by(Farm_type, Family.abbr.) %>%
  dplyr::summarise(n = sum(Abundance)) %>%
  mutate(Prop = n/sum(n))

Rice_herb <- Abundance %>% dplyr::filter(Trophic == "Rice_herb") %>%
  droplevels() %>%
  dplyr::group_by(Farm_type, Family.abbr.) %>%
  dplyr::summarise(n = sum(Abundance)) %>%
  mutate(Total = sum(n)) %>%
  mutate(Prop = n/sum(n))


### Dietary proportions of predators by each farm
### Load the data
Abundance <- read.xlsx("Data_raw/Arthropod abundance data sheet.xlsx", header = T, sheetIndex = 1)
Abundance <- Abundance[, 1:5]
Abundance <- Abundance[complete.cases(Abundance), ]
Abundance$Farmtype <- substr(Abundance$Farm, start = 2, stop = 2) %>%
  replace(substr(Abundance$Farm, start = 2, stop = 2) == "O", "Or") %>%
  replace(substr(Abundance$Farm, start = 2, stop = 2) == "C", "Cv")
Abundance$Landscape <- substr(Abundance$Farm, start = 1, stop = 1) %>%
  replace(substr(Abundance$Farm, start = 1, stop = 1) == "M", "Mount") %>%
  replace(substr(Abundance$Farm, start = 1, stop = 1) == "L", "Land") %>%
  replace(substr(Abundance$Farm, start = 1, stop = 1) == "S", "Sea")
Abundance$Stage <- Abundance$Date %>%
  replace(Abundance$Date == "20180402", "Seedling") %>%
  replace(Abundance$Date == "20180506", "Tillering") %>%
  replace(Abundance$Date == "20180608", "Flowering") %>%
  replace(Abundance$Date == "20180629", "Ripening")

Abundance <- Abundance[, c(2:8, 1)]
names(Abundance) <- c("Farm.ID", "Family", "Abundance", "N.capsule", "Farmtype", "Landscape", "Stage", "Date")
Abundance$Family <- as.character(Abundance$Family) %>%
  toupper()

Abundance$Farm.ID <- ordered(Abundance$Farm.ID, levels = c("MO-1", "MO-2", "MO-3", "MC-1", "MC-2", "MC-3","LO-1", "LO-2", "LO-3", "LC-1", "LC-2", "LC-3", "SO-1", "SC-1"))
Abundance$Farmtype <- ordered(Abundance$Farmtype, levels = c("Or", "Cv"))
Abundance$Landscape <- ordered(Abundance$Landscape, levels = c("Mount", "Land", "Sea"))
Abundance$Stage <- ordered(Abundance$Stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"))

### Subset to different trophic groups
# Predator
Predator.Abd <- subset(Abundance , Family == "ARA"|
                         Family == "COC"|
                         Family == "TET")
Predator.Sum <- ddply(Predator.Abd, c("Farm.ID", "Farmtype", "Stage"), summarise, Total.Abd = sum(Abundance), Trophic = "Predator")
Predator.Mean <- ddply(Predator.Sum, c("Farmtype", "Stage"), summarise, Mean.Abd = mean(Total.Abd), SE = sd(Total.Abd)/sqrt(length(Total.Abd)), Trophic = "Predator", .drop = F)

# Rice-associated herbivore
Rice.herb.Abd <- subset(Abundance, Family == "DEL"|
                          Family == "CIC"|
                          Family == "PEN"|
                          Family == "ALY"|
                          Family == "LYG"|
                          Family == "PYR"|
                          Family == "HES"|
                          Family == "PYL")

Rice.herb.Sum <- ddply(Rice.herb.Abd, c("Farm.ID", "Farmtype", "Stage"), summarise, Total.Abd = sum(Abundance), Trophic = "Rice.herb")
Rice.herb.Mean <- ddply(Rice.herb.Sum, c("Farmtype", "Stage"), summarise, Mean.Abd = mean(Total.Abd), SE = sd(Total.Abd)/sqrt(length(Total.Abd)), Trophic = "Rice.herb", .drop = F)

# Tourist herbivore
Tour.herb.Abd <- subset(Abundance, Family == "ACR"|
                          Family == "CHR")

Tour.herb.Sum <- ddply(Tour.herb.Abd, c("Farm.ID", "Farmtype", "Stage"), summarise, Total.Abd = sum(Abundance), Trophic = "Tour.herb")
Tour.herb.Mean <- ddply(Tour.herb.Sum, c("Farmtype", "Stage"), summarise, Mean.Abd = mean(Total.Abd), SE = sd(Total.Abd)/sqrt(length(Total.Abd)), Trophic = "Tour.herb", .drop = F)

# Detritivore
Detritivore.Abd <- subset(Abundance, Family == "CHI"|
                            Family == "SCI"|
                            Family == "MUS"|
                            Family == "EPH"|
                            Family == "EMP"|
                            Family == "STR"|
                            Family == "CHL"|
                            Family == "TER")

Detritivore.Sum <- ddply(Detritivore.Abd, c("Farm.ID", "Farmtype", "Stage"), summarise, Total.Abd = sum(Abundance), Trophic = "Detritivore")
Detritivore.Mean <- ddply(Detritivore.Sum, c("Farmtype", "Stage"), summarise, Mean.Abd = mean(Total.Abd), SE = sd(Total.Abd)/sqrt(length(Total.Abd)), Trophic = "Detritivore", .drop = F)

Source.Abd.Summary <- rbind.data.frame(Rice.herb.Mean, Tour.herb.Mean, Detritivore.Mean)
Source.Abd.Summary <- replace(Source.Abd.Summary, Source.Abd.Summary == "NaN", 0)
Source.Abd.Summary <- Source.Abd.Summary[, c(1, 2, 5, 3, 4)]
Source.Abd.Summary$Trophic <- ordered(Source.Abd.Summary$Trophic, levels = c("Rice.herb", "Tour.herb", "Detritivore"))
Source.Abd.Summary[order(Source.Abd.Summary$Stage), ]
Source.Abd.Summary <- ddply(Source.Abd.Summary, c("Farmtype", "Stage"), transform, Rel.Abd = Mean.Abd/sum(Mean.Abd))


### Dietary proportions of predators by each farm
Source.Abd.Summary2 <- rbind.data.frame(Rice.herb.Sum, Tour.herb.Sum, Detritivore.Sum)
Source.Abd.Summary2 <- ddply(Source.Abd.Summary2, c("Farm.ID", "Stage"), transform, Rel.Abd = Total.Abd/sum(Total.Abd))
write.csv(Source.Abd.Summary2, "Output/Data_clean/Diet_prop_by_farm.csv")




