########## Rice Project - Dietary Proportions and Relative Abundance Figure ##########
########## Author: Gen-Chang Hsu ##########
library(ggplot2)
library(plyr)
library(dplyr)
library(xlsx)
library(ggpubr)

### Load the dietary proportion data
Summary <- read.csv('Output/Data_clean/Mix_mod_out_summary.csv', header = T)
Summary$Farm.ID <- ordered(Summary$Farm.ID, levels = c("MO1", "MO2", "MO3", "MC1", "MC2", "MC3","LO1", "LO2", "LO3", "LC1", "LC2", "LC3", "SO1", "SC1"))
Summary$Farmtype <- ordered(Summary$Farmtype, levels = c("Or", "Cv"))
Summary$Landscape <- ordered(Summary$Landscape, levels = c("Mount", "Land", "Sea"))
Summary$Stage <- ordered(Summary$Stage, levels = c("Seedling", "Tillering", "Flowering", "Ripening"))
Summary$Source <- ordered(Summary$Source, levels = c("Rice.herb", "Tour.herb", "Detritivore"))

Summary2 <- ddply(Summary, c("Farmtype", "Stage", "Source"), summarise, mean = mean(Mean), Sd = sd(Mean), n = length(Mean), Se = Sd/sqrt(n))
Summary2$Farmtype2 <- as.character(Summary2$Farmtype) %>%
  replace(as.character(Summary2$Farmtype) == "Or", "Organic") %>%
  replace(as.character(Summary2$Farmtype) == "Cv", "Conventional")
Summary2$Farmtype2 <- ordered(Summary2$Farmtype2, levels = c("Organic", "Conventional"))
write.csv(Summary2, "Output/Data_clean/Mix_mod_out_summary2.csv")

Labs <- data.frame(Farmtype2 = c("Organic", "Conventional"),
                   x = c(2, 2),
                   y = c(1, 1),
                   Source = c("Rice.herb", "Rice.herb"))

P1 <- ggplot(Summary2[Summary2$Stage != "Seedling", ] , aes(x = Stage, y = mean, color = Source, shape = Source, group = Source)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  geom_line(size = 1, position = position_dodge(0.1)) +
  facet_grid(~Farmtype2) +
  geom_errorbar(aes(ymin = ifelse((mean-Se)>0, mean-Se, 0), ymax = ifelse((mean+Se)<1, mean+Se, 1)), width = 0.2, position = position_dodge(0.1), size = 0.6) +
  xlab(NULL) +
  ylab(paste("Proportion of prey sources in \n predators' diet", " (Mean ", "\u00B1", " SE)", sep = "")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 12.5, margin = margin(t = 12)),
        axis.title.y = element_text(size = 12.5, margin = margin(r = 10)),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.background = element_rect(fill = NA, color = "transparent"),
        strip.text.x = element_text(size = 10),
        legend.position = c(0.5, 1.15),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(0, "line"),
        legend.key = element_blank(),
        legend.text = element_text(size = 8.5),
        legend.title = element_text(size = 10),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "transparent", size = 0.5, linetype = "solid", colour = "transparent")) +
  scale_color_manual(values=c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_shape_manual(values=c(15, 16, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

### Load the arthropod abundance data
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

### Subset to different trophic guilds
# Predator
Predator.Abd <- subset(Abundance , Family == "ARA"|
                         Family == "COC"|
                         Family == "TET")
Predator.Sum <- ddply(Predator.Abd, c("Farm.ID", "Farmtype", "Stage"), summarise, Total.Abd = sum(Abundance), Trophic = "Predator")
Predator.Mean <- ddply(Predator.Sum, c("Farmtype", "Stage"), summarise, Mean.Abd = mean(Total.Abd), SE = sd(Total.Abd)/sqrt(length(Total.Abd)), Trophic = "Predator", .drop = F)

# Rice herbivore
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
Source.Abd.Summary$Farmtype2 <- as.character(Source.Abd.Summary$Farmtype) %>%
  replace(as.character(Source.Abd.Summary$Farmtype) == "Or", "Organic") %>%
  replace(as.character(Source.Abd.Summary$Farmtype) == "Cv", "Conventional")
Source.Abd.Summary$Farmtype2 <- ordered(Source.Abd.Summary$Farmtype2, levels = c("Organic", "Conventional"))

P2 <- ggplot(Source.Abd.Summary, aes(x = Stage, y = Rel.Abd, color = Trophic, shape = Trophic, group = Trophic)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  geom_line(size = 1, position = position_dodge(0.1)) +
  facet_grid(~Farmtype2) +
  xlab("Crop stage") +
  ylab("\n Relative abundance") +
  ylim(0, 1) +
  theme(axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 15, margin = margin(t = 10)),
        axis.title.y = element_text(size = 15, margin = margin(r = 5)),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 10, hjust = 0.5),
        legend.position = "top",
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.key.size = unit(0.75, "line"),
        legend.key = element_blank(),
        legend.text = element_text(size = 8.5),
        legend.title = element_text(size = 10),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "transparent", size = 0.5, linetype = "solid", colour = "transparent")) +
  scale_color_manual(values=c("#00BA38", "#619CFF", "#993300"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "", guide = F)+
  scale_shape_manual(values=c(15, 16, 17), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore"), name = "", guide = F) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)

ggarrange(P1, P2, labels = c("(a)", "(b)"), label.x = 0.02, nrow = 2)
ggsave("Output/Figures/Diet_prop_rel_abd.tiff", width = 7, height = 6, dpi = 600)





