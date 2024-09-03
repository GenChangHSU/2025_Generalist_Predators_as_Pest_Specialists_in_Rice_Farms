## -----------------------------------------------------------------------------
## Title: Bayesian Stable Isotope Mixing Model with MixSIAR with predators as the fourth prey source
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-30
##
## Description: 
## 1. Assign arthropod families into different trophic guilds
## 2. Prepare consumer data for the mixing model
## 3. Prepare source data for the mixing model
## 4. Prepare C and N trophic discrimination factors
## 5. Run the mixing models using JAGS
## 6. Organize the raw mixing model outputs
## 7. Extract the posterior draws in the predator model
## 
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(vegan)
library(magrittr)
library(MixSIAR)
library(sp)


# Import files -----------------------------------------------------------------
SID_all_clean <- readRDS("Output/Data_clean/SID_all_clean.rds")


############################### Code starts here ###############################

# 1. Trophic guild assignment --------------------------------------------------
predator <- c("ARA", "TET", "COC")
spider <- c("ARA", "TET")
ladybeetle <- c("COC")
rice_herb <- c("DEL", "CIC", "PEN", "ALY", "LYG")
tour_herb <- c("ACR", "CHR")
detritivore <- c("CHI", "SCI", "MUS", "EPH", "EMP", "STR", "CHL", "TER")

SID_all_clean <- filter(SID_all_clean, Stage != "Seedling")  # filter out seedling stage

spider_data <- SID_all_clean %>%
  filter(Family %in% spider)

ladybeetle_data <- SID_all_clean %>%
  filter(Family %in% ladybeetle)

predator_data <- bind_rows(spider_data, ladybeetle_data)

rice_herb_data <- SID_all_clean %>%
  filter(Family %in% rice_herb)

tour_herb_data <- SID_all_clean %>%
  filter(Family %in% tour_herb)

detritivore_data <- SID_all_clean %>%
  filter(Family %in% detritivore)

### Find the predator individuals within the prey mixing polygon
prey_SI_summary <- SID_all_clean %>% 
  filter(Stage != "Seedling") %>% 
  mutate(Prey_source = case_when(Family %in% rice_herb ~ "Rice_herb",
                                 Family %in% tour_herb ~ "Tour_herb",
                                 Family %in% detritivore ~ "Detritivore",
                                 T ~ "Non")) %>% 
  filter(Prey_source != "Non") %>% 
  mutate(Prey_source = factor(Prey_source, levels = c("Rice_herb", "Tour_herb", "Detritivore"), ordered = T)) %>% 
  group_by(Prey_source) %>% 
  summarise(mean_d13C = mean(d13C),
            mean_d15N = mean(d15N))

predator_in_prey_polygon <- point.in.polygon(predator_data$d13C, predator_data$d15N,
                 prey_SI_summary$mean_d13C, prey_SI_summary$mean_d15N) == 1

predator_as_prey_data <- predator_data[predator_in_prey_polygon, ]


# 2. Preparation of consumer data ----------------------------------------------
mixture_predator <- 
  data.frame(d13C = predator_data$d13C,
             d15N = predator_data$d15N,
             Farm = factor(predator_data$Farm),
             Stage = factor(predator_data$Stage, levels = c("Tillering", "Flowering", "Ripening"), ordered = T))

mixture_spider <- 
  data.frame(d13C = spider_data$d13C,
             d15N = spider_data$d15N,
             Farm = factor(spider_data$Farm),
             Stage = factor(spider_data$Stage, levels = c("Tillering", "Flowering", "Ripening"), ordered = T))

mixture_ladybeetle <- 
  data.frame(d13C = ladybeetle_data$d13C,
             d15N = ladybeetle_data$d15N,
             Farm = factor(ladybeetle_data$Farm),
             Stage = factor(ladybeetle_data$Stage, levels = c("Tillering", "Flowering", "Ripening"), ordered = T))

write.csv(mixture_predator, "Output/Temp_IGP/mixture_predator.csv", row.names = FALSE)
write.csv(mixture_spider, "Output/Temp_IGP/mixture_spider.csv", row.names = FALSE)
write.csv(mixture_ladybeetle, "Output/Temp_IGP/mixture_ladybeetle.csv", row.names = FALSE)

mix_siar_predator <- load_mix_data(filename = "Output/Temp_IGP/mixture_predator.csv",
                                   iso_names = c("d13C","d15N"),
                                   factors = c("Farm", "Stage"),
                                   fac_random = c(F, F),
                                   fac_nested = c(F, F),
                                   cont_effects = NULL)

mix_siar_spider <- load_mix_data(filename = "Output/Temp_IGP/mixture_spider.csv",
                                 iso_names = c("d13C","d15N"),
                                 factors = c("Farm", "Stage"),
                                 fac_random = c(F, F),
                                 fac_nested = c(F, F),
                                 cont_effects = NULL)

mix_siar_ladybeetle <- load_mix_data(filename = "Output/Temp_IGP/mixture_ladybeetle.csv",
                                     iso_names = c("d13C","d15N"),
                                     factors = c("Farm", "Stage"),
                                     fac_random = c(F, F),
                                     fac_nested = c(F, F),
                                     cont_effects = NULL)


# 3. Preparation of source data ------------------------------------------------
source <- list(rice_herb_data, tour_herb_data, detritivore_data, predator_as_prey_data) %>% 
  map(., function(x){
    select(x, d13C, d15N, Concd13C = C_conc, Concd15N = N_conc, Farm)
  }) %>%
  `names<-`(c("Rice_herb", "Tour_herb", "Detritivore", "Predator")) %>%
  bind_rows(.id = "Source")

write.csv(source, "Output/Temp_IGP/source.csv", row.names = FALSE)

source_mix_siar_predator <- load_source_data(filename = "Output/Temp_IGP/source.csv",
                           source_factors = NULL,
                           conc_dep = TRUE,
                           data_type = "raw",
                           mix_siar_predator)

source_mix_siar_spider <- load_source_data(filename = "Output/Temp_IGP/source.csv",
                                             source_factors = NULL,
                                             conc_dep = TRUE,
                                             data_type = "raw",
                                             mix_siar_spider)

source_mix_siar_ladybeetle <- load_source_data(filename = "Output/Temp_IGP/source.csv",
                                           source_factors = NULL,
                                           conc_dep = TRUE,
                                           data_type = "raw",
                                           mix_siar_ladybeetle)


# 4. Preparation of C and N trophic discrimination factors ---------------------
TDF_C_fun <- function(x){-0.113*x - 1.916}
TDF_N_fun <- function(x){-0.311*x + 4.065}

TDF <- source %>% 
  group_by(Source) %>%
  summarise(Meand13C = mean(TDF_C_fun(d13C)), SDd13C = sd(TDF_C_fun(d13C)),
            Meand15N = mean(TDF_N_fun(d15N)), SDd15N = sd(TDF_N_fun(d15N)))

TDF <- TDF %>%
  rows_update(., tibble(Source = "Predator", Meand13C = 0.5, SDd13C = 0.13, Meand15N = 1.4, SDd15N = 0.2))

write.csv(TDF, "Output/Temp_IGP/TDF.csv", row.names = F)

TDF_out <- TDF %>% 
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>% 
  mutate(Mean_SD_d13C = str_c(Meand13C, SDd13C, sep = " ± "),
         Mean_SD_d15N = str_c(Meand15N, SDd15N, sep = " ± ")) %>% 
  select(Source, Mean_SD_d13C, Mean_SD_d15N)

write.csv(TDF_out, "Output/Temp_IGP/TDF_Out.csv", row.names = F)

discr_mix_siar_predator <- load_discr_data(filename = "Output/Temp_IGP/TDF.csv", mix_siar_predator)
discr_mix_siar_spider <- load_discr_data(filename = "Output/Temp_IGP/TDF.csv", mix_siar_spider)
discr_mix_siar_ladybeetle <- load_discr_data(filename = "Output/Temp_IGP/TDF.csv", mix_siar_ladybeetle)


# 5. Run the mixing models using JAGS -------------------------------------------
### Write JAGS files
dr <- getwd()
setwd(dir = paste0(dr, "/Output/Temp_IGP/JAGS"))

resid_err <- T
process_err <- T
write_JAGS_model("JAGS_predator.txt", resid_err, process_err, mix_siar_predator, source_mix_siar_predator)
write_JAGS_model("JAGS_spider.txt", resid_err, process_err, mix_siar_spider, source_mix_siar_spider)
write_JAGS_model("JAGS_ladybeetle.txt", resid_err, process_err, mix_siar_ladybeetle, source_mix_siar_ladybeetle)

### Run JAGS files
jags_predator <- run_model(run = "short", 
                           mix_siar_predator, 
                           source_mix_siar_predator, 
                           discr_mix_siar_predator, 
                           "JAGS_predator.txt", 
                           alpha.prior = 1, 
                           resid_err, 
                           process_err)

jags_spider <- run_model(run = "short", 
                         mix_siar_spider, 
                         source_mix_siar_spider, 
                         discr_mix_siar_spider, 
                         "JAGS_spider.txt", 
                         alpha.prior = 1, 
                         resid_err, 
                         process_err)

jags_ladybeetle <- run_model(run = "short", 
                             mix_siar_ladybeetle, 
                             source_mix_siar_ladybeetle, 
                             discr_mix_siar_ladybeetle, 
                             "JAGS_ladybeetle.txt", 
                             alpha.prior = 1, 
                             resid_err, 
                             process_err)

### Evaluate JAGS outputs
# setwd("./Output/Temp_IGP/JAGS")
options(max.print = 1000000)

output_JAGS_predator <- 
output_JAGS(jags_predator, mix_siar_predator, source_mix_siar_predator, 
            output_options = list(summary_save = T,
                                  summary_name = "model_out_predator",
                                  sup_post = T,
                                  plot_post_save_pdf = F,
                                  plot_post_name = "Posterior_density",
                                  sup_pairs = T,
                                  plot_pairs_save_pdf = F,
                                  plot_pairs_name = "Pairs_plot",
                                  sup_xy = T,
                                  plot_xy_save_pdf = F,
                                  plot_xy_name = "xy_plot",
                                  gelman = T,
                                  heidel = F,
                                  geweke = T,
                                  diag_save = T,
                                  diag_name = "Diagnostics_predator",
                                  indiv_effect = F,
                                  plot_post_save_png = F,
                                  plot_pairs_save_png = F,
                                  plot_xy_save_png = F))

output_JAGS_spider <- 
output_JAGS(jags_spider, mix_siar_spider, source_mix_siar_spider, 
            output_options = list(summary_save = T,
                                  summary_name = "model_out_spider",
                                  sup_post = T,
                                  plot_post_save_pdf = F,
                                  plot_post_name = "Posterior_density",
                                  sup_pairs = T,
                                  plot_pairs_save_pdf = F,
                                  plot_pairs_name = "Pairs_plot",
                                  sup_xy = T,
                                  plot_xy_save_pdf = F,
                                  plot_xy_name = "xy_plot",
                                  gelman = T,
                                  heidel = F,
                                  geweke = T,
                                  diag_save = T,
                                  diag_name = "Diagnostics_spider",
                                  indiv_effect = F,
                                  plot_post_save_png = F,
                                  plot_pairs_save_png = F,
                                  plot_xy_save_png = F))

output_JAGS_ladybeetle <- 
output_JAGS(jags_ladybeetle, mix_siar_ladybeetle, source_mix_siar_ladybeetle, 
            output_options = list(summary_save = T,
                                  summary_name = "model_out_ladybeetle",
                                  sup_post = T,
                                  plot_post_save_pdf = F,
                                  plot_post_name = "Posterior_density",
                                  sup_pairs = T,
                                  plot_pairs_save_pdf = F,
                                  plot_pairs_name = "Pairs_plot",
                                  sup_xy = T,
                                  plot_xy_save_pdf = F,
                                  plot_xy_name = "xy_plot",
                                  gelman = T,
                                  heidel = F,
                                  geweke = T,
                                  diag_save = T,
                                  diag_name = "Diagnostics_ladybeetle",
                                  indiv_effect = F,
                                  plot_post_save_png = F,
                                  plot_pairs_save_png = F,
                                  plot_xy_save_png = F))

setwd("../../..")


# 6. Organize the raw mixing model outputs -------------------------------------
model_out_predator_raw <- read.table("Output/Temp_IGP/JAGS/model_out_predator.txt", header = F, fill = TRUE)
model_out_predator_clean <- bind_cols(model_out_predator_raw[5:372, c(1:4, 7)], model_out_predator_raw[376:743, 2]) %>%
  `names<-`(c("ID", "Mean", "SD", "2.5%", "50%", "97.5%")) %>%
  mutate(Predator = "All") %>% 
  separate(col = ID, into = c("P", "Farm", "Stage", "Source"), sep = "\\.") %>%
  select(-P) %>%
  separate(col = Farm, into = c("Farm_ID", "Year")) %>%
  mutate(Farmtype = str_sub(Farm_ID, start = 2, end = 2),
         Farmtype = plyr::mapvalues(Farmtype, from = c("C", "O"), to = c("Cv", "Or"))) %>%
  select(Predator, Source, Mean, SD, `2.5%`, `50%`, `97.5%`, Farmtype, Stage, Year, Farm_ID)

model_out_spider_raw <- read.table("Output/Temp_IGP/JAGS/model_out_spider.txt", header = F, fill = TRUE)
model_out_spider_clean <- bind_cols(model_out_spider_raw[5:312, c(1:4, 7)], model_out_spider_raw[316:623, 2]) %>%
  `names<-`(c("ID", "Mean", "SD", "2.5%", "50%", "97.5%")) %>%
  mutate(Predator = "Spider") %>% 
  separate(col = ID, into = c("P", "Farm", "Stage", "Source"), sep = "\\.") %>%
  select(-P) %>%
  separate(col = Farm, into = c("Farm_ID", "Year")) %>%
  mutate(Farmtype = str_sub(Farm_ID, start = 2, end = 2),
         Farmtype = plyr::mapvalues(Farmtype, from = c("C", "O"), to = c("Cv", "Or"))) %>%
  select(Predator, Source, Mean, SD, `2.5%`, `50%`, `97.5%`, Farmtype, Stage, Year, Farm_ID)

model_out_ladybeetle_raw <- read.table("Output/Temp_IGP/JAGS/model_out_ladybeetle.txt", header = F, fill = TRUE)
model_out_ladybeetle_clean <- bind_cols(model_out_ladybeetle_raw[5:232, c(1:4, 7)], model_out_ladybeetle_raw[236:463, 2]) %>%
  `names<-`(c("ID", "Mean", "SD", "2.5%", "50%", "97.5%")) %>%
  mutate(Predator = "Ladybeetle") %>% 
  separate(col = ID, into = c("P", "Farm", "Stage", "Source"), sep = "\\.") %>%
  select(-P) %>%
  separate(col = Farm, into = c("Farm_ID", "Year")) %>%
  mutate(Farmtype = str_sub(Farm_ID, start = 2, end = 2),
         Farmtype = plyr::mapvalues(Farmtype, from = c("C", "O"), to = c("Cv", "Or"))) %>%
  select(Predator, Source, Mean, SD, `2.5%`, `50%`, `97.5%`, Farmtype, Stage, Year, Farm_ID)

model_out_clean <- bind_rows(model_out_predator_clean, model_out_spider_clean, model_out_ladybeetle_clean) %>% 
  mutate(Predator = ordered(Predator, levels = c("All", "Spider", "Ladybeetle")),
         Source = ordered(Source, levels = c("Rice_herb", "Tour_herb", "Detritivore", "Predator")),
         Farmtype = ordered(Farmtype, levels = c("Or", "Cv")),
         Stage = ordered(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>%
  mutate_at(.vars = c("Mean", "SD", "2.5%", "50%", "97.5%"), as.numeric) %>%
  arrange(Predator, Year, Stage, Farmtype, Source) %>%
  mutate(Farm_ID = plyr::mapvalues(Farm_ID, from = c("MO", "MC", "GO", "GC", "OO", "OC"), 
                                   to = c("MO1", "MC1", "LO1", "LC1", "SO1", "SC1")))

write_rds(model_out_clean, "Output/Temp_IGP/model_out_clean.rds")

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

ggsave("Output/Data_clean/Temp_IGP/Diet_proportion.tiff", width = 6, height = 7, dpi = 600)

### Both predators only
model_out_clean %>% 
  filter(Predator == "All") %>% 
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
  facet_grid(~ Farmtype, labeller = as_labeller(c("Or" = "Organic", "Cv" = "Conventional"))) + 
  # geom_text(data = label1, aes(x = x, y = y, label = Label), size = 5, color = "black", nudge_x = -0.5) +
  coord_cartesian(ylim = c(0, 1), clip = "off") +
  xlab("Crop stage") +
  ylab("Proportion (mean ± SE)") +
  scale_color_manual(values = c("#00BA38", "#619CFF", "#993300", "black"), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore", "Predator"), name = "") +
  scale_shape_manual(values = c(16, 15, 17, 18), labels = c("Rice herbivore", "Tourist herbivore", "Detritivore", "Predator"), name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  my_theme + 
  theme(panel.spacing.x = unit(0, "lines"),
        panel.spacing.y = unit(2, "lines"),
        legend.direction = "horizontal",
        legend.key.width = unit(0.6, "in"),
        legend.text = element_text(margin = margin(l = 4)),
        legend.position = "bottom",
        legend.key.spacing.x = unit(0.2, "in"),
        strip.background.y = element_rect(fill = "grey80"))

ggsave("./Output/Temp_IGP/Diet_proportion_both.tiff", width = 6, height = 4, dpi = 600)









