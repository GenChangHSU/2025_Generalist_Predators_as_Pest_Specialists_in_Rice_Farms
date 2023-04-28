## -----------------------------------------------------------------------------
## Title: Bayesian Stable Isotope Mixing Model with MixSIAR
##
## Author: Gen-Chang Hsu
##
## Date: 2023-04-27
##
## Description: 
## 1. Assign arthropod families into different trophic guilds
## 2. Prepare consumer data for the mixing model
## 3. Prepare source data for the mixing model
## 4. Prepare C and N trophic discrimination factors
## 5. Run the mixing models using JAGS
## 6. Organize the raw mixing model outputs
## 
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(MixSIAR)


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

predator_data <- SID_all_clean %>%
  filter(Family %in% predator)

spider_data <- SID_all_clean %>%
  filter(Family %in% spider)

ladybeetle_data <- SID_all_clean %>%
  filter(Family %in% ladybeetle)

rice_herb_data <- SID_all_clean %>%
  filter(Family %in% rice_herb)

tour_herb_data <- SID_all_clean %>%
  filter(Family %in% tour_herb)

detritivore_data <- SID_all_clean %>%
  filter(Family %in% detritivore)


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

write.csv(mixture_predator, "Output/Data_clean/mixture_predator.csv", row.names = FALSE)
write.csv(mixture_spider, "Output/Data_clean/mixture_spider.csv", row.names = FALSE)
write.csv(mixture_ladybeetle, "Output/Data_clean/mixture_ladybeetle.csv", row.names = FALSE)

mix_siar_predator <- load_mix_data(filename = "Output/Data_clean/mixture_predator.csv",
                                   iso_names = c("d13C","d15N"),
                                   factors = c("Farm", "Stage"),
                                   fac_random = c(F, F),
                                   fac_nested = c(F, F),
                                   cont_effects = NULL)

mix_siar_spider <- load_mix_data(filename = "Output/Data_clean/mixture_spider.csv",
                                 iso_names = c("d13C","d15N"),
                                 factors = c("Farm", "Stage"),
                                 fac_random = c(F, F),
                                 fac_nested = c(F, F),
                                 cont_effects = NULL)

mix_siar_ladybeetle <- load_mix_data(filename = "Output/Data_clean/mixture_ladybeetle.csv",
                                     iso_names = c("d13C","d15N"),
                                     factors = c("Farm", "Stage"),
                                     fac_random = c(F, F),
                                     fac_nested = c(F, F),
                                     cont_effects = NULL)


# 3. Preparation of source data ------------------------------------------------
source <- list(rice_herb_data, tour_herb_data, detritivore_data) %>% 
  map(., function(x){
    select(x, d13C, d15N, Concd13C = C_conc, Concd15N = N_conc, Farm)
  }) %>%
  `names<-`(c("Rice_herb", "Tour_herb", "Detritivore")) %>%
  bind_rows(.id = "Source")

write.csv(source, "Output/Data_clean/source.csv", row.names = FALSE)

source_mix_siar_predator <- load_source_data(filename = "Output/Data_clean/source.csv",
                           source_factors = NULL,
                           conc_dep = TRUE,
                           data_type = "raw",
                           mix_siar_predator)

source_mix_siar_spider <- load_source_data(filename = "Output/Data_clean/source.csv",
                                             source_factors = NULL,
                                             conc_dep = TRUE,
                                             data_type = "raw",
                                             mix_siar_spider)

source_mix_siar_ladybeetle <- load_source_data(filename = "Output/Data_clean/source.csv",
                                           source_factors = NULL,
                                           conc_dep = TRUE,
                                           data_type = "raw",
                                           mix_siar_ladybeetle)



# 4. Preparation of C and N trophic discrimination factors ---------------------
### Diet-Dependent Discrimination Factor (DDDF) from Caut et al. (2009)
TDF_C_fun <- function(x){-0.113*x - 1.916}
TDF_N_fun <- function(x){-0.311*x + 4.065}

TDF <- source %>% 
  group_by(Source) %>%
  summarise(Meand13C = mean(TDF_C_fun(d13C)), SDd13C = sd(TDF_C_fun(d13C)),
            Meand15N = mean(TDF_N_fun(d15N)), SDd15N = sd(TDF_N_fun(d15N)))

write.csv(TDF, "Output/Data_clean/TDF.csv", row.names = F)

discr_mix_siar_predator <- load_discr_data(filename = "Output/Data_clean/TDF.csv", mix_siar_predator)
discr_mix_siar_spider <- load_discr_data(filename = "Output/Data_clean/TDF.csv", mix_siar_spider)
discr_mix_siar_ladybeetle <- load_discr_data(filename = "Output/Data_clean/TDF.csv", mix_siar_ladybeetle)


# 5. Run the mixing models using JAGS -------------------------------------------
### Write JAGS files
dr <- getwd()
setwd(dir = paste0(dr, "/Output/JAGS"))

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
setwd("./Output/JAGS")
options(max.print = 1000000)

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

setwd("../..")












# 6. Organize the raw mixing model outputs -------------------------------------
model_out_predator_raw <- read.table("Output/JAGS/model_out_predator.txt", header = F, fill = TRUE)
model_out_predator_clean <- bind_cols(model_out_predator_raw[5:286, c(1:4, 7)], model_out_predator_raw[290:571, 3]) %>%
  `names<-`(c("ID", "Mean", "SD", "2.5%", "50%", "97.5%")) %>%
  mutate(Predator = "All") %>% 
  separate(col = ID, into = c("P", "Farm", "Stage", "Source"), sep = "\\.") %>%
  select(-P) %>%
  separate(col = Farm, into = c("Farm_ID", "Year")) %>%
  mutate(Farmtype = str_sub(Farm_ID, start = 2, end = 2),
         Farmtype = plyr::mapvalues(Farmtype, from = c("C", "O"), to = c("Cv", "Or"))) %>%
  select(Predator, Source, Mean, SD, `2.5%`, `50%`, `97.5%`, Farmtype, Stage, Year, Farm_ID)

model_out_spider_raw <- read.table("Output/JAGS/model_out_spider.txt", header = F, fill = TRUE)
model_out_spider_clean <- bind_cols(model_out_spider_raw[5:262, c(1:4, 7)], model_out_spider_raw[266:523, 3]) %>%
  `names<-`(c("ID", "Mean", "SD", "2.5%", "50%", "97.5%")) %>%
  mutate(Predator = "Spider") %>% 
  separate(col = ID, into = c("P", "Farm", "Stage", "Source"), sep = "\\.") %>%
  select(-P) %>%
  separate(col = Farm, into = c("Farm_ID", "Year")) %>%
  mutate(Farmtype = str_sub(Farm_ID, start = 2, end = 2),
         Farmtype = plyr::mapvalues(Farmtype, from = c("C", "O"), to = c("Cv", "Or"))) %>%
  select(Predator, Source, Mean, SD, `2.5%`, `50%`, `97.5%`, Farmtype, Stage, Year, Farm_ID)

model_out_ladybeetle_raw <- read.table("Output/JAGS/model_out_ladybeetle.txt", header = F, fill = TRUE)
model_out_ladybeetle_clean <- bind_cols(model_out_ladybeetle_raw[5:175, c(1:4, 7)], model_out_ladybeetle_raw[179:349, 3]) %>%
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
         Source = ordered(Source, levels = c("Rice_herb", "Tour_herb", "Detritivore")),
         Farmtype = ordered(Farmtype, levels = c("Or", "Cv")),
         Stage = ordered(Stage, levels = c("Tillering", "Flowering", "Ripening"))) %>%
  mutate_at(.vars = c("Mean", "SD", "2.5%", "50%", "97.5%"), as.numeric) %>%
  arrange(Predator, Year, Stage, Farmtype, Source) %>%
  mutate(Farm_ID = plyr::mapvalues(Farm_ID, from = c("MO", "MC", "GO", "GC", "OO", "OC"), 
                                   to = c("MO1", "MC1", "LO1", "LC1", "SO1", "SC1")))

write_rds(model_out_clean, "Output/Data_clean/model_out_clean.rds")




