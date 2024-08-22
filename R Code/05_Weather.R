## -----------------------------------------------------------------------------
## Title: Daily mean temperature and precipitation of the study sites
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-19
##
## Description: 
## 1. Plot the daily mean temperature and precipitation of the study sites during 
##    the first rice growth season in year 2017, 2018, and 2019
##
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(magrittr)


# Import files -----------------------------------------------------------------
file_names <- list.files("./Data_raw/Weather_data", pattern = ".csv")
weather_list <- map(file_names, function(x){
  read_csv(paste0("./Data_raw/Weather_data/", x), 
           skip = 1,
           col_types = cols_only("ObsTime" = col_character(),
                                 "Temperature" = col_double(),
                                 "Precp" = col_double()))
})


# ggplot theme -----------------------------------------------------------------
my_theme <- 
  theme(# Axis
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 15, margin = margin(t = 10)),
    axis.title.y = element_text(size = 15, margin = margin(r = 8)),
    
    # Plot
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.margin = unit(c(0.1, 0.7, 0.3, 0.3), "cm"),
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
    legend.key.width = unit(1.2, "cm"),
    legend.key.size = unit(1.2, "line"),
    legend.key = element_blank(),
    legend.text = element_text(size = 10, margin = margin(0, 10, 0, 0)),
    legend.text.align = 0,
    legend.box.just = "center",
    legend.justification = c(0.5, 0.5),
    legend.title.align = 0.5,
    legend.background = element_rect(fill = "transparent"),
    
    # Facet strip
    strip.background = element_rect(fill = "transparent"),
    strip.text = element_text(size = 12, hjust = 0.5)
  )


############################### Code starts here ###############################

# 1. Daily mean temperature and precipitation of the study sites ---------------
### Organize the data
weather_data <- weather_list %>% 
  `names<-`(file_names) %>%
  bind_rows(.id = "File") %>%
  mutate(Year = str_extract(File, pattern = "(201.)"),
         Month = str_extract(File, pattern = "(?<=-)(0.)")) %>%
  select(Year, Month, Day = ObsTime, Temperature, Precipitation = Precp) %>%
  mutate(Date = paste(Month, Day, sep = "-")) %>%
  pivot_longer(cols = c(Temperature, Precipitation), 
               names_to = "Env_var", 
               values_to = "Values") %>%
  mutate(Env_var = factor(Env_var, levels = c("Temperature", "Precipitation"), ordered = T))


### Plot the data
# Axis limits for the panels
limits_fun <- function(x) {
  if (max(x) < 50) {
    c(14, 33)
  } else {
    c(-1, 210)
  }
}

# Axis breaks for the panels
breaks_fun <- function(x) {  # A function to specify the axis tick positions for the two panels
  if (max(x) < 50) {
    seq(15, 30, 5)
  } else {
    seq(0, 200, 50)
  }
}

# Plot
ggplot(data = weather_data, aes(x = Date, y = Values, color = Year, group = Year)) + 
  geom_point() + 
  geom_line() + 
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(breaks = breaks_fun,
                     limits = limits_fun,
                     expand = c(0, 0)) +
  scale_x_discrete(breaks = c("04-01", "05-01", "06-01", "07-01", "07-31"),
                   labels = c("April-1", "May-1", "June-1", "July-1", "July-31")) +
  facet_wrap(~Env_var, nrow = 2, 
             scales = "free_y",
             strip.position = "left", 
             labeller = as_labeller(c(Temperature = "Daily mean temperature (°C)", 
                                      Precipitation = "Daily precipitation (mm)"))) + 
  my_theme + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.title = element_blank())

ggsave("./Output/Figures/Weather.tiff", width = 7.5, height = 5.5, dpi = 600, device = "tiff")







