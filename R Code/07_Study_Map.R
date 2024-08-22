## -----------------------------------------------------------------------------
## Title: Create a map of the study sites
##
## Author: Gen-Chang Hsu
##
## Date: 2024-08-20
##
## Description:
## 1. Create a main map of the study farms
## 2. Create an inset map of Taiwan
## 3. Combine the two maps
## 
## -----------------------------------------------------------------------------
set.seed(123)


# Libraries --------------------------------------------------------------------
library(tidyverse)
library(ggmap)
library(ggthemes)
library(sf)
library(ggsn)
library(patchwork)


# Import files -----------------------------------------------------------------


############################### Code starts here ###############################

# 1. Create a main map of the study farms --------------------------------------
### Farm coordinates
coord <- structure(list(X1 = structure(c(V1 = 7L, V2 = 8L, V3 = 9L, V4 = 10L, V5 = 11L, V6 = 12L, V7 = 1L, V8 = 2L, V9 = 3L, V10 = 4L, V11 = 5L, V12 = 6L, V13 = 13L, V14 = 14L), 
                                       .Label = c("LC-1", "LC-2", "LC-3", "LO-1", "LO-2", "LO-3", "MC-1", "MC-2", "MC-3", "MO-1", "MO-2", "MO-3", "SC-1", "SO-1"), class = "factor"), 
                        X2 = structure(c(V1 = 9L, V2 = 8L, V3 = 11L, V4 = 10L, V5 = 7L, V6 = 12L, V7 = 5L, V8 = 2L, V9 = 4L, V10 = 6L, V11 = 1L, V12 = 3L, V13 = 13L, V14 = 14L), 
                                       .Label = c("24.363708, 120.708252", "24.364497, 120.707921", "24.368548, 120.705008", "24.369643, 120.709031", "24.378030, 120.697275", "24.381196, 120.704561", "24.4056, 120.7217",
                                                  "24.406726, 120.721659", "24.410743, 120.716231", "24.412337, 120.715802", "24.421391, 120.688577", "24.4221, 120.6899", "24.457978, 120.656940", "24.458515, 120.655713"), class = "factor")), 
                   class = "data.frame", row.names = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14"))

coord <- coord %>% 
  rename(Farm = X1 , Coord = X2) %>% 
  separate(Coord, c("Lat", "Long"), sep = ", ") %>%
  mutate(Farmtype = substring(Farm, 2, 2)) %>%
  mutate(Long = as.numeric(Long)) %>%
  mutate(Lat = as.numeric(Lat))

### Main map
register_stadiamaps("69ccb61d-8886-4e2a-9ed1-fd5c67dab17b", write = TRUE)
bbox <- make_bbox(lon = coord$Long, lat = coord$Lat, f = 0.2)
map_dat <- get_stadiamap(bbox, zoom = 15, maptype = "stamen_terrain") 

main_map <- ggmap(map_dat) +
  geom_point(data = coord, aes(x = Long, y = Lat, color = Farmtype, shape = Farmtype), size = 3) +
  scale_color_manual(values = c("orange2", "darkgreen"), guide = guide_legend(reverse = TRUE), name = "", labels = c("Conventional", "Organic")) +
  scale_shape_manual(values = c(16, 17), guide = guide_legend(reverse = TRUE), name = "", labels = c("Conventional", "Organic")) +
  annotate(geom = "text", x = 120.66, y = 24.411, label = "N", fontface = 2, size = 4.5) +
  theme_map() +
  theme(plot.margin = rep(unit(0.05,"null"), 4),
        panel.background = element_rect(color = "black"),
        legend.position = c(0.95, 0.95),
        legend.spacing.x = unit(0.15, "cm"),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.key.size = unit(0.1, "line"),
        legend.key = element_blank(),
        legend.text = element_text(size = 8),
        legend.box.just = "center",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black", fill = "grey90", size = 0.5, linetype = "solid")) +
        scalebar(x.min = 120.65, x.max = 120.67, y.min = 24.395, y.max = 24.405,
                dist = 1, dist_unit = "km", transform = T, model = "WGS84", height = 0.2, st.dist = 0.2, st.size = 2.5) +
        coord_equal() +
        north(x.min = 120.663, x.max = 120.665, y.min = 24.42, y.max = 24.43, symbol = 10, scale = 5)


# 2. Inset map of Taiwan --------------------------------------------------
taiwan_boundary <- st_read("./Data_raw/mapdata/COUNTY_MOI_1080617.shp", quiet = T)

inset_map <- ggplot() + 
  geom_sf(data = taiwan_boundary) + 
  coord_sf() +
  labs(title = "Taiwan") +
  scale_x_continuous(limits = c(119, 122.5), breaks = c(119, 120, 121, 122)) +
  scale_y_continuous(limits = c(21.5, 25.5), breaks = c(22, 23, 24, 25)) + 
  theme(axis.text.x = element_text(size = 5, color = "black"),
        axis.text.y = element_text(size = 5, color = "black", angle = 90, hjust = 0.5),
        axis.text.y.right = element_text(size = 5, angle = -90, hjust = 0.5),
        axis.title.x = element_text(size = 6, margin = margin(t = 0)),
        axis.title.y = element_text(size = 6, margin = margin(r = 0)),
        plot.title = element_text(hjust = 0.5, size = 10, margin = margin(b = 0)),
        plot.margin = rep(unit(0.05, "null"), 4),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(colour = "black"),
        panel.border = element_rect(colour = "black", fill = "transparent", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = c(0, 1.05),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.width = unit(0.7, "cm"),
        legend.key.size = unit(1.2, "line"),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        legend.box.just = "center",
        legend.justification = c(-0.2, 1.2),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "transparent", size = 0.5, linetype = "solid", colour = "transparent")) +
  annotate(geom = "rect", xmin = 120.5, xmax = 120.9, ymin = 24.3, ymax = 24.6, col = "red", lwd = 1, fill = "transparent")


# 3. Combine the two maps -------------------------------------------------
main_map + inset_element(inset_map, 0.01, 0, 0.4, 0.35)

ggsave("./Output/Figures/Map.tiff", width = 6.5, height = 4.5, dpi = 600, device = "tiff")


