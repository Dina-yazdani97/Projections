#Package Loading --------------------------------------------------------
library(raster)
library(rasterVis)
library(rworldxtra)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(extrafont)

# --- USER CAN CHANGE COLORS HERE ----------------------------------------
ocean_color <- "#000000"   
country_color <- "#C1CDCD"  
country_border_color <- "#EE3B3B"  
background_color <- "#000000" 

# Define Eckert projection ----------------------------------------------
target_crs_Eckert <- "ESRI:54012" #Eckert IV


# Download and prepare basemap data --------------------------------------
world_countries <- ne_countries(scale = 'medium', returnclass = 'sf')
world_oceans <- ne_download(scale = 'medium', type = 'ocean', 
                            category = 'physical', returnclass = 'sf')
world_countries_Eckert <- st_transform(world_countries, crs = target_crs_Eckert)
world_oceans_Eckert <- st_transform(world_oceans, crs = target_crs_Eckert)


# Create graticules -------------------------------------------------------
graticules_Eckert <- st_graticule(
  lat = seq(-90, 90, by = 10),
  lon = seq(-180, 180, by = 10),
  crs = st_crs(4326)
) |> st_transform(crs = target_crs_Eckert)


# Create degree labels ----------------------------------------------------
create_degree_labels <- function() {
  lon_breaks <- seq(-180, 180, by = 20)
  lon_labels <- ifelse(lon_breaks == 0, "0°",
                       ifelse(lon_breaks == 180, "180°",
                              ifelse(lon_breaks > 0, paste0(lon_breaks, "°E"), 
                                     paste0(lon_breaks, "°W"))))
  
  lat_breaks <- seq(-80, 80, by = 10)
  lat_labels <- ifelse(lat_breaks == 0, "0°",
                       ifelse(lat_breaks > 0, paste0(lat_breaks, "°N"), 
                              paste0(lat_breaks, "°S")))
  
  lon_points_top <- st_sfc(
    lapply(lon_breaks, function(lon) st_point(c(lon, 85))),
    crs = 4326
  ) |> st_transform(crs = target_crs_Eckert)
  
  lon_points_bottom <- st_sfc(
    lapply(lon_breaks, function(lon) st_point(c(lon, -85))),
    crs = 4326
  ) |> st_transform(crs = target_crs_Eckert)
  
  lat_points_left <- st_sfc(
    lapply(lat_breaks, function(lat) st_point(c(-179, lat))),
    crs = 4326
  ) |> st_transform(crs = target_crs_Eckert)
  
  lat_points_right <- st_sfc(
    lapply(lat_breaks, function(lat) st_point(c(179, lat))),
    crs = 4326
  ) |> st_transform(crs = target_crs_Eckert)
  
  return(list(
    lon_labels_top = st_sf(geometry = lon_points_top, label = lon_labels),
    lon_labels_bottom = st_sf(geometry = lon_points_bottom, label = lon_labels),
    lat_labels_left = st_sf(geometry = lat_points_left, label = lat_labels),
    lat_labels_right = st_sf(geometry = lat_points_right, label = lat_labels)
  ))
}

degree_labels <- create_degree_labels()


# Create BASE MAP (Eckert only) ----------------------------------------
map <- ggplot() +
  geom_sf(data = world_oceans_Eckert, fill = ocean_color, color = NA) +

  geom_sf(data = world_countries_Eckert, fill = country_color,
          color = country_border_color, linewidth = 0.5) +

  geom_sf(data = graticules_Eckert, color = "white", linewidth = 0.3, alpha = 0.3) +
  
  geom_sf_text(data = degree_labels$lon_labels_top, aes(label = label),
               color = "white", size = 3, family = "Times New Roman",
               fontface = "bold", nudge_y = 500000) +
  geom_sf_text(data = degree_labels$lon_labels_bottom, aes(label = label),
               color = "white", size = 3, family = "Times New Roman",
               fontface = "bold", nudge_y = -500000) +
  geom_sf_text(data = degree_labels$lat_labels_left, aes(label = label),
               color = "white", size = 3, family = "Times New Roman",
               fontface = "bold", nudge_x = -800000) +
  geom_sf_text(data = degree_labels$lat_labels_right, aes(label = label),
               color = "white", size = 3, family = "Times New Roman",
               fontface = "bold", nudge_x = 800000) +
  
  ggtitle("Eckert IV") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "white"),
    plot.title = element_text(hjust = 0.5, size = 35, face = "bold", color = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = background_color, color = NA),
    panel.background = element_rect(fill = background_color, color = NA)
  )

ggsave(paste0("Eckert IV",  "map.png"), 
       width = 16, height = 10, dpi = 300, bg = background_color)