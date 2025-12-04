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

# Define Mercator projection ----------------------------------------------
target_crs_mercator <- "EPSG:3857"  # WGS 84 / Pseudo-Mercator


# Download and prepare basemap data --------------------------------------
world_countries <- ne_countries(scale = 'medium', returnclass = 'sf')
world_oceans <- ne_download(scale = 'medium', type = 'ocean', 
                            category = 'physical', returnclass = 'sf')
world_countries_mercator <- st_transform(world_countries, crs = target_crs_mercator)
world_oceans_mercator <- st_transform(world_oceans, crs = target_crs_mercator)


# Create graticules -------------------------------------------------------
graticules_mercator <- st_graticule(
  lat = seq(-90, 90, by = 20),
  lon = seq(-180, 180, by = 20),
  crs = st_crs(4326)
) |> st_transform(crs = target_crs_mercator)


# Create degree labels ----------------------------------------------------
create_degree_labels <- function() {
  lon_breaks <- seq(-180, 180, by = 20)
  lon_labels <- ifelse(lon_breaks == 0, "0°",
                       ifelse(lon_breaks == 180, "180°",
                              ifelse(lon_breaks > 0, paste0(lon_breaks, "°E"), 
                                     paste0(lon_breaks, "°W"))))
  
  lat_breaks <- seq(-80, 80, by = 20)
  lat_labels <- ifelse(lat_breaks == 0, "0°",
                       ifelse(lat_breaks > 0, paste0(lat_breaks, "°N"), 
                              paste0(lat_breaks, "°S")))
  lon_points_top <- st_sfc(
    lapply(lon_breaks, function(lon) st_point(c(lon, 85))),
    crs = 4326
  ) |> st_transform(crs = target_crs_mercator)
  
  lon_points_bottom <- st_sfc(
    lapply(lon_breaks, function(lon) st_point(c(lon, -85))),
    crs = 4326
  ) |> st_transform(crs = target_crs_mercator)
  
  lat_points_left <- st_sfc(
    lapply(lat_breaks, function(lat) st_point(c(-179, lat))),
    crs = 4326
  ) |> st_transform(crs = target_crs_mercator)
  
  lat_points_right <- st_sfc(
    lapply(lat_breaks, function(lat) st_point(c(179, lat))),
    crs = 4326
  ) |> st_transform(crs = target_crs_mercator)
  
  return(list(
    lon_labels_top = st_sf(geometry = lon_points_top, label = lon_labels),
    lon_labels_bottom = st_sf(geometry = lon_points_bottom, label = lon_labels),
    lat_labels_left = st_sf(geometry = lat_points_left, label = lat_labels),
    lat_labels_right = st_sf(geometry = lat_points_right, label = lat_labels)
  ))
}

degree_labels <- create_degree_labels()

mercator_bbox <- st_bbox(c(xmin = -20000000, ymin = -20000000, 
                           xmax = 20000000, ymax = 20000000), 
                         crs = target_crs_mercator)

# Create BASE MAP (Mercator) ----------------------------------------
map <- ggplot() +
  geom_sf(data = world_oceans_mercator, fill = ocean_color, color = NA) +
  
  geom_sf(data = world_countries_mercator, fill = country_color,
          color = country_border_color, linewidth = 0.5) +
  geom_sf(data = graticules_mercator, color = "white", linewidth = 0.3, alpha = 0.3) +

  coord_sf(xlim = c(mercator_bbox["xmin"], mercator_bbox["xmax"]),
           ylim = c(mercator_bbox["ymin"], mercator_bbox["ymax"])) +
  
  ggtitle("Mercator Projection") +
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

ggsave(paste0("Mercator", "map.png"), 
       width = 16, height = 10, dpi = 300, bg = background_color)
