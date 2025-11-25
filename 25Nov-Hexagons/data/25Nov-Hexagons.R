
library(sf)
library(ggplot2)
library(magick)
library(grid)
library(tidyverse)
library(patchwork)

# reading the gpkg file of boundaries
boundary <- st_read("AdministrativeBoundary.gpkg", quiet = TRUE)

# reading the geojson file
hex <- st_read ("Spatial_Units-MCDA.geojson", quiet = TRUE)

# setting the CRS into Germany's EPSG
crs_mercator <- 25832

# defining the files into varibales
boundary <- st_transform(boundary, crs_mercator)
hex <- st_transform(hex, crs_mercator)

# identifying and locating the values of the layer
head(boundary)
unique(boundary$name)

#filtering the boundary based on Dresden value among other attributes
dresden_b <- boundary |>
  filter(name == "Dresden")

#Read logo
rbanism_logo <- image_read("Logo_Rbanism_ White.png")

# plotting the map
ggplot(data = hex) +
  geom_sf(data = dresden_b, fill = NA, color = "grey75",
          size = 1) +
  geom_sf(aes(fill = MCDA_Score, color = MCDA_Score)) +
  scale_fill_viridis_c(option = "magma" , name = "MCDA_Score") + 
  scale_color_viridis_c(option = "magma", name = "MCDA_Score") +
  theme(panel.background = element_rect(fill = "grey10"),
        plot.background  = element_rect(fill = "grey10"),
        panel.grid.major  = element_line(color = "gray50"),   
        panel.grid.minor  = element_line(color = "gray60"),
        axis.text = element_text(color = "white"),            
        axis.title = element_blank(),       
        plot.title = element_text(color = "white"),
        legend.background = element_rect(fill = "grey10", color = NA),
        legend.key = element_rect(fill = "grey10", color = NA),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")) +
  plot_annotation(
    caption = "#30DayMapChallenge — 'Hexagons' by Soroush Saffarzadeh, 2025. Data: OSM, GitHub.",
    theme = theme(
      plot.caption = element_text(color = "white", hjust = 1, size = 10),
      plot.background = element_rect(fill = "grey10", color = NA)
    )
  ) +
  labs(
    title = "MCDA analysis of streams in Dresden",
    subtitle = NA)

# plotting logo
grid.raster(rbanism_logo,
              x = 0.80, y=0.16,
              width = unit(45, "points"))

