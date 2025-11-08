# Attach packages ----
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(giscoR)
library(readxl)
library(osmdata)
library(grid)
library(png)
library(cowplot)

# Load Rbanism logo ----
rbanism_logo_path <- "8Nov-Urban/Logo_Rbanism_ Black.png"
rbanism_logo_png <- readPNG(rbanism_logo_path)
img_dim <- dim(rbanism_logo_png)
asp <- img_dim[1] / img_dim[2]
rbanism_logo <- rasterGrob(rbanism_logo_png,
                           x = unit(1.10, "npc"),
                           y = unit(0.20, "npc"),
                           width = unit(0.12, "npc"),
                           height = unit(0.12 * asp, "npc"),
                           just = c("right", "top"),
                           interpolate=TRUE,
                           default.units = "npc")

# Load data ----
## Get EEA grid
grid_gelderland_base <- st_read("8Nov-Urban/data/EU_grid_NL.gpkg")
grid_gelderland_pu <- st_read("8Nov-Urban/data/NL_cells_PU.gpkg")

## Get data
data <- read_csv(
  "8Nov-Urban/data/NL_PU_amenities_grid_summaries_all_types_all_amenities.csv"
)

## Get OSM data for plot
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))
osm_natural_water <- opq(bbox = "Gelderland, The Netherlands") |>
  add_osm_feature("natural", "water") |>
  osmdata_sf()

osm_natural_water_grid <- osm_natural_water$osm_polygons |>
  st_transform(st_crs(grid_gelderland_base)) |> 
  st_intersection(st_union(grid_gelderland_base))

osm_water_river <- opq(bbox = "Gelderland, The Netherlands") |>
  add_osm_feature("water", "river") |>
  osmdata_sf()

osm_water_river_grid <- osm_water_river$osm_polygons |>
  st_transform(st_crs(grid_gelderland_base)) |> 
  st_intersection(st_union(grid_gelderland_base))

osm_water_river_mp_grid <- osm_water_river$osm_multipolygons |>
  st_make_valid() |> 
  st_transform(st_crs(grid_gelderland_base)) |> 
  st_intersection(st_union(grid_gelderland_base))

osm_water_river_lines_grid <- osm_water_river$osm_lines |>
  st_transform(st_crs(grid_gelderland_base)) |> 
  st_intersection(st_union(grid_gelderland_base))

## Take top 25% and count how many of the categories are present in each
## cluster combination
data_filtered <- data |>
  filter(bucket == "Top 25% (>= Q3)") |> 
  group_by(C_DC_5, C_SC_9) |>
  # count how many times `share` is above 10%
  mutate(count = sum(share > 10))

## Join data with the PU grid
grid_gelderland_pu_joined <- grid_gelderland_pu |> 
  left_join(data_filtered, by = c("DC_5" = "C_DC_5", "SC_9" = "C_SC_9"))

# Create map ----
## Create grid centroids
grid_gelderland_pu_joined_centroids <- st_centroid(grid_gelderland_pu_joined)

## Use that count as a point size using the grid centroids for each occurrence
## of cluster combinations in the map
p <- grid_gelderland_pu_joined_centroids |> 
  ggplot() +
  geom_sf(data = osm_natural_water_grid, fill = "#0091ff", color = NA) +
  geom_sf(data = osm_water_river_grid, fill = "#0091ff", color = NA) +
  geom_sf(data = osm_water_river_mp_grid, fill = "#0091ff", color = NA) +
  # geom_sf(data = osm_water_river_lines_grid, color = "#0091ff", linewidth = 0.1) +
  geom_sf(data = grid_gelderland_base, color = "grey", lwd = 0.1, fill = NA) +
  geom_sf(aes(size = count), shape = 15) +
  scale_size_continuous(range = c(1, 3), name = "Count of types\nof amenities",
                        breaks = 0:9) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#fffee8", colour = NULL),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(hjust = 0.063, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.118),
    plot.caption = element_text(hjust = 0.15, size = 9),
    legend.justification = c(0.95, 0.95)
  ) + 
  labs(
    title = "Periurban Intensities",
    subtitle = paste0("Diversity of Amenities in Peripheral ",
                      "Spatial-Demographic Types in Gelderland"),
    caption = paste0("#30DayMapChallenge. Birgit Hausleitner & Claudiu Forgaci",
                     ", 2025. Data: EEA grid 1km and InPUT project data.")
  )
p

# Save map ----
out <- ggdraw() +
  draw_plot(p) +
  draw_image(rbanism_logo_path, x = 0.91, y = 0.25,
             width = 0.12, height = 0.12 * asp, hjust = 1, vjust = 1)
ggsave("8Nov-Urban/Urban.png", out, width = 10, height = 8, dpi = 300)
