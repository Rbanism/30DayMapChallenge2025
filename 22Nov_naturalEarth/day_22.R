library(ggplot2)
library(maps)
library(mapproj)
library(patchwork)
library(magick)
library(grid)
library(sf)
library(ggrepel)
library(dplyr)

# Logo
logo_path <- "C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/Logo_Rbanism_ Blue.png"
logo <- image_read(logo_path)


# Data
water  <- st_read("C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/BATHYMETRY_WORLD.gpkg")
coast  <- st_read("C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/ne_50m_coastline/ne_50m_coastline.shp")
ports  <- st_read("C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/ne_10m_ports/ne_10m_ports.shp")
cities <- st_read("C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/ne_50m_populated_places_simple/ne_50m_populated_places_simple.shp")
sea_names <- st_read("C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/sea_names_pt.gpkg")
box    <- st_read("C:/Users/Roger M Y/Desktop/30_d_m_c/dia_22/box.gpkg")


# Ensure ALL layers share the same CRS (CRS of the box)
target_crs <- st_crs(box)

water  <- st_transform(water,  target_crs)
coast  <- st_transform(coast,  target_crs)
ports  <- st_transform(ports,  target_crs)
cities <- st_transform(cities, target_crs)
sea_names <- st_transform(sea_names, target_crs)


# Bounding box
box1 <- st_bbox(box)


# Convert ports to data frame with coordinates
ports_df <- cbind(ports, st_coordinates(ports))
sea_names_df <- cbind(sea_names, st_coordinates(sea_names))

# Filter ports inside bounding box
ports_df <- ports_df %>%
  filter(X >= box1$xmin & X <= box1$xmax &
           Y >= box1$ymin & Y <= box1$ymax)
sea_names_df <- sea_names_df %>%
  filter(X >= box1$xmin & X <= box1$xmax &
           Y >= box1$ymin & Y <= box1$ymax)


# Base map
base_map <- ggplot() +
  geom_sf(data = water, color = NA, fill = "#163838", linewidth = NA, alpha = 0.2) +
  geom_sf(data = coast, color = "#a00000", linewidth = 0.1) +
  geom_sf(data = cities, aes(size = pop_max), color = "#a00000", alpha = 0.4, shape = 15) +
  scale_size_area(max_size = 16) +
  geom_point(data = ports_df, aes(x = X, y = Y),
             shape = 23, size = 1, fill = "white", color = "#a00000", stroke = 0.5) +
  # Labels
  geom_text_repel(data = ports_df, aes(x = X, y = Y, label = name),
                  color = "#a00000", size = 1.2,
                  min.segment.length = 0,
                  box.padding = 0.1,
                  point.padding = 0.1,
                  segment.color = NA,
                  nudge_y = 1) +
  geom_text_repel(data = sea_names_df, aes(x = X, y = Y, label = name),
                  color = "#a00000", alpha = .5, size = 3,
                  min.segment.length = 0,
                  box.padding = 0.2,
                  point.padding = 0.1,
                  segment.color = NA,
                  nudge_y = 1) +
  # Apply bounding box
  coord_sf(
    xlim = c(box1$xmin, box1$xmax),
    ylim = c(box1$ymin, box1$ymax),
    expand = FALSE
  )


# Main plot
p <- base_map +
  plot_annotation(
    title = "Ports of the Mediterranean Sea",
    subtitle = "Bathymetry and Geografic Names of the Sea",
    caption = "#30DayMapChallenge · Roger Marin de Yzaguirre, 2025",
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(color = "#a00000", size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(color = "#a00000", size = 12, hjust = 0.5),
      plot.caption = element_text(color = "#a00000", size = 9, hjust = 0.5)
    )
  ) +
  theme_void() +                  
  theme(legend.position = "none") 


# Save intermediate plot
out_path <- "C:/Users/Roger M Y/Desktop/30_d_m_c/22nov.png"
ggsave(out_path, p, width = 11, height = 7, dpi = 300)

# Add logo
final_plot <- image_read(out_path)
logo_small <- image_scale(logo, "250x250")

# Logo position
final <- image_composite(final_plot, logo_small, offset = "+80+1780")

# Save final
image_write(final, "C:/Users/Roger M Y/Desktop/30_d_m_c/22nov_logo.png")

