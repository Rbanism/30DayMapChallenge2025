library(sf)
library(tidyverse)
library(magick)
library(magrittr)
library(grid)

rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')

# Data from: https://doi.org/10.5281/zenodo.11196161
# Datapaper: https://journals.openedition.org/cybergeo/41791

roads1810 <- read.csv("data/roads_1810.csv")
relays1810 <- read.csv("data/relays_1810.csv")

relays <- st_as_sf(relays1810, 
                   coords = c("COORDINATES_2154_X", "COORDINATES_2154_Y"))
st_crs(relays) <- st_crs(postal_roads)


xyrelays_A <- relays1810 |> 
  mutate(IDREL_A = IDREL,
         COORDINATES_2154_X_A = COORDINATES_2154_X,
         COORDINATES_2154_Y_A = COORDINATES_2154_Y) |>
  select(IDREL_A, COORDINATES_2154_X_A,COORDINATES_2154_Y_A)

xyrelays_B <- relays1810 |> 
  mutate(IDREL_B = IDREL,
         COORDINATES_2154_X_B = COORDINATES_2154_X,
         COORDINATES_2154_Y_B = COORDINATES_2154_Y) |>
  select(IDREL_B, COORDINATES_2154_X_B,COORDINATES_2154_Y_B)

roads <- roads1810 |>
  left_join(xyrelays_A, by="IDREL_A") |>
  left_join(xyrelays_B, by="IDREL_B" ) 
  


############## Bit of code adapted from
#https://www.adventuremeng.com/post/create-sf-lines-from-start-and-end-coordinates/
library(sp)

###The table above is imported as "od" here.

od_coords_to_sf <- function(df) {
  #prepare the table from wide to long
  part1 <-df[,c(2,11,12)]  %>%
    mutate(shape_pt_sequence =1) %>%
    rename(shape_pt_lat = COORDINATES_2154_Y_A,
           shape_pt_lon = COORDINATES_2154_X_A)
  
  part2 <-df[,c(2,13,14)]  %>%
    mutate(shape_pt_sequence =2) %>%
    rename(shape_pt_lat=COORDINATES_2154_Y_B,
           shape_pt_lon=COORDINATES_2154_X_B)
  
  allpart <- rbind(part1, part2)
  allpart<-allpart %>% arrange(colnames(df[1]))
  m <- as.matrix(allpart[order(allpart$shape_pt_sequence),
                         c("shape_pt_lon", "shape_pt_lat")])
  m <- sf::st_linestring(m)
  shape_linestrings <- sf::st_sfc(m, crs = 2154)
  shapes_sf <- sf::st_sf(IDROAD =allpart$IDROAD, geometry = shape_linestrings)
  shapes_sf <-shapes_sf %>% distinct(IDROAD, .keep_all = TRUE)
  return(shapes_sf)
}

all_segment <- data.frame()

for (i in 1:nrow(roads)){
  one_seg <- roads[i,] %>% od_coords_to_sf()
  all_segment <- rbind(all_segment, one_seg)
}

plot(all_segment)

#####################

all_roads <- left_join(all_segment,roads)

fr <- st_read("data/fr.json") |>
  st_union() 

fr_lambert <- st_transform(fr, crs = 2154)



ggplot() +
  geom_sf(data=fr, fill = "gold", col = "white") +
  geom_sf(data = all_roads, aes(col = MAX_SLOPE)) +
  scale_colour_gradient(high="darkblue", low = "white") +
  theme_minimal() +
  ggtitle("#30DayMapChallenge. Lines\nPostal horse roads in 1810 France") +
  xlab(str_wrap("#30DayMapChallenge. Clémentine Cottineau-Mugadza, 2025. Source: Verdier, N., Giraud, T., Mimeur, C., & Bretagnolle, A. (2024). Postal horse relays and roads in France, from the 17th to the 19th centuries.Zenodo. https://doi.org/10.5281/zenodo.11196161")) +
  coord_sf(crs = sf::st_crs(2154)) +
  guides(col=guide_legend(title="Maximum slope (m)"), legend.position="bottom") +
  theme(axis.title.x=element_text(family="Fira Code", hjust = 0, size=6,colour = "#00A99D"),
        plot.title = element_text(family="Fira Code", size=14, colour = "#00A99D"),
        legend.title = element_text(family="Fira Code", size=10, colour = "#93278F"),
        legend.text =element_text(family="Fira Code", size=8, colour = "#93278F"),
        axis.line =  element_blank(),
        axis.text =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())



grid.raster(rbanism_logo,
            x = 0.88, y=0.1,
            width = unit(100, "points"))
ggsave(filename = "Lines.png",
       width = 8, height = 8, dpi = 300)

