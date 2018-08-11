library('dplyr')
library('ggplot2')
library('reshape2')
library('ggalt')
library('rgdal')
library('maptools')

gpclibPermit()

## all usa states
usa.states <- readOGR("cb_2017_us_state_500k/cb_2017_us_state_500k.shp")

usa.states2 <- usa.states %>%
  fortify(region = "NAME") %>%
  as_tibble() %>%
  left_join(usa.states@data, by = c("id" = "NAME"))

## all usa counties
usa.counties <- readOGR("cb_2017_us_county_500k/cb_2017_us_county_500k.shp")
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html

usa.counties2 <- usa.counties %>%
  fortify(region = "NAME") %>%
  as_tibble() %>%
  left_join(usa.counties@data, by = c("id" = "NAME"))

## baltimore
ggplot(data = usa.counties2 %>% filter(id == "Baltimore"), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")

## baltimore city
places.md <- readOGR("cb_2017_24_place_500k/cb_2017_24_place_500k.shp")
# https://www.census.gov/geo/maps-data/data/cbf/cbf_place.html

places.md2 <- places.md %>%
  fortify(region = "NAME") %>%
  as_tibble() %>%
  left_join(places.md@data, by = c("id" = "NAME"))

ggplot(data = places.md2 %>% filter(id == "Baltimore"), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")

ggplot() +
  coord_fixed(1.3) +
  geom_polygon(data = usa.states2 %>% filter(STATEFP == '24'), 
               mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "#e3e3e3") +
  geom_polygon(data = usa.counties2 %>% filter(id == "Baltimore"), 
               mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "#8c2a35") +
  geom_polygon(data = places.md2 %>% filter(id == "Baltimore"), 
               mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "#e1ac3b") +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        plot.background = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
  
ggsave('plot.png', width = 10, height = 6) 

# without state

ggplot() +
  coord_fixed(1.3) +
  # geom_polygon(data = usa.states2 %>% filter(STATEFP == '24'), 
  #              mapping = aes(x = long, y = lat, group = group), 
  #              color = "black", fill = "#e3e3e3") +
  geom_polygon(data = usa.counties2 %>% filter(id == "Baltimore"), 
               mapping = aes(x = long, y = lat, group = group), 
               color = "black", size = .2) +
  geom_polygon(data = places.md2 %>% filter(id == "Baltimore"), 
               mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "#9E7C0C", size = .2) +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.background = element_blank(), 
        plot.background = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave('baltimore.png', width = 6, height = 6)

# Los Angeles
ggplot(data = usa.counties2 %>% filter(id == "Los Angeles"), mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "white", fill = "#e3e3e3")

# Los Angeles city
places.ca <- readOGR("cb_2017_06_place_500k/cb_2017_06_place_500k.shp")
# https://www.census.gov/geo/maps-data/data/cbf/cbf_place.html

places.ca2 <- places.ca %>%
  fortify(region = "NAME") %>%
  as_tibble() %>%
  left_join(places.ca@data, by = c("id" = "NAME"))

ggplot() +
  coord_fixed(1.3) +
  # geom_polygon(data = usa.states2 %>% filter(STATEFP == '06'),
  #              mapping = aes(x = long, y = lat, group = group),
  #              color = "black", fill = "#e3e3e3") +
  geom_polygon(data = usa.counties2 %>% filter(id == "Los Angeles"),
               mapping = aes(x = long, y = lat, group = group),
               color = "black", size = .2) +
  geom_polygon(data = places.ca2 %>% filter(id == "Los Angeles"),
               mapping = aes(x = long, y = lat, group = group, fill = hole),
               color = "black", size = .2) + scale_fill_manual(values = c('#999999', '#d3effd')) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave('la.png', width = 6, height = 6)


# New York City

places.ny <- readOGR("cb_2017_36_place_500k/cb_2017_36_place_500k.shp")
# https://www.census.gov/geo/maps-data/data/cbf/cbf_place.html

places.ny <- places.ny %>%
  fortify(region = "NAME") %>%
  as_tibble() %>%
  left_join(places.ca@data, by = c("id" = "NAME"))

# this is WRONG! where the f is the east river?!?! 
ggplot() +
  coord_fixed(1.3) +
  geom_polygon(data = places.ny %>% filter(id == "New York"),
               mapping = aes(x = long, y = lat, group = group, fill = hole),
               color = "black", size = .2)

places.ny <- readOGR("nybb_18b/nybb.shp")
# https://www1.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page

places.ny <- places.ny %>%
  fortify(region = "BoroName") %>%
  as_tibble() %>%
  left_join(places.ny@data, by = c("id" = "BoroName"))

ggplot() +
  coord_fixed(1.3) +
  geom_polygon(data = places.ny,
               mapping = aes(x = long, y = lat, group = group, color = id),
               size = .4) +
theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave('nyc.png', width = 6, height = 6)


# dc ... water areas are a mess ugh

places.dc <- readOGR("tl_2017_us_state/tl_2017_us_state.shp")
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php

places.dc <- places.dc %>%
  fortify(region = "NAME") %>%
  as_tibble() %>%
  left_join(places.dc@data, by = c("id" = "NAME"))

area.water.dc <- readOGR("tl_2017_11001_areawater/tl_2017_11001_areawater.shp")

area.water.dc <- area.water.dc %>%
  fortify(region = "FULLNAME") %>%
  as_tibble() %>%
  left_join(area.water.dc@data, by = c("id" = "FULLNAME"))


ggplot() +
  coord_fixed(1.3) +
  geom_polygon(data = places.dc %>% filter(id == 'District of Columbia'),
               mapping = aes(x = long, y = lat, group = group),
               color = "black", size = .2) +
  geom_polygon(data = area.water.dc,
               mapping = aes(x = long, y = lat, group = group, fill = piece)) +
  scale_fill_manual(values = c('#d3effd', 'white', 'white',  'white'))+
  geom_polygon(data = area.water.dc %>% filter(id == 'Potomac Riv' & piece != 1),
               mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(data = area.water.dc %>% filter(id == 'Kingman Lk' & piece != 1),
               mapping = aes(x = long, y = lat, group = group), fill = '#d3effd') +
  geom_polygon(data = area.water.dc %>% filter(id == 'Georgetown Reservoir' & piece != 1),
               mapping = aes(x = long, y = lat, group = group), fill = '#d3effd') +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')


ggsave('dc.png', width = 6, height = 6)
