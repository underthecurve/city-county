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

# maryland
ggplot(data = usa.states2 %>% filter(STATEFP == '24'), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")


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

# California
ggplot(data = usa.states2 %>% filter(STATEFP == '06'), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")

## to be added
# # Los Angeles
# ggplot(data = usa.counties2 %>% filter(id == "Los Angeles"), mapping = aes(x = long, y = lat, group = group)) + 
#   coord_fixed(1.3) + 
#   geom_polygon(color = "white", fill = "#e3e3e3")
#   
# # Los Angeles city
# places.ca <- readOGR("cb_2017_06_place_500k/cb_2017_06_place_500k.shp")
# # https://www.census.gov/geo/maps-data/data/cbf/cbf_place.html
# 
# places.ca2 <- places.ca %>%
#   fortify(region = "NAME") %>%
#   as_tibble() %>%
#   left_join(places.ca@data, by = c("id" = "NAME"))
# 
# ggplot(data = places.ca2 %>% filter(id == "Los Angeles"), mapping = aes(x = long, y = lat, group = group, fill = hole)) + 
#   coord_fixed(1.3) + 
#   geom_polygon(color = "white")
# 
# ggplot() +
#   coord_fixed(1.3) +
#   geom_polygon(data = usa.states2 %>% filter(STATEFP == '06'), 
#                mapping = aes(x = long, y = lat, group = group), 
#                color = "black", fill = "#e3e3e3") +
#   geom_polygon(data = usa.counties2 %>% filter(id == "Los Angeles"), 
#                mapping = aes(x = long, y = lat, group = group), 
#                color = "black", fill = "#8c2a35") +
#   geom_polygon(data = places.ca2 %>% filter(id == "Los Angeles"), 
#                mapping = aes(x = long, y = lat, group = group), 
#                color = "black", fill = "#e1ac3b") +
#   theme(axis.line = element_blank(), 
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(), 
#         axis.ticks = element_blank(), 
#         axis.title.x = element_blank(), 
#         axis.title.y = element_blank(),
#         panel.background = element_blank(), 
#         plot.background = element_blank(),  
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) 
# 
# ggsave('plot.png', width = 10, height = 6)



