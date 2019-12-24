# Week 2 Notes


library(ggplot2)
library(maps)
library(tidyverse)


us_map <- map_data("state")
head(us_map, 3)

us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat)) +
  geom_point()

# path and group is needed rather than point to make nice plot
us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_path()

# Just Pa
us_map %>% 
  filter(region %in% c("pennsylvania")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_path()

# Add Color
us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black")

# Remove axis and labels
us_map %>% 
  filter(region %in% c("north carolina", "south carolina")) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") + 
  theme_void()

# Entire US
us_map %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") + 
  theme_void()

# World Map
world <- map_data("world") %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "thistle", color = "black") + 
  theme_void()





