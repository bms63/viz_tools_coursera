# Week 2 Notes


library(ggplot2)
library(maps)
library(tidyverse)
library(viridis)
library(ggmap)
library(gridExtra)
library(choroplethr)
library(choroplethrMaps)


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

# Introduce  viridis color scheme
data(votes.repub)
head(votes.repub)
votes.repub %>%
  tbl_df() %>%
  mutate(state = rownames(votes.repub),
         state = tolower(state)) %>%
  right_join(us_map, by = c("state" = "region")) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = `1976`)) +
  geom_polygon(color = "black") + 
  theme_void() + 
  scale_fill_viridis(name = "Republican\nvotes (%)")

# Serial Podcast
library(readr)
serial <- read_csv(paste0("https://raw.githubusercontent.com/",
                          "dgrtwo/serial-ggvis/master/input_data/",
                          "serial_podcast_data/serial_map_data.csv"))
head(serial, 3)

# Converts x,y variable to lat and longitue
serial <- serial %>%
  mutate(long = -76.8854 + 0.00017022 * x,
         lat  = 39.23822 + 1.371014e-04 * y,
         tower = Type == "cell-site")
serial %>%
  slice(c(1:3, (n() - 3):(n())))

# Getting map of baltimore county
maryland <- map_data('county', region = 'maryland')
head(maryland)
baltimore <- maryland %>%
  filter(subregion %in% c("baltimore city", "baltimore"))
head(baltimore, 3)

# Base map of two counties "baltimore city", "baltimore"
ggplot(baltimore, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "lightblue", color = "black") + 
  theme_void()

ggplot(baltimore, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "lightblue", color = "black") + 
  geom_point(data = serial, aes(group = NULL, color = tower)) + 
  theme_void() + 
  scale_color_manual(name = "Cell tower", values = c("black", "red"))

# Google API Maps - Had to do some Enable stuff on Google Cloud Services
beijing <- get_map("Beijing", zoom = 12)
ggmap(beijing)

philly <- get_map("Philadelphia", zoom = 15)
ggmap(philly)

# Maps of Philly side by side
map_1 <- get_map("Philadelphia", zoom = 12,
                 source = "google", maptype = "terrain") %>%
  ggmap(extent = "device")

map_2 <- get_map("Philadelphia", zoom = 12,
                 source = "stamen", maptype = "watercolor") %>%
  ggmap(extent = "device")

map_3 <- get_map("Philadelphia", zoom = 12,
                 source = "google", maptype = "hybrid") %>%
  ggmap(extent = "device")


grid.arrange(map_1, map_2, map_3, nrow = 1) 

# Putting Serial data onto a Google Map
get_map("Baltimore County", zoom = 10, 
        source = "stamen", maptype = "toner") %>%
  ggmap() + 
  geom_polygon(data = baltimore, aes(x = long, y = lat, group = group),
               color = "navy", fill = "lightblue", alpha = 0.2) + 
  geom_point(data = serial, aes(x = long, y = lat, color = tower)) + 
  theme_void() + 
  scale_color_manual(name = "Cell tower", values = c("black", "red"))

# Counties and States

data(df_pop_county)
df_pop_county %>% slice(1:3)

#entire country
county_choropleth(df_pop_county)

# Two States
county_choropleth(df_pop_county, state_zoom = c("colorado", "wyoming", "pennsylvania"))

# Google reference map
county_choropleth(df_pop_county, state_zoom = c("north carolina"),
                  reference_map = TRUE)

library(readr)
floyd_events <- read_csv(paste0("https://raw.githubusercontent.com/coop711/r_programming_2/master/data/floyd_events.csv")) 
floyd_events %>% slice(1:3)

floyd_data <- floyd_events %>% 
  group_by(fips) %>%
  dplyr::summarize(n_events = n()) %>%
  mutate(fips = as.numeric(fips)) %>%
  dplyr::rename(region = fips, 
                value = n_events) 

county_choropleth(floyd_data, state_zoom = c("north carolina", "virginia"),
                    reference_map = TRUE)
  