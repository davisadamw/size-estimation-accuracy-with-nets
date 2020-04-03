library(tidyverse)
library(sf)
library(RANN)

# load data and remove really weird locations (size <= 2 or size >= 10k)
nets_size_2013 <- read_rds('Data/CA_business_sizes_2013.rds') %>% 
  filter(Emp13 >= 3) %>% 
  mutate(Longitude = -Longitude) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  st_transform(3310) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  st_drop_geometry()


charger_locations <- read_csv('Data/charger_paired_wld.csv') %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(3310) %>% 
  bind_cols(as_tibble(st_coordinates(.))) %>% 
  st_drop_geometry()

# nearest neighbor distance
neighbs <- nn2(select(charger_locations, X, Y),
               select(nets_size_2013, X, Y),
               k = 1) %>% 
  pluck('nn.dists')

nets_with_chargers <- nets_size_2013 %>%
  mutate(charger_distance = as.vector(neighbs))

nets_with_chargers %>% 
  group_by(within_500m = charger_distance <= 500) %>% 
  summarize(estabs = n(),
            emps   = sum(Emp13))

nets_with_chargers %>% 
  write_rds('Data/CA_business_sizes_2013_d2survey.rds', compress = 'gz')
