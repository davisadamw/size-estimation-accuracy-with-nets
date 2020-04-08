library(tidyverse)
library(sf)
library(RANN)

# load data and remove really weird locations (size <= 2 or size >= 10k)
nets_size_2013 <- read_rds('Data/CA_business_sizes_2013.rds') %>% 
  mutate(Longitude = -Longitude) %>% 
  filter(Emp13 >= 3,
         between(Longitude, -125, -113),
         between(Latitude, -32.5, 42.1)) %>% 
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
  write_rds('Data/CA_business_sizes_2013_d2survey.rds', compress = 'gz')

nets_with_chargers <- read_rds('Data/CA_business_sizes_2013_d2survey.rds')


nets_with_chargers %>% 
  transmute(estabs = 1,
            emps   = Emp13,
            within_100  = charger_distance <= 100,
            within_500  = charger_distance <= 500,
            within_1000 = charger_distance <= 1000,
            within_5000 = charger_distance <= 5000) %>%
  pivot_longer(starts_with('within'),
               names_to = c('.value', 'distance'),
               names_sep = '_',
               names_ptypes = list(distance = integer())) %>%
  group_by(distance, within) %>% 
  summarize_all(sum) %>% 
  mutate_at(vars(estabs, emps), ~./sum(.)) %>% 
  filter(within) %>% 
  ungroup()


#nets_with_chargers <- read_rds('Data/CA_business_sizes_2013_d2survey.rds')
nets_with_chargers %>% 
  ggplot(aes(x = charger_distance)) +
  geom_histogram() + 
  scale_x_log10('Distance to Survey Point, m', labels = scales::comma) +
  scale_y_continuous('Businesses', labels = scales::comma, expand = c(0,0)) +
  expand_limits(y = 155000) +
  theme_bw()



