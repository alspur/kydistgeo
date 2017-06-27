# script to clean raw ky district data

# load packages
library(tidyverse)
library(sf)

# read shapefile
districts <- read_sf("raw_data/ky_dist.shp")

# change map projection
# remove districts that no longer/never existed
# reformat district name column
# remove all cols other than name and geometry
ky_dist_geo <- st_transform(districts,
                                 crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
  filter(!stringr::str_detect(NAME, "DISTRICT NOT DEFINED")) %>%
  mutate(dist_name = stringr::str_to_title(NAME)) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              " School District", "")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              " Dependent Schools", "")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              "Ft Thomas", "Fort Thomas")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                         "Walton Verona",
                                         "Walton-Verona")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              "Larue", "LaRue")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              "Mccracken", "McCracken")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              "Mccreary", "McCreary")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              "Mclean", "McLean")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                         "Raceland",
                                         "Raceland-Worthington")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                         "Harrodsburg Independent",
                                         "Mercer County")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                         "Providence Independent",
                                         "Webster County")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                         "Monticello Independent",
                                         "Wayne County")) %>%
  select(-ID, -COUNTY, -UNIFIED, -NAME)

# save ky_dist_geo
devtools::use_data(ky_dist_geo, overwrite = TRUE)
