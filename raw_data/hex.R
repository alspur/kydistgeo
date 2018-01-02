library(hexmapr)
library(tidyverse)
# load packages
library(tidyverse)
library(sf)
library(mapview)

# read shapefile
districts <- read_sf("raw_data/ky_dist.shp")


ky_dist_geo <- st_transform(districts,
                            crs = "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
  filter(!stringr::str_detect(NAME, "DISTRICT NOT DEFINED")) %>%
  mutate(dist_name = stringr::str_to_title(NAME)) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              " School District", "")) %>%
  mutate(dist_name = stringr::str_replace_all(dist_name,
                                              " Dependent Schools", ""))


# mercer merger --------

# get dist shapes
mercer_hburg <- ky_dist_geo %>%
  filter(dist_name == "Mercer County" |
           dist_name == "Harrodsburg Independent")

# join shapes
new_mercer <- st_union(mercer_hburg)

# update mercer geography
ky_dist_geo[ky_dist_geo$dist_name == "Mercer County",]$geometry <- new_mercer

# webster merger --------

# get dist shapes
webster_prov <- ky_dist_geo %>%
  filter(dist_name == "Webster County" |
           dist_name == "Providence Independent")

# join shapes
new_webster <- st_union(webster_prov)

# update mercer geography
ky_dist_geo[ky_dist_geo$dist_name == "Webster County",]$geometry <- new_webster

# wayne merger --------

# get dist shapes
wayne_mont <- ky_dist_geo %>%
  filter(dist_name == "Wayne County" |
           dist_name == "Monticello Independent")

# join shapes
new_wayne <- st_union(wayne_mont)

# update mercer geography
ky_dist_geo[ky_dist_geo$dist_name == "Wayne County",]$geometry <- new_wayne


# caverna conundrum ---------

caverna <- ky_dist_geo %>%
  filter(str_detect(dist_name, "Caverna Ind"))

new_caverna <- st_union(caverna)

ky_dist_geo[ky_dist_geo$dist_name == "Caverna Independent",]$geometry <- new_caverna

# corbin conundrum ---------

corbin <- ky_dist_geo %>%
  filter(str_detect(dist_name, "Corbin Ind"))

new_corbin<- st_union(corbin)

ky_dist_geo[ky_dist_geo$dist_name == "Corbin Independent",]$geometry <- new_corbin

# fort knox + bullitt -----------
# get dist shapes
bullitt_knox <- ky_dist_geo %>%
  filter(dist_name == "Bullitt County" |
           dist_name == "Fort Knox") %>%
  filter(COUNTY == 21029)

# join shapes
new_bullitt <- st_union(bullitt_knox)

# update bullitt geography
ky_dist_geo[ky_dist_geo$dist_name == "Bullitt County",]$geometry <- new_bullitt

# fort knox + hardin  -----------
# get dist shapes
hardin_knox <- ky_dist_geo %>%
  filter(dist_name == "Hardin County" |
           dist_name == "Fort Knox") %>%
  filter(COUNTY == 21093)

# join shapes
new_hardin <- st_union(hardin_knox)

# update bullitt geography
ky_dist_geo[ky_dist_geo$dist_name == "Hardin County",]$geometry <- new_hardin

# fort knox + meade  -----------
# get dist shapes
meade_knox <- ky_dist_geo %>%
  filter(dist_name == "Meade County" |
           dist_name == "Fort Knox") %>%
  filter(COUNTY == 21163)

# join shapes
new_meade <- st_union(meade_knox)

# update bullitt geography
ky_dist_geo[ky_dist_geo$dist_name == "Meade County",]$geometry <- new_meade

# fort campbell + christian  -----------
# get dist shapes
christian_campbell <- ky_dist_geo %>%
  filter(dist_name == "Christian County" |
           dist_name == "Fort Campbell") %>%
  filter(COUNTY == 21047)

# join shapes
new_christian <- st_union(christian_campbell)

# update christian geography
ky_dist_geo[ky_dist_geo$dist_name == "Christian County",]$geometry <- new_christian


# fort campbell + trigg  -----------
# get dist shapes
trigg_campbell <- ky_dist_geo %>%
  filter(dist_name == "Trigg County" |
           dist_name == "Fort Campbell") %>%
  filter(COUNTY == 21221)

# join shapes
new_trigg <- st_union(trigg_campbell)

# update trigg geography
ky_dist_geo[ky_dist_geo$dist_name == "Trigg County",]$geometry <- new_trigg


# remove independent districts from ky geo df
ky_dist_geo <- ky_dist_geo %>%
  filter(dist_name != "Harrodsburg Independent") %>%
  filter(dist_name != "Providence Independent") %>%
  filter(dist_name != "Monticello Independent") %>%
  filter(dist_name != "Caverna Independent" | ID == 1) %>%
  filter(dist_name != "Corbin Independent" | ID == 1) %>%
  filter(dist_name != "Fort Knox") %>%
  filter(dist_name != "Fort Campbell") %>%
  select(dist_name, geometry) %>%
  mutate(dist_name = str_replace_all(dist_name, "Ft Thomas", "Fort Thomas"))

# get dist id's
dist_id <- profile_dist %>%
  filter(year == "2016-2017") %>%
  pull(sch_id)


ky_dist_geo <- ky_dist_geo %>%
  arrange(dist_name) %>%
  mutate(sch_id = dist_id[1:173]) %>%
  select(sch_id, dist_name, geometry)

# plot clean districts

ggplot(ky_dist_geo) +
  geom_sf() +
  theme_bw()


mapview(ky_dist_geo)

# save clean districts
devtools::use_data(ky_dist_geo, overwrite = TRUE)


# convert sf df to spatial df for hex operations
ky_spatial <- as(ky_dist_geo, "Spatial")

ky_details <- get_shape_details(ky_spatial)

ky_spatial@data$xcentroid <- coordinates(ky_spatial)[,1]
ky_spatial@data$ycentroid <- coordinates(ky_spatial)[,2]


clean <- function(shape){
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = plyr::join(shape.points, shape@data, by="id")
}

result_df_ky <- clean(ky_spatial)


rawplot <- ggplot(result_df_ky) +
  geom_polygon( aes(x=long, y=lat, fill = dist_name, group = group)) +
  coord_equal() +
  guides(fill=FALSE) +
  theme_void()

rawplot

# test hex patterns ------

par(mfrow=c(7,5), mar = c(0,1,2,1))
for (i in 1:35){
  new_cells <-  calculate_cell_size(ky_spatial, ky_details,
                                    0.03, 'hexagonal', i)
  if(i == 7) {
    plot(new_cells[[2]], main = paste("Seed",i, sep=" "), border = "red")
  } else {
    plot(new_cells[[2]], main = paste("Seed",i, sep=" "))
  }
}

# assign hex pattern --------

new_cells <-  calculate_cell_size(ky_spatial, ky_details,
                                  0.03, 'hexagonal', 2)

plot(new_cells[[2]], main = paste("Seed", 2, sep=" "))



resulthex <- assign_polygons(ky_spatial ,new_cells)

result_df <- clean(resulthex)

hexplot <- ggplot(result_df) +
  geom_polygon( aes(x=long, y=lat, fill = dist_name, group = group)) +
  geom_text(aes(V1, V2,
                label = str_replace_all(dist_name, " ", "\n")),
            size=2,color = "white") +
  coord_equal() +
  guides(fill=FALSE) +
  theme_void()

hexplot

# save hex data -----
ky_hex <- result_df

devtools::use_data(ky_hex, overwrite = TRUE)

