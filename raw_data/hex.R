library(hexmapr)
library(tigris)
library(tidyverse)

wi <- counties("Wisconsin", cb = TRUE)
wi_original <- wi
plot(wi)

input_file <- system.file('data/ky_dist.shp')
ky <- read_polygons("raw_data/ky_dist.shp")



ky_details <- get_shape_details(ky)


ky@data$xcentroid <- coordinates(ky)[,1]
ky@data$ycentroid <- coordinates(ky)[,2]


clean <- function(shape){
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = plyr::join(shape.points, shape@data, by="id")
}

result_df_ky <- clean(ky)


rawplot <- ggplot(result_df_ky) +
  geom_polygon( aes(x=long, y=lat, fill = NAME, group = group)) +
  coord_equal() +
  guides(fill=FALSE) +
  theme_void()

rawplot


new_cells <-  calculate_cell_size(ky, ky_details,
                                  0.03, 'hexagonal', 2)

plot(new_cells[[2]], main = paste("Seed",2, sep=" "))


resulthex <- assign_polygons(ky ,new_cells)

result_df <- clean(resulthex)

hexplot <- ggplot(result_df) +
  geom_polygon( aes(x=long, y=lat, fill = NAME, group = group)) +
  geom_text(aes(V1, V2, label = substr(NAME,1,10)), size=2,color = "white") +
  coord_equal() +
  guides(fill=FALSE) +
  theme_void()

hexplot
