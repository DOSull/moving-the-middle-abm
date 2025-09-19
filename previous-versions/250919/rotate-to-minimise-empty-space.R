library(sf)
library(dplyr)
library(ggplot2)

parcels <- st_read("~/Documents/work/mwlr-moving-the-middle/abm/data/spatial/Rangitaiki/parcels.shp")

u_parcels <- parcels |>
  st_union()

c_parcels <- u_parcels |>
  st_centroid() |>
  st_coordinates()

mrr <- u_parcels |>
  st_minimum_rotated_rectangle() |>
  st_coordinates() |>
  as.data.frame() |>
  select(X, Y)

l1 <- sqrt((mrr$X[2] - mrr$X[1]) ^ 2 + (mrr$Y[2] - mrr$Y[1]) ^ 2)
l2 <- sqrt((mrr$X[3] - mrr$X[2]) ^ 2 + (mrr$Y[3] - mrr$Y[2]) ^ 2)

if (l1 < l2) {
  angle <- -atan2(mrr$Y[2] - mrr$Y[1], mrr$X[2] - mrr$X[1])
} else {
  angle <- -atan2(mrr$Y[3] - mrr$Y[2], mrr$X[3] - mrr$X[2])
}

ang1 <- angle
ang2 <- pi + angle

if (abs(ang1) < abs(ang2)) {
  angle <- ang1
} else {
  angle <- ang2
}

rot <- matrix(c( cos(angle), sin(angle),
                -sin(angle), cos(angle)), 2, 2, byrow = TRUE)

r_parcels <- parcels
r_parcels$geometry <- ((parcels$geometry - c_parcels) * rot) + c_parcels
r_parcels <- r_parcels |>
  st_set_crs(st_crs(parcels))

ggplot(u_parcels) + 
  geom_sf() + 
  geom_sf(data = r_parcels, fill = "white", alpha = 0, colour = "red")
