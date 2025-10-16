library(terra)
library(sf)
library(stringr)

folder <- "/Users/david/Documents/work/mwlr-moving-the-middle/abm/data/spatial/Rangitaiki"

landuse <- rast(str_glue("{folder}/landuse-hires.asc"))
luc     <- rast(str_glue("{folder}/luc-hires.asc"))
parcels <- st_read(str_glue("{folder}/parcels.shp"))

crs(landuse) <- st_crs(parcels)$wkt
crs(luc) <- st_crs(parcels)$wkt

bb <- parcels |> st_bbox()

w <- bb[3] - bb[1]
h <- bb[4] - bb[2]
xc <- (bb[1] + bb[3]) / 2
yc <- (bb[2] + bb[4]) / 2

cellsize <- 400
nc <- ceiling(w / cellsize)
nr <- ceiling(h / cellsize)

w_actual <- cellsize * nc
h_actual <- cellsize * nr

bb_r <- ext(c(xc - w_actual / 2, xc + w_actual / 2, yc - h_actual / 2, yc + h_actual / 2))

template_r <- rast(bb_r, res = cellsize, crs = st_crs(parcels)$wkt)

parcels |> 
  as("SpatVector") |>
  rasterize(template_r, field = "STR_ID") |> 
  classify(rcl = matrix(c(0, NA), ncol = 2),) |>
  writeRaster(str_glue("{folder}/parcels_r.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)

landuse |> project(template_r, method = "mode") |>
  writeRaster(str_glue("{folder}/landuse.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)

luc |> project(template_r, method = "mode") |>
  writeRaster(str_glue("{folder}/luc.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)
