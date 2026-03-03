library(terra)
library(sf)
library(dplyr)
library(stringr)

folder <- "/Users/david/Documents/work/mwlr-moving-the-middle/abm/data/spatial/Oreti"

parcels_src <- st_read(str_glue("{folder}/parcels.gpkg"))

parcels <- parcels_src |>
  select() |>
  mutate(ID = row_number(), SRC_ID = as.character(ID))

parcels |> st_write(str_glue("{folder}/parcels.shp"), delete_dsn = TRUE)

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

luc_src <- st_read(str_glue("{folder}/luc.gpkg"))

# parcels |> 
#   as("SpatVector") |>
#   rasterize(template_r, field = "STR_ID") |> 
#   classify(rcl = matrix(c(0, NA), ncol = 2),) |>
#   writeRaster(str_glue("{folder}/parcels_r.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)
# 
# landuse |> project(template_r, method = "mode") |>
#   writeRaster(str_glue("{folder}/landuse.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)
# 
luc_src |>
  select(domluc) |>
  st_intersection(parcels |> select() |> st_union()) |>
  mutate(LUC = as.integer(domluc)) |>
  as("SpatVector") |>
  rasterize(template_r, field = "LUC", fun = "min") |>
  # classify(rcl = matrix(c(0, NA), ncol = 2),) |>
  writeRaster(str_glue("{folder}/luc.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)

lu_src  <- st_read(str_glue("{folder}/landuse.gpkg"))

lu_lookup <- data.frame(
  NZFARM_LU = lu_src$NZFARM_LU |> unique(), 
  LANDUSE = c(1000, 1000, 0, 1000, 1, 2, 0, 1000, 3, 1000, 0, 1000, 1000)
)

lu_src |> 
  left_join(lu_lookup) |>
  select(LANDUSE) |>
  as("SpatVector") |>
  rasterize(template_r, field = "LANDUSE", fun = "min") |>
  writeRaster(str_glue("{folder}/landuse.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)

