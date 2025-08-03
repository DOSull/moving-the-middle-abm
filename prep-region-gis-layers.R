library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(strex)
library(qgisprocess)
library(units)

region <- "Waihou-Piako"

spatial_folder <- str_glue("/Users/david/Documents/work/mwlr-moving-the-middle/abm/data/spatial")
region_folder  <- str_glue("{spatial_folder}/{region}")
src_folder     <- str_glue("{region_folder}/src")

# source parcels data 
parcels_src <- st_read(str_glue("{src_folder}/parcels.shp")) |>
  select(owners) |>
  qgis_run_algorithm_p("native:dissolve", FIELD = "owners", SEPARATE_DISJOINT = TRUE) |>
  st_as_sf() |>
  st_set_geometry("geometry") |>
  mutate(area = st_area(geometry)) |>
  filter(area >= as_units(1e4, "m^2")) |>
  select(-area)

parcels_bb <- parcels_src |>
  st_buffer(100) |>
  st_bbox() |>
  st_as_sfc() |>
  as.data.frame() |>
  st_sf()

# get LUC from LRIS clipped by parcels
luc_src <- st_read(str_glue("{spatial_folder}/nzlri-land-use-capability-2021.gpkg")) |>
  select(nzcu) |>
  st_intersection(parcels_bb) |>
  separate_wider_position(nzcu, widths = c(2, LUC = 1), too_many = "drop") |>
  filter(str_can_be_numeric(LUC)) |>
  mutate(LUC = as.numeric(LUC)) |>
  st_as_sf() |>
  st_set_geometry("geometry")

lu_src <- st_read(str_glue("{src_folder}/landuse.shp")) |>
  select(NZFARM_LU)

lu_lookup <- data.frame(
  NZFARM_LU = lu_src$NZFARM_LU |> unique() |> sort()
  # this should work for both? unclear as yet
  , LANDUSE = c(0, 1, 1000, 2, 0, 1000, 1000, 1000, 1000, 1000, 3, 1000, 0)
)

lu_src <- lu_src |> 
  left_join(lu_lookup) |>
  select(LANDUSE)

# make basic geometry of parcels
# use qgis process here as it is a lot faster than sf's use of dplyr::group_by
parcels <- parcels_src |>
  st_filter(luc_src) |>
  st_join(lu_src, largest = TRUE) |>
  filter(LANDUSE < 1000) |>
  mutate(area = st_area(geometry)) |>
  filter(area > as_units(1e4, "m^2")) |>
  mutate(ID = row_number(), STR_ID = as.character(ID)) |>
  select(-area, -owners, -LANDUSE)

parcels |> st_write(str_glue("{region_folder}/parcels.shp"), delete_dsn = TRUE)

bb <- parcels_src |> st_bbox()

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

luc_src |>
  st_intersection(parcels_src |> select() |> st_union()) |>
  as("SpatVector") |>
  rasterize(template_r, field = "LUC", fun = "min") |>
  # classify(rcl = matrix(c(0, NA), ncol = 2),) |>
  writeRaster(str_glue("{region_folder}/luc.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)

lu_src |> 
  as("SpatVector") |>
  rasterize(template_r, field = "LANDUSE", fun = "min") |>
  writeRaster(str_glue("{region_folder}/landuse.asc"), overwrite = TRUE, filetype = "AAIGrid", NAflag = -9999)

