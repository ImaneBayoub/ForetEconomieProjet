library(terra)
library(sf)
library(dplyr)

# Communes France IGN via geodata (CRS 3035 par défaut)
communes <- geodata::gadm("FRA", level = 2, path = tempdir())
communes <- st_as_sf(communes)

communes <- st_transform(communes, crs(forest2012))

forest_communes <- exactextractr::exact_extract(
  forest2012,
  communes,
  fun = "mean"
)

communes$part_foret <- forest_communes

