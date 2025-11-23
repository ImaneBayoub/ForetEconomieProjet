library(terra)

# Fix pour certaines versions de Rscript + terra (optionnel mais OK)
rast_temp <- rast(nrows=1, ncols=1, vals=1)

library(sf)
library(dplyr)

# Charger les rasters CLC
r2012 <- rast("clc_rasters/CLC2012.tif")
r2018 <- rast("clc_rasters/CLC2018.tif")

# Convertir en valeurs brutes (car CLC est catégoriel)
r2012_raw <- app(r2012, fun = function(x) as.integer(x))
r2018_raw <- app(r2018, fun = function(x) as.integer(x))

# Codes internes CLC pour les classes forestières
forest_codes_internal <- c(23, 24, 25)

# Création du raster forêt 2012 (1 = forêt, 0 = non-forêt)
forest2012 <- ifel(r2012_raw %in% c(23,24,25), 1, 0)
forest2018 <- ifel(r2018_raw %in% c(23,24,25), 1, 0)


# Vérifications
print(forest2012)
print(forest2018)

print(freq(forest2012))
print(freq(forest2018))

writeRaster(forest2012, "data/forest2012.tif", overwrite = TRUE)
writeRaster(forest2018, "data/forest2018.tif", overwrite = TRUE)

