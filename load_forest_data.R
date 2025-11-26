library(terra)

# Fix pour certaines versions de Rscript + terra (optionnel mais OK)
rast_temp <- rast(nrows=1, ncols=1, vals=1)

library(sf)
library(dplyr)

# Charger les rasters CLC
r2012 <- rast("clc_rasters/CLC2012.tif")
r2018 <- rast("clc_rasters/CLC2018.tif")

forest_codes_internal <- c(23, 24, 25)

forest2012 <- ifel(r2012 %in% forest_codes_internal, 1, 0)
forest2018 <- ifel(r2018 %in% forest_codes_internal, 1, 0)


# Vérifications
print(forest2012)
print(forest2018)

print(freq(forest2012))
print(freq(forest2018))

writeRaster(forest2012, "data/forest2012.tif", overwrite = TRUE)
writeRaster(forest2018, "data/forest2018.tif", overwrite = TRUE)

