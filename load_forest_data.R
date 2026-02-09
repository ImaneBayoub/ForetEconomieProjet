library(terra)
DATADIR <- "C://Users//fanny//Desktop//ENSAE//Cours//Projet-ESSD//ForetEconomieProjet//data//Forêts"

# Fix pour certaines versions de Rscript + terra (optionnel mais OK)
rast_temp <- rast(nrows=1, ncols=1, vals=1)

library(sf)
library(dplyr)

# Charger les rasters CLC
r1990 <- rast(file.path(DATADIR, "CLC1990.tif"))
r2000 <- rast(file.path(DATADIR, "CLC2000.tif"))
r2006 <- rast(file.path(DATADIR, "CLC2006.tif"))
r2012 <- rast(file.path(DATADIR, "CLC2012.tif"))
r2018 <- rast(file.path(DATADIR, "CLC2018.tif"))

forest_codes_internal <- c(23, 24, 25)

# Créer des rasters binaires forêt/non-forêt
forest1990 <- ifel(r1990 %in% forest_codes_internal, 1, 0)
forest2000 <- ifel(r2000 %in% forest_codes_internal, 1, 0)
forest2006 <- ifel(r2006 %in% forest_codes_internal, 1, 0)
forest2012 <- ifel(r2012 %in% forest_codes_internal, 1, 0)
forest2018 <- ifel(r2018 %in% forest_codes_internal, 1, 0)


# Vérifications
print(forest1990)
print(forest2000)
print(forest2006)
print(forest2012)
print(forest2018)

print(freq(forest1990))
print(freq(forest2000))
print(freq(forest2006))
print(freq(forest2012))
print(freq(forest2018))

writeRaster(forest1990, file.path(DATADIR, "forest1990.tif"), overwrite = TRUE)
writeRaster(forest2000, file.path(DATADIR, "forest2000.tif"), overwrite = TRUE)
writeRaster(forest2006, file.path(DATADIR, "forest2006.tif"), overwrite = TRUE)
writeRaster(forest2012, file.path(DATADIR, "forest2012.tif"), overwrite = TRUE)
writeRaster(forest2018, file.path(DATADIR, "forest2018.tif"), overwrite = TRUE)