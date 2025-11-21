library(terra)
library(sf)
library(dplyr)

# Charger les rasters
r2012 <- rast("/home/imane/ForetEconomieProjet/clc_rasters/CLC2012.tif")
r2018 <- rast("/home/imane/ForetEconomieProjet/clc_rasters/CLC2018.tif")

# Convertir en valeurs brutes (car les rasters sont catégoriels)
r2012_raw <- app(r2012, fun = function(x) as.integer(x))
r2018_raw <- app(r2018, fun = function(x) as.integer(x))

# Codes forêt CLC internes (1-44 mapping):
# 23 = 311 (forêt feuillue)
# 24 = 312 (conifères)
# 25 = 313 (forêts mixtes)
forest_codes_internal <- c(23, 24, 25)

# Table de reclassification forêt=1 / autres = NA
rcl <- cbind(forest_codes_internal,
             forest_codes_internal,
             1)

# Classifier
forest2012 <- classify(r2012_raw, rcl = rcl, others = NA)
forest2018 <- classify(r2018_raw, rcl = rcl, others = NA)

# Vérifier
forest2012
forest2018

# Optionnel : fréquences (doit montrer 1 et NA)
freq(forest2012)
freq(forest2018)
