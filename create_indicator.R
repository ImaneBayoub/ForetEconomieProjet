library(sf)
library(terra)
library(dplyr)
library(exactextractr)

# -------------------------------
# 1. Charger les communes IGN
# -------------------------------

library(terra)
forest2012 <- rast("data/forest2012.tif")
forest2018 <- rast("data/forest2018.tif")


communes <- st_read(
  "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg",
  layer = "COMMUNE"
)

# Reprojection vers le CRS des rasters forêt
communes <- st_transform(communes, crs(forest2012))


# -------------------------------
# 2. Extraire la proportion de forêt
# -------------------------------

communes$part_foret_2012 <- exact_extract(forest2012, communes, 'mean')
communes$part_foret_2018 <- exact_extract(forest2018, communes, 'mean')


# -------------------------------
# 3. Charger ton fichier INSEE (RA)
# -------------------------------

df_insee <- read.csv("data/agri_all_clean.csv")   # contient CODE_GEO = "01001"


# -------------------------------
# 4. Jointure avec les communes IGN
# Identifiant IGN = code_insee
# -------------------------------

resultat <- df_insee %>%
  left_join(
    communes %>%
      st_drop_geometry() %>% 
      select(code_insee, part_foret_2012, part_foret_2018),
    by = c("CODE_GEO" = "code_insee")
  )


# -------------------------------
# 5. Export final
# -------------------------------

write.csv(resultat, "data/resultat_avec_foret.csv", row.names = FALSE)
