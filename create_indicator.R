library(sf)
library(terra)
library(dplyr)
library(exactextractr)

# -------------------------------
# 1. Charger les communes IGN
# -------------------------------
communes <- st_read(
  "C:\\Users\\fanny\\Desktop\\ENSAE\\Cours\\Projet ESSD\\data\\ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20\\ADMIN-EXPRESS\\1_DONNEES_LIVRAISON_2025-11-00136\\ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20\\ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg",
  layer = "COMMUNE"
)

cat("\n=== CRS communes (IGN) ===\n")
print(st_crs(communes))   # EPSG:2154

# -------------------------------
# 2. Charger rasters forêt (déjà 0/1)
# -------------------------------
forest2012 <- rast("data/forest2012.tif")
forest2018 <- rast("data/forest2018.tif")

cat("\n=== CRS rasters forêt ===\n")
print(crs(forest2012))    # EPSG:3035
print(crs(forest2018))

# -------------------------------
# 3. Reprojeter les COMMUNES vers le raster (EPSG:3035)
# -------------------------------
communes_3035 <- st_transform(communes, crs(forest2012))

# Vérification simple
cat("\n=== CRS communes après transform ===\n")
print(st_crs(communes_3035))

# -------------------------------
# 4. Extraire la proportion de forêt (mean)
# -------------------------------
cat("\n=== Extraction forêt ===\n")

communes_3035$part_foret_2012 <- exact_extract(forest2012, communes_3035, 'mean')
communes_3035$part_foret_2018 <- exact_extract(forest2018, communes_3035, 'mean')

# Vérifier NA
cat("\nNA 2012 = ", sum(is.na(communes_3035$part_foret_2012)))
cat("\nNA 2018 = ", sum(is.na(communes_3035$part_foret_2018)), "\n")

# -------------------------------
# 5. Jointure INSEE
# -------------------------------
df_insee <- read.csv("data/agri_all_clean.csv")

resultat <- df_insee %>%
  left_join(
    communes_3035 %>%
      st_drop_geometry() %>%
      select(code_insee, part_foret_2012, part_foret_2018),
    by = c("CODE_GEO" = "code_insee")
  )
codes_orphelins <- df_insee %>%
  filter(!(CODE_GEO %in% communes_3035$code_insee)) %>%
  distinct(CODE_GEO)

print(codes_orphelins)

# -------------------------------
# 6. Export
# -------------------------------
write.csv(resultat, "data/resultat_avec_foret.csv", row.names = FALSE)

cat("\n>>> Extraction forêt + jointure terminées ✓ <<<\n")
