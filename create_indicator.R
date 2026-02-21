library(sf)
library(terra)
library(dplyr)
library(exactextractr)

# -------------------------------
# 1) Charger les communes IGN
# -------------------------------
communes <- st_read(
  "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg",
  layer = "COMMUNE",
  quiet = TRUE
)

# -------------------------------
# 2) Charger rasters (alignés mêmes résolution/extent)
#    - forêt : 0/1
#    - agricole 2012 : 0/1  (nécessaire pour "agri -> foret")
# -------------------------------
forest2012 <- rast("data/forest2012.tif")
forest2018 <- rast("data/forest2018.tif")

# IMPORTANT: il te faut un raster agricole 2012 (0/1)
agri2012   <- rast("data/agri2012.tif")

# Si tes rasters ne sont pas parfaitement alignés, on aligne tout sur forest2012
forest2018 <- resample(forest2018, forest2012, method = "near")
agri2012   <- resample(agri2012,   forest2012, method = "near")

# -------------------------------
# 3) Reprojeter les communes vers le CRS du raster
# -------------------------------
communes_r <- st_transform(communes, crs(forest2012))

# -------------------------------
# 4) Construire les indicateurs raster
# -------------------------------

# (A) Pixels passés de agricole (2012) à forêt (2018)
#     => agri2012 == 1 ET forest2018 == 1
agri_to_forest_1218 <- (agri2012 == 1) & (forest2018 == 1)
agri_to_forest_1218 <- as.integer(agri_to_forest_1218)  # 0/1

# (B) Pixels agricoles "collés" à la forêt (ex: en 2012)
#     => agri2012 == 1 ET au moins un voisin (3x3) est forêt2012
w <- matrix(1, 3, 3)
forest2012_neigh <- focal(forest2012, w = w, fun = max, na.rm = TRUE, pad = TRUE)
agri_adj_forest_2012 <- (agri2012 == 1) & (forest2012_neigh == 1) & (forest2012 == 0)
agri_adj_forest_2012 <- as.integer(agri_adj_forest_2012)  # 0/1

# Optionnel: adjacence à la forêt en 2018
forest2018_neigh <- focal(forest2018, w = w, fun = max, na.rm = TRUE, pad = TRUE)
agri_adj_forest_2018 <- (agri2012 == 1) & (forest2018_neigh == 1) & (forest2018 == 0)
agri_adj_forest_2018 <- as.integer(agri_adj_forest_2018)

# -------------------------------
# 5) Extraire des COMPTES de pixels par commune
#    exact_extract avec sum(value * coverage_fraction)
#    => donne un "nombre de pixels équivalents" (si bord coupé)
# -------------------------------
sum_pixels <- function(values, coverage_fraction) {
  sum(values * coverage_fraction, na.rm = TRUE)
}

communes_r$px_agri_to_forest_1218 <- exact_extract(agri_to_forest_1218, communes_r, sum_pixels)
communes_r$px_agri_adj_forest_2012 <- exact_extract(agri_adj_forest_2012, communes_r, sum_pixels)
communes_r$px_agri_adj_forest_2018 <- exact_extract(agri_adj_forest_2018, communes_r, sum_pixels)

# (Optionnel) arrondir si tu veux des comptes "entiers"
communes_r <- communes_r %>%
  mutate(
    px_agri_to_forest_1218   = round(px_agri_to_forest_1218),
    px_agri_adj_forest_2012  = round(px_agri_adj_forest_2012),
    px_agri_adj_forest_2018  = round(px_agri_adj_forest_2018)
  )

# -------------------------------
# 6) Jointure INSEE
# -------------------------------
df_insee <- read.csv("data/agri_all_clean.csv", stringsAsFactors = FALSE)

resultat <- df_insee %>%
  left_join(
    communes_r %>%
      st_drop_geometry() %>%
      select(code_insee, px_agri_to_forest_1218, px_agri_adj_forest_2012, px_agri_adj_forest_2018),
    by = c("CODE_GEO" = "code_insee")
  )

# Diagnostics: codes INSEE non matchés
codes_orphelins <- df_insee %>%
  filter(!(CODE_GEO %in% communes_r$code_insee)) %>%
  distinct(CODE_GEO)

print(codes_orphelins)

# -------------------------------
# 7) Export
# -------------------------------
write.csv(resultat, "data/resultat_pixels_foret.csv", row.names = FALSE)
cat("\n>>> Pixels agri->foret + agri adj forêt extraits et joints ✓ <<<\n")