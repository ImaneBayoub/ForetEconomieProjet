library(sf)
library(terra)
library(dplyr)
library(ggplot2)

# ============================================================
# 0) Chemins
# ============================================================
gpkg_path <- "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"

clc2012_tif <- "data/Results/u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif"
clc1990_tif <- "/home/imane/Documents/ensae/ForetEconomieProjet/ForetEconomieProjet-1/data/Results/u2018_clc2012_v2020_20u1_raster100m/DATA/U2000_CLC1990_V2020_20u1.tif"


# ============================================================
# 1) Polygones des régions
# ============================================================
communes_sf <- st_read(gpkg_path, layer = "COMMUNE", quiet = TRUE)

regions_sf <- communes_sf %>%
  group_by(code_insee_de_la_region) %>%
  summarise(geometrie = st_union(geometrie), .groups = "drop")

regions_sf$area_ha <- as.numeric(st_area(regions_sf$geometrie)) / 10000
regions_v <- vect(regions_sf)

# ============================================================
# 2) Charger rasters
# ============================================================
clc2012 <- rast(clc2012_tif)
clc1990 <- rast(clc1990_tif)

if (!compareGeom(clc2012, clc1990, stopOnError = FALSE)) {
  clc1990 <- resample(clc1990, clc2012, method = "near")
}

regions_v <- project(regions_v, crs(clc2012))

cell_area_ha <- prod(res(clc2012)) / 10000

# ============================================================
# 3) Recodage forêt & agricole
# ============================================================
rcl_forest <- matrix(c(
  -Inf, 22, 0,
  23, 33, 1,
  34, Inf, 0
), ncol = 3, byrow = TRUE)

rcl_agri <- matrix(c(
  -Inf, 11, 0,
  12, 22, 1,
  23, Inf, 0
), ncol = 3, byrow = TRUE)

forest2012 <- classify(clc2012, rcl_forest)
forest1990 <- classify(clc1990, rcl_forest)

agri2012 <- classify(clc2012, rcl_agri)
agri1990 <- classify(clc1990, rcl_agri)

area_ha_in_poly <- function(bin_raster, poly_vect) {
  r <- crop(bin_raster, poly_vect)
  r <- mask(r, poly_vect)
  n <- global(r == 1, "sum", na.rm = TRUE)[1,1]
  if (is.na(n)) n <- 0
  n * cell_area_ha
}

# ============================================================
# 4) Indicateurs inversés
#    X = part agricole 2012
#    Y = taux évolution forêt
#    Taille = solde forêt
# ============================================================
res <- vector("list", nrow(regions_sf))

for (i in seq_len(nrow(regions_sf))) {

  reg_sf <- regions_sf[i, ]
  reg_v  <- regions_v[i]
  tot_ha <- reg_sf$area_ha[1]

  agri12_ha   <- area_ha_in_poly(agri2012, reg_v)
  forest90_ha <- area_ha_in_poly(forest1990, reg_v)
  forest12_ha <- area_ha_in_poly(forest2012, reg_v)

  # X : part agricole en 2012
  x_share_agri_2012 <- ifelse(tot_ha > 0, 100 * agri12_ha / tot_ha, NA_real_)

  # Y : taux évolution forêt
  y_taux_evol_forest <- ifelse(forest90_ha > 0,
                               (forest12_ha - forest90_ha) / forest90_ha,
                               NA_real_)

  # Taille : solde forestier
  solde_forest_ha <- forest12_ha - forest90_ha

  res[[i]] <- data.frame(
    code_insee_de_la_region = as.character(reg_sf$code_insee_de_la_region[1]),
    share_agri_2012 = x_share_agri_2012,
    taux_evol_forest = y_taux_evol_forest,
    solde_forest_ha = solde_forest_ha
  )
}

df <- bind_rows(res) %>%
  mutate(
    solde_abs = abs(solde_forest_ha),
    signe_forest = ifelse(solde_forest_ha >= 0,
                          "Gain forêt",
                          "Perte forêt")
  )

write.csv(df,
          file.path("data",
          "indicateurs_regions_Xagri2012_Yforet_TailleSoldeForet_1990_2012.csv"),
          row.names = FALSE)

# ============================================================
# 5) Graphique bulles
# ============================================================
p <- ggplot(df, aes(
  x = share_agri_2012,
  y = taux_evol_forest,
  size = solde_abs,
  fill = signe_forest
)) +
  geom_point(shape = 21, color = "black", alpha = 0.85) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(name = "Part du territoire agricole en 2012 (%)") +
  scale_size_continuous(name = "Solde forestier 1990–2012 (ha)\n(valeur absolue)") +
  labs(
    title = "Agriculture (stock 2012) vs évolution de la forêt (1990–2012)",
    subtitle = "X = part agricole 2012 ; Y = taux évolution forêt ; taille = solde forêt",
    y = "Taux d’évolution de la forêt 1990→2012",
    fill = ""
  ) +
  theme_minimal()

ggsave(file.path("plots",
       "bubble_Xagri2012_Yforet_TailleSoldeForet_1990_2012.png"),
       p, width = 13, height = 7, dpi = 220)

print(p)
cat("\nOK -> plots directory\n")