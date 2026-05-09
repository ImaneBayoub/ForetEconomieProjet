# Approche matricielle rapide pour CLC 2000
library(terra)
library(sf)
library(dplyr)

gpkg_path <- "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"
clc2000_tif <- "data/Results/u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif"

# --- 1. Charger communes France métro ---
communes <- st_read(gpkg_path, layer = "COMMUNE", quiet = TRUE)
communes <- communes %>%
  mutate(insee = as.character(code_insee)) %>%
  filter(!substr(insee, 1, 2) %in% c("971","972","973","974","975","976","977","978"))
cat("Communes :", nrow(communes), "\n")

# --- 2. Charger raster CLC 2000, crop à la France ---
r0 <- rast(clc2000_tif)
# Convertir en valeurs numériques (enlever les catégories)
r0 <- as.int(r0)
cat("Raster valeurs : min =", global(r0, "min", na.rm=TRUE)[1,1],
    "max =", global(r0, "max", na.rm=TRUE)[1,1], "\n")

# Union France, projeter et crop
france_union <- st_union(communes)
france_vect <- vect(france_union)
france_proj <- project(france_vect, crs(r0))
r_fr <- crop(r0, france_proj)
cat("Raster France :", nrow(r_fr), "x", ncol(r_fr), "\n")

rcl <- matrix(c(1, 11, 1, 12, 22, 2, 23, 33, 3), ncol = 3, byrow = TRUE)
rc <- classify(r_fr, rcl, others = NA)
names(rc) <- "clc_class"
cat("Valeurs dans rc :", as.character(freq(rc)$value), "\n")
rm(r0, r_fr)

# --- 3. Rasteriser les communes ---
communes_v <- vect(communes)
communes_v$cid <- seq_len(nrow(communes_v))
communes_proj <- project(communes_v, crs(rc))
cid_r <- rasterize(communes_proj, rc, field = "cid", touches = TRUE)
names(cid_r) <- "cid"
cat("CIDs dans cid_r :", nrow(freq(cid_r)), "\n")

# --- 4. Focal adjacency sur le raster entier ---
w <- matrix(1, 3, 3); w[2,2] <- 0
is_foret <- rc == 3
foret_nb <- focal(is_foret, w = w, fun = sum, na.rm = TRUE, fillvalue = 0)
rm(is_foret)
is_agri <- rc == 2
agri_nb <- focal(is_agri, w = w, fun = sum, na.rm = TRUE, fillvalue = 0)
rm(is_agri)
gc()
cat("Focal terminé\n")

agri_adj <- (rc == 2) & (foret_nb > 0)
foret_adj <- (rc == 3) & (agri_nb > 0)
rm(foret_nb, agri_nb)
gc()
cat("Adjacence calculée\n")

# --- 5. Zonal par commune ---
z_total <- zonal(!is.na(rc), cid_r, fun = "sum", na.rm = TRUE)
z_agri  <- zonal(rc == 2, cid_r, fun = "sum", na.rm = TRUE)
z_foret <- zonal(rc == 3, cid_r, fun = "sum", na.rm = TRUE)
z_aaf   <- zonal(agri_adj, cid_r, fun = "sum", na.rm = TRUE)
z_faa   <- zonal(foret_adj, cid_r, fun = "sum", na.rm = TRUE)
cat("Zonal :", nrow(z_total), nrow(z_agri), nrow(z_foret), nrow(z_aaf), nrow(z_faa), "\n")

# --- 6. Assembler avec full_join ---
res <- z_total %>% rename(pixels_total = clc_class)
res <- full_join(res, z_agri %>% rename(agri = clc_class), by = "cid")
res <- full_join(res, z_foret %>% rename(foret = clc_class), by = "cid")
res <- full_join(res, z_aaf %>% rename(agri_adj_foret = clc_class), by = "cid")
res <- full_join(res, z_faa %>% rename(foret_adj_agri = clc_class), by = "cid")
res[is.na(res)] <- 0

cid_lut <- data.frame(cid = communes_v$cid, insee = communes_v$insee,
  nom = communes_v$nom_officiel, stringsAsFactors = FALSE)

res <- res %>% left_join(cid_lut, by = "cid") %>%
  mutate(
    lisiere_pct = ifelse(pixels_total > 0, agri_adj_foret / pixels_total, 0),
    lisiere_pct_agri = ifelse(agri > 0, agri_adj_foret / agri, 0),
    lisiere_px = agri_adj_foret
  ) %>% filter(!is.na(insee))

cat("Résultat :", nrow(res), "communes\n")

# --- 7. Fusion avec twfe_data_lisiere.csv ---
twfe <- read.csv("data/twfe_data_lisiere.csv", stringsAsFactors = FALSE)
twfe$id <- as.character(twfe$id)

clc2000_join <- res %>% select(insee, lisiere_pct, lisiere_pct_agri, lisiere_px)

twfe <- twfe %>%
  left_join(clc2000_join, by = c("id" = "insee"))

twfe <- twfe %>% mutate(
  lisiere_pct.x = ifelse(time == 2 & !is.na(lisiere_pct.y), lisiere_pct.y, lisiere_pct.x),
  lisiere_pct_agri.x = ifelse(time == 2 & !is.na(lisiere_pct_agri.y), lisiere_pct_agri.y, lisiere_pct_agri.x),
  lisiere_px.x = ifelse(time == 2 & !is.na(lisiere_px.y), lisiere_px.y, lisiere_px.x)
) %>% select(-lisiere_pct.y, -lisiere_pct_agri.y, -lisiere_px.y)

write.csv(twfe, "data/twfe_data_lisiere.csv", row.names = FALSE)

cat("Obs avec lisière :", sum(!is.na(twfe$lisiere_pct.x)), "/", nrow(twfe), "\n")
cat("Obs sans lisière :", sum(is.na(twfe$lisiere_pct.x)), "\n")
cat("Fini !\n")
