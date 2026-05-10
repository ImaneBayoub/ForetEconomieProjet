library(terra)
library(sf)

# --- Chemins ---
# Communes IGN
gpkg_path <- "data/Communes_IGN/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"

# Données CLC
clc1990_tif <- "data/Forêts/U2000_CLC1990_V2020_20u1.tif"
clc2000_tif <- "data/Forêts/U2006_CLC2000_V2020_20u1.tif"
clc2012_tif <- "data/Forêts/U2018_CLC2012_V2020_20u1.tif"


# --- Output ---
out_dir <- "data/Forêts"

# --- Lire communes + filtre IDF ---
communes_sf <- st_read(gpkg_path, layer = "COMMUNE", quiet = TRUE)
idf_sf <- communes_sf
stopifnot(nrow(idf_sf) > 0)

# --- Charger rasters une fois ---
clc1990 <- rast(clc1990_tif)
clc2000 <- rast(clc2000_tif)
clc2012 <- rast(clc2012_tif)

# --- Aligner les 3 rasters 1990, 2000 et 2012 pour géométrie identique ---
# (important pour les transitions pixel à pixel)
if (!compareGeom(clc2012, clc1990, stopOnError = FALSE)) {
  clc1990 <- resample(clc1990, clc2012, method = "near")
}
if (!compareGeom(clc2012, clc2000, stopOnError = FALSE)) {
  clc2000 <- resample(clc2000, clc2012, method = "near")
}
if (!compareGeom(clc2012, clc2000, stopOnError = FALSE)) {
  clc2012 <- resample(clc2012, clc2000, method = "near")
}

# --- Matrice de recodage CLC -> 1/2/3 ---
# 1-11 artificiel, 12-22 agricole, 23-33 foret/semi-naturel
rcl <- matrix(c(
  1,  11, 1,
  12, 22, 2,
  23, 33, 3
), ncol = 3, byrow = TRUE)

# --- Voisinage 8 (queen) ---
w <- matrix(1, 3, 3)
w[2, 2] <- 0

# ---- helpers ----
count_true <- function(x) {
  global(x, "sum", na.rm = TRUE)[1, 1]
}

count_adjacent <- function(r3, target_class, neigh_class, w) {
  neigh_bin <- r3 == neigh_class
  neigh_sum <- focal(neigh_bin, w = w, fun = sum, na.rm = TRUE, fillvalue = 0)
  adj <- (r3 == target_class) & (neigh_sum > 0)
  count_true(adj)
}

# --- Résultats (1 ligne par commune) ---
res_list <- vector("list", nrow(idf_sf))

for (i in seq_len(nrow(idf_sf))) {

  com <- idf_sf[i, ]

  # 1) prendre la 1ère colonne existante
  com_code <- as.character(com$code_insee[1])
  com_name <- as.character(com$nom_officiel[1]) 
  # 2) fallback si NA/vide
  if (length(com_code) == 0 || is.na(com_code) || trimws(com_code) == "") {
    com_code <- sprintf("row_%05d", i)
  }
  if (length(com_name) == 0 || is.na(com_name) || trimws(com_name) == "") {
    com_name <- com_code
  } 

  # 3) garantir scalaire
  com_code <- as.character(com_code[1])
  com_name <- as.character(com_name[1])
  com_vect <- vect(com)

  # Reprojeter la commune sur CRS raster
  com_proj <- project(com_vect, crs(clc2012))

  # Extraire/masquer chaque raster
  r12 <- mask(crop(clc2012, com_proj), com_proj)
  r02 <- mask(crop(clc2000, com_proj), com_proj)
  r90 <- mask(crop(clc1990, com_proj), com_proj)

  # Recoder en 1/2/3
  r2012 <- classify(r12, rcl, others = NA)
  r2000 <- classify(r02, rcl, others = NA)
  r1990 <- classify(r90, rcl, others = NA)

  # Si commune vide (pas de pixels), on skip proprement
  nvalid12 <- global(!is.na(r2012), "sum", na.rm = TRUE)[1, 1]
  nvalid02 <- global(!is.na(r2000), "sum", na.rm = TRUE)[1, 1]
  nvalid90 <- global(!is.na(r1990), "sum", na.rm = TRUE)[1, 1]
  if (is.na(nvalid12) || nvalid12 == 0 || is.na(nvalid90) || nvalid90 == 0 || is.na(nvalid02) || nvalid02 == 0) {
  res_list[[i]] <- data.frame(
    insee = com_code,
    nom   = com_name,
    pixels_total_1990 = 0,
    pixels_total_2000 = 0,
    pixels_total_2012 = 0,
    agri_1990 = 0, agri_2000 = 0, agri_2012 = 0,
    foret_1990 = 0, foret_2000 = 0, foret_2012 = 0,
    agri_adj_foret_1990 = 0, agri_adj_foret_2000 = 0, agri_adj_foret_2012 = 0,
    diff_agri_12_90 = 0,
    diff_agri_00_90 = 0,
    diff_agri_12_00 = 0,
    diff_foret_12_90 = 0,
    diff_foret_00_90 = 0,
    diff_foret_12_00 = 0,
    agri_to_foret_90_12 = 0,
    agri_to_foret_00_12 = 0,
    agri_to_foret_90_00 = 0,
    foret_to_agri_90_00 = 0,
    foret_to_agri_00_12 = 0,
    foret_to_agri_90_12 = 0
  )
    next
  }

  pixels_total_2012 <- as.numeric(nvalid12)
  pixels_total_1990 <- as.numeric(nvalid90)
  pixels_total_2000 <- as.numeric(nvalid02)

  # Comptages agricoles / forêt
  agri_1990  <- count_true(r1990 == 2)
  agri_2012  <- count_true(r2012 == 2)
  foret_1990 <- count_true(r1990 == 3)
  foret_2012 <- count_true(r2012 == 3)
  agri_2000  <- count_true(r2000 == 2)
  foret_2000 <- count_true(r2000 == 3)

  # Agricole adjacent forêt
  agri_adj_foret_1990 <- count_adjacent(r1990, 2, 3, w)
  agri_adj_foret_2012 <- count_adjacent(r2012, 2, 3, w)
  agri_adj_foret_2000 <- count_adjacent(r2000, 2, 3, w)

  # Transitions
  # (1990 -> 2012)
  stopifnot(compareGeom(r1990, r2012, stopOnError = FALSE))
  agri_to_foret_90_12 <- count_true((r1990 == 2) & (r2012 == 3))
  foret_to_agri_90_12 <- count_true((r1990 == 3) & (r2012 == 2))
  # (1990 -> 2000)
  stopifnot(compareGeom(r1990, r2000, stopOnError = FALSE))
  agri_to_foret_90_00 <- count_true((r1990 == 2) & (r2000 == 3))
  foret_to_agri_90_00 <- count_true((r1990 == 3) & (r2000 == 2))
  # (2000 -> 2012)
  stopifnot(compareGeom(r2000, r2012, stopOnError = FALSE))
  agri_to_foret_00_12 <- count_true((r2000 == 2) & (r2012 == 3))
  foret_to_agri_00_12 <- count_true((r2000 == 3) & (r2012 == 2))

  # Différences
  # (2012 - 1990)
  diff_agri  <- agri_2012  - agri_1990
  diff_foret <- foret_2012 - foret_1990
  # (2000 - 1990)
  diff_agri_00_90  <- agri_2000  - agri_1990
  diff_foret_00_90 <- foret_2000 - foret_1990
  # (2012 - 2000)
  diff_agri_12_00  <- agri_2012  - agri_2000
  diff_foret_12_00 <- foret_2012 - foret_2000


  # --- Stocker ligne résultat ---
  res_list[[i]] <- data.frame(
    insee = com_code,
    nom   = com_name,
    pixels_total_1990 = pixels_total_1990,
    pixels_total_2000 = pixels_total_2000,
    pixels_total_2012 = pixels_total_2012,
    agri_1990 = agri_1990,
    agri_2000 = agri_2000,
    agri_2012 = agri_2012,
    foret_1990 = foret_1990,
    foret_2000 = foret_2000,
    foret_2012 = foret_2012,
    agri_adj_foret_1990 = agri_adj_foret_1990,
    agri_adj_foret_2000 = agri_adj_foret_2000,
    agri_adj_foret_2012 = agri_adj_foret_2012,
    diff_agri_12_90 = diff_agri,
    diff_foret_12_90 = diff_foret,
    agri_to_foret_90_12 = agri_to_foret_90_12,
    foret_to_agri_90_12 = foret_to_agri_90_12,
    diff_agri_00_90 = diff_agri_00_90,
    diff_foret_00_90 = diff_foret_00_90,
    agri_to_foret_90_00 = agri_to_foret_90_00,
    foret_to_agri_90_00 = foret_to_agri_90_00,
    agri_to_foret_00_12 = agri_to_foret_00_12,
    foret_to_agri_00_12 = foret_to_agri_00_12,
    diff_agri_12_00 = diff_agri_12_00,
    diff_foret_12_00 = diff_foret_12_00
  )

  if (i %% 100 == 0) cat("... commune", i, "/", nrow(idf_sf), "\n")
}

# Compter le nombre de colonnes pour chaque élément de la liste
col_counts <- sapply(res_list, ncol)

# Trouver l'index des éléments qui n'ont pas le même nombre de colonnes que le premier
problemes <- which(col_counts != col_counts[1])

if(length(problemes) > 0) {
  cat("Les éléments suivants posent problème :", problemes, "\n")
  print(res_list[[problemes[1]]]) # Voir à quoi ressemble une ligne erronée
}

res <- do.call(rbind, res_list)

# Ajout de la productivité agricole
agri1988 =  read_parquet("agri1988.parquet") %>% rename(id = com) %>% mutate(time = 1) %>% select("id","time","ratio_prod_surface","production","superficie")
agri2000 = read_parquet("agri2000.parquet") %>% rename(id = com) %>% mutate(time = 2) %>% select("id","time","ratio_prod_surface","production","superficie")
agri2010 = read_parquet("agri2010.parquet") %>% rename(id = com) %>% mutate(time = 3) %>% select("id","time","ratio_prod_surface","production","superficie")


df_prod = agri1988 %>%
  bind_rows(agri2000) %>%
  bind_rows(agri2010)

# Wide: prod_1990 (time=1), prod_2000 (time=2), prod_2012 (time=3) + delta
df_prod_wide <- df_prod %>%
  filter(time %in% c(1, 2, 3)) %>%
  select(id, time, ratio_prod_surface) %>%
  distinct() %>%
  pivot_wider(
    names_from = time,
    values_from = ratio_prod_surface,
    names_prefix = "t"
  ) %>%
  rename(
    prod_1990 = t1,
    prod_2000 = t2,
    prod_2012 = t3
  ) %>%
  mutate(
    d_prod_12_90 = prod_2012 - prod_1990,
    d_prod_12_00 = prod_2012 - prod_2000,
    d_prod_00_90 = prod_2000 - prod_1990
  )

df_cluster <- res

# Joindre au niveau commune (utile si tu veux aussi faire des boxplots etc.)
df_cluster_prod <- df_cluster %>%
  left_join(df_prod_wide, by = c("insee" = "id"))

# Sauvegarde CSV
csv_path <- file.path(out_dir, "indicateurs_communes_clc_1990_2000_2012.csv")
write.csv(res, csv_path, row.names = FALSE)
cat("OK -> CSV :", csv_path, "\n")

# petit aperçu
print(head(res, 10))
