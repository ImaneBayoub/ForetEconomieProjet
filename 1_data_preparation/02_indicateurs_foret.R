# =============================================================================
# 02_indicateurs_foret.R
# Objectif :
#   Construire des indicateurs communaux d'occupation des sols à partir de CLC :
#   - pixels agricoles
#   - pixels forestiers/semi-naturels
#   - pixels agricoles adjacents à de la forêt, appelés "lisière"
#   - transitions agricole <-> forêt entre 1990, 2000 et 2012
#
# Sortie :
#   data/interim/clc_commune_indicateurs.csv
# =============================================================================

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

message_step("Construction des indicateurs CLC communaux")

# -----------------------------------------------------------------------------
# 1. Chemins
# -----------------------------------------------------------------------------

gpkg_path <- path("data", "raw", "communes", "ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg")

clc1990_tif <- path("data", "raw", "clc", "U2000_CLC1990_V2020_20u1.tif")
clc2000_tif <- path("data", "raw", "clc", "U2006_CLC2000_V2020_20u1.tif")
clc2012_tif <- path("data", "raw", "clc", "U2018_CLC2012_V2020_20u1.tif")

required_files <- c(gpkg_path, clc1990_tif, clc2000_tif, clc2012_tif)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Fichiers introuvables :\n",
    paste(missing_files, collapse = "\n")
  )
}

# -----------------------------------------------------------------------------
# 2. Lecture communes
# -----------------------------------------------------------------------------

message_step("Lecture des communes IGN")

communes_sf <- sf::st_read(gpkg_path, layer = "COMMUNE", quiet = TRUE) %>%
  sf::st_make_valid() %>%
  mutate(
    insee = as.character(code_insee),
    nom = as.character(nom_officiel)
  ) %>%
  filter(!is.na(insee), insee != "")

stopifnot(nrow(communes_sf) > 0)

# On ne garde que les colonnes utiles.
# La syntaxe sf[, c(...)] conserve automatiquement la géométrie active
communes_sf <- communes_sf[, c("insee", "nom")]

# -----------------------------------------------------------------------------
# 3. Lecture et alignement des rasters
# -----------------------------------------------------------------------------

message_step("Lecture des rasters CLC")

clc1990 <- terra::rast(clc1990_tif)
clc2000 <- terra::rast(clc2000_tif)
clc2012 <- terra::rast(clc2012_tif)

message_step("Alignement des rasters CLC sur la géométrie de 2012")

if (!terra::compareGeom(clc2012, clc1990, stopOnError = FALSE)) {
  clc1990 <- terra::resample(clc1990, clc2012, method = "near")
}

if (!terra::compareGeom(clc2012, clc2000, stopOnError = FALSE)) {
  clc2000 <- terra::resample(clc2000, clc2012, method = "near")
}

# -----------------------------------------------------------------------------
# 4. Recodage CLC et fonctions auxiliaires
# -----------------------------------------------------------------------------

# Recodage CLC agrégé :
# 1-11  : artificiel
# 12-22 : agricole
# 23-33 : forêt / milieux semi-naturels
rcl <- matrix(
  c(
    1,  11, 1,
    12, 22, 2,
    23, 33, 3
  ),
  ncol = 3,
  byrow = TRUE
)

# Voisinage 8 directions : un pixel agricole est en lisière s'il touche au moins
# un pixel forestier autour de lui.
w <- matrix(1, 3, 3)
w[2, 2] <- 0

count_true <- function(x) {
  val <- terra::global(x, "sum", na.rm = TRUE)[1, 1]
  ifelse(is.na(val), 0, as.numeric(val))
}

count_adjacent <- function(r3, target_class, neigh_class, w) {
  neigh_bin <- r3 == neigh_class
  neigh_sum <- terra::focal(
    neigh_bin,
    w = w,
    fun = sum,
    na.rm = TRUE,
    fillvalue = 0
  )
  adj <- (r3 == target_class) & (neigh_sum > 0)
  count_true(adj)
}

safe_ratio <- function(num, den) {
  ifelse(!is.na(den) & den > 0, 100 * num / den, NA_real_)
}

# -----------------------------------------------------------------------------
# 5. Boucle communale
# -----------------------------------------------------------------------------

message_step("Calcul des indicateurs par commune")

res_list <- vector("list", nrow(communes_sf))

for (i in seq_len(nrow(communes_sf))) {
  
  com <- communes_sf[i, ]
  
  com_code <- as.character(com$insee[1])
  com_name <- as.character(com$nom[1])
  
  if (length(com_code) == 0 || is.na(com_code) || trimws(com_code) == "") {
    com_code <- sprintf("row_%05d", i)
  }
  
  if (length(com_name) == 0 || is.na(com_name) || trimws(com_name) == "") {
    com_name <- com_code
  }
  
  com_vect <- terra::vect(com)
  com_proj <- terra::project(com_vect, terra::crs(clc2012))
  
  r12 <- terra::mask(terra::crop(clc2012, com_proj), com_proj)
  r02 <- terra::mask(terra::crop(clc2000, com_proj), com_proj)
  r90 <- terra::mask(terra::crop(clc1990, com_proj), com_proj)
  
  r2012 <- terra::classify(r12, rcl, others = NA)
  r2000 <- terra::classify(r02, rcl, others = NA)
  r1990 <- terra::classify(r90, rcl, others = NA)
  
  nvalid12 <- count_true(!is.na(r2012))
  nvalid02 <- count_true(!is.na(r2000))
  nvalid90 <- count_true(!is.na(r1990))
  
  if (nvalid12 == 0 || nvalid02 == 0 || nvalid90 == 0) {
    res_list[[i]] <- data.frame(
      insee = com_code,
      nom = com_name,
      pixels_total_1990 = nvalid90,
      pixels_total_2000 = nvalid02,
      pixels_total_2012 = nvalid12,
      agri_1990 = 0,
      agri_2000 = 0,
      agri_2012 = 0,
      foret_1990 = 0,
      foret_2000 = 0,
      foret_2012 = 0,
      agri_adj_foret_1990 = 0,
      agri_adj_foret_2000 = 0,
      agri_adj_foret_2012 = 0,
      agri_to_foret_90_00 = 0,
      agri_to_foret_00_12 = 0,
      agri_to_foret_90_12 = 0,
      foret_to_agri_90_00 = 0,
      foret_to_agri_00_12 = 0,
      foret_to_agri_90_12 = 0
    )
    next
  }
  
  agri_1990 <- count_true(r1990 == 2)
  agri_2000 <- count_true(r2000 == 2)
  agri_2012 <- count_true(r2012 == 2)
  
  foret_1990 <- count_true(r1990 == 3)
  foret_2000 <- count_true(r2000 == 3)
  foret_2012 <- count_true(r2012 == 3)
  
  agri_adj_foret_1990 <- count_adjacent(r1990, 2, 3, w)
  agri_adj_foret_2000 <- count_adjacent(r2000, 2, 3, w)
  agri_adj_foret_2012 <- count_adjacent(r2012, 2, 3, w)
  
  agri_to_foret_90_00 <- count_true((r1990 == 2) & (r2000 == 3))
  agri_to_foret_00_12 <- count_true((r2000 == 2) & (r2012 == 3))
  agri_to_foret_90_12 <- count_true((r1990 == 2) & (r2012 == 3))
  
  foret_to_agri_90_00 <- count_true((r1990 == 3) & (r2000 == 2))
  foret_to_agri_00_12 <- count_true((r2000 == 3) & (r2012 == 2))
  foret_to_agri_90_12 <- count_true((r1990 == 3) & (r2012 == 2))
  
  res_list[[i]] <- data.frame(
    insee = com_code,
    nom = com_name,
    
    pixels_total_1990 = nvalid90,
    pixels_total_2000 = nvalid02,
    pixels_total_2012 = nvalid12,
    
    agri_1990 = agri_1990,
    agri_2000 = agri_2000,
    agri_2012 = agri_2012,
    
    foret_1990 = foret_1990,
    foret_2000 = foret_2000,
    foret_2012 = foret_2012,
    
    agri_adj_foret_1990 = agri_adj_foret_1990,
    agri_adj_foret_2000 = agri_adj_foret_2000,
    agri_adj_foret_2012 = agri_adj_foret_2012,
    
    agri_to_foret_90_00 = agri_to_foret_90_00,
    agri_to_foret_00_12 = agri_to_foret_00_12,
    agri_to_foret_90_12 = agri_to_foret_90_12,
    
    foret_to_agri_90_00 = foret_to_agri_90_00,
    foret_to_agri_00_12 = foret_to_agri_00_12,
    foret_to_agri_90_12 = foret_to_agri_90_12
  )
  
  if (i %% 100 == 0) {
    message("... commune ", i, " / ", nrow(communes_sf))
  }
}

clc_indicateurs <- dplyr::bind_rows(res_list)

# -----------------------------------------------------------------------------
# 6. Ajout des différences et pourcentages
# -----------------------------------------------------------------------------

clc_indicateurs <- clc_indicateurs %>%
  mutate(
    diff_agri_00_90 = agri_2000 - agri_1990,
    diff_agri_12_00 = agri_2012 - agri_2000,
    diff_agri_12_90 = agri_2012 - agri_1990,
    
    diff_foret_00_90 = foret_2000 - foret_1990,
    diff_foret_12_00 = foret_2012 - foret_2000,
    diff_foret_12_90 = foret_2012 - foret_1990,
    
    pct_agri_1990 = safe_ratio(agri_1990, pixels_total_1990),
    pct_agri_2000 = safe_ratio(agri_2000, pixels_total_2000),
    pct_agri_2012 = safe_ratio(agri_2012, pixels_total_2012),
    
    pct_foret_1990 = safe_ratio(foret_1990, pixels_total_1990),
    pct_foret_2000 = safe_ratio(foret_2000, pixels_total_2000),
    pct_foret_2012 = safe_ratio(foret_2012, pixels_total_2012),
    
    pct_lisiere_1990 = safe_ratio(agri_adj_foret_1990, pixels_total_1990),
    pct_lisiere_2000 = safe_ratio(agri_adj_foret_2000, pixels_total_2000),
    pct_lisiere_2012 = safe_ratio(agri_adj_foret_2012, pixels_total_2012),
    
    pct_agri_to_foret_90_00 = safe_ratio(agri_to_foret_90_00, pixels_total_1990),
    pct_agri_to_foret_00_12 = safe_ratio(agri_to_foret_00_12, pixels_total_2000),
    pct_agri_to_foret_90_12 = safe_ratio(agri_to_foret_90_12, pixels_total_1990),
    
    pct_foret_to_agri_90_00 = safe_ratio(foret_to_agri_90_00, pixels_total_1990),
    pct_foret_to_agri_00_12 = safe_ratio(foret_to_agri_00_12, pixels_total_2000),
    pct_foret_to_agri_90_12 = safe_ratio(foret_to_agri_90_12, pixels_total_1990)
  )

# -----------------------------------------------------------------------------
# 7. Export
# -----------------------------------------------------------------------------

write_csv2(clc_indicateurs, path("data", "interim", "clc_commune_indicateurs.csv"))
write_parquet2(clc_indicateurs, path("data", "interim", "clc_commune_indicateurs.parquet"))
write_csv2(clc_summary, path("output", "tables", "clc_indicateurs_summary.csv"))

message("CLC indicateurs écrits dans data/interim/clc_commune_indicateurs.parquet")
