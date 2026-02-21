library(terra)
library(sf)

# --- Chemins ---
gpkg_path <- "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"

clc2012_tif <- "data/Results/u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif"

# Raster CLC 1990 (chemin que tu as donné)
clc1990_tif <- "/home/imane/Documents/ensae/ForetEconomieProjet/ForetEconomieProjet-1/data/Results/u2018_clc2012_v2020_20u1_raster100m/DATA/U2000_CLC1990_V2020_20u1.tif"

# --- Output ---
out_dir <- "out_communes"
img_dir <- file.path(out_dir, "images")
dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)

# --- Lire communes + filtre IDF ---
communes_sf <- st_read(gpkg_path, layer = "COMMUNE", quiet = TRUE)
idf_sf <- communes_sf[communes_sf$code_insee_de_la_region %in% c("11", 11), ]
stopifnot(nrow(idf_sf) > 0)

# --- Charger rasters une fois ---
clc2012 <- rast(clc2012_tif)
clc1990 <- rast(clc1990_tif)

# --- Aligner 1990 sur 2012 pour être sûr (géométrie identique) ---
# (important pour les transitions pixel à pixel)
if (!compareGeom(clc2012, clc1990, stopOnError = FALSE)) {
  clc1990 <- resample(clc1990, clc2012, method = "near")
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

get_first_existing <- function(x, candidates) {
  cols <- names(x)
  for (nm in candidates) {
    if (nm %in% cols) return(as.character(x[[nm]][1]))
  }
  return(NA_character_)
}

safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

# Palette
cols <- c("red", "yellow", "green")
labs <- c("Artificiel", "Agricole", "Forêt")

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
  r90 <- mask(crop(clc1990, com_proj), com_proj)

  # Recoder en 1/2/3
  r2012 <- classify(r12, rcl, others = NA)
  r1990 <- classify(r90, rcl, others = NA)

  # Si commune vide (pas de pixels), on skip proprement
  nvalid12 <- global(!is.na(r2012), "sum", na.rm = TRUE)[1, 1]
  nvalid90 <- global(!is.na(r1990), "sum", na.rm = TRUE)[1, 1]
  if (is.na(nvalid12) || nvalid12 == 0 || is.na(nvalid90) || nvalid90 == 0) {
    res_list[[i]] <- data.frame(
      insee = com_code,
      nom   = com_name,
      agri_1990 = 0, agri_2012 = 0,
      foret_1990 = 0, foret_2012 = 0,
      agri_adj_foret_1990 = 0, agri_adj_foret_2012 = 0,
      diff_agri_12_90 = 0,
      diff_foret_12_90 = 0,
      agri_to_foret = 0,
      foret_to_agri = 0
    )
    next
  }

  # Comptages agricoles / forêt
  agri_1990  <- count_true(r1990 == 2)
  agri_2012  <- count_true(r2012 == 2)
  foret_1990 <- count_true(r1990 == 3)
  foret_2012 <- count_true(r2012 == 3)

  # Agricole adjacent forêt
  agri_adj_foret_1990 <- count_adjacent(r1990, 2, 3, w)
  agri_adj_foret_2012 <- count_adjacent(r2012, 2, 3, w)

  # Transitions (1990 -> 2012)
  stopifnot(compareGeom(r1990, r2012, stopOnError = FALSE))
  agri_to_foret <- count_true((r1990 == 2) & (r2012 == 3))
  foret_to_agri <- count_true((r1990 == 3) & (r2012 == 2))

  # Différences (2012 - 1990)
  diff_agri  <- agri_2012  - agri_1990
  diff_foret <- foret_2012 - foret_1990

  # --- Image par commune : nom + contour noir ---
  fname <- paste0(safe_filename(com_code), "_", safe_filename(com_name))
  png(file.path(img_dir, paste0(fname, "_1990_2012.png")), width = 1600, height = 800, res = 150)
  par(mfrow = c(1, 2), mar = c(3, 3, 7, 7))

  # 1990
  plot(r1990, col = cols, breaks = c(0.5, 1.5, 2.5, 3.5), legend = FALSE,
       main = "Classe dominante en 1990")
  lines(com_proj, col = "black", lwd = 2)
  mtext(paste0(com_name, " (", com_code, ")"), side = 3, line = 0.2, cex = 1.0, font = 2)
  legend("right", legend = labs, fill = cols, bty = "n")

  # 2012
  plot(r2012, col = cols, breaks = c(0.5, 1.5, 2.5, 3.5), legend = FALSE,
       main = "Classe dominante en 2012")
  lines(com_proj, col = "black", lwd = 2)
  mtext(paste0(com_name, " (", com_code, ")"), side = 3, line = 0.2, cex = 1.0, font = 2)
  legend("right", legend = labs, fill = cols, bty = "n")

  dev.off()

  # --- Stocker ligne résultat ---
  res_list[[i]] <- data.frame(
    insee = com_code,
    nom   = com_name,
    agri_1990 = agri_1990,
    agri_2012 = agri_2012,
    foret_1990 = foret_1990,
    foret_2012 = foret_2012,
    agri_adj_foret_1990 = agri_adj_foret_1990,
    agri_adj_foret_2012 = agri_adj_foret_2012,
    diff_agri_12_90 = diff_agri,
    diff_foret_12_90 = diff_foret,
    agri_to_foret = agri_to_foret,
    foret_to_agri = foret_to_agri
  )

  if (i %% 100 == 0) cat("... commune", i, "/", nrow(idf_sf), "\n")
}

res <- do.call(rbind, res_list)

# Sauvegarde CSV
csv_path <- file.path(out_dir, "indicateurs_communes_idf_clc_1990_2012.csv")
write.csv(res, csv_path, row.names = FALSE)

cat("\nOK -> images:", img_dir, "\n")
cat("OK -> CSV :", csv_path, "\n")

# petit aperçu
print(head(res, 10))

# ============================================================
# Analyses globales : seuils + distributions (1 graphe/colonne)
# ============================================================

# Dossier pour les graphes de stats
stats_dir <- file.path(out_dir, "stats")
dir.create(stats_dir, recursive = TRUE, showWarnings = FALSE)

# Colonnes numériques à analyser
num_cols <- c(
  "agri_1990","agri_2012",
  "foret_1990","foret_2012",
  "agri_adj_foret_1990","agri_adj_foret_2012",
  "diff_agri_12_90","diff_foret_12_90",
  "agri_to_foret","foret_to_agri"
)

# ---- A) Histogramme pour chaque colonne ----
for (col in num_cols) {
  x <- res[[col]]
  x <- x[!is.na(x)]

  png(file.path(stats_dir, paste0("hist_", col, ".png")), width = 1200, height = 800, res = 150)
  hist(x, main = paste0("Distribution - ", col), xlab = col)
  dev.off()
}

# ---- B) Comptage des communes au-dessus de seuils ----
# Exemple de seuils (tu peux modifier)
thresholds <- c(0, 50, 100, 200, 500, 1000)

counts_over <- data.frame(colonne = character(), seuil = numeric(), n_communes = integer())

for (col in num_cols) {
  x <- res[[col]]
  for (t in thresholds) {
    n <- sum(x > t, na.rm = TRUE)
    counts_over <- rbind(counts_over, data.frame(colonne = col, seuil = t, n_communes = n))
  }
}

# Sauver tableau des seuils
write.csv(counts_over, file.path(stats_dir, "communes_depassement_seuils.csv"), row.names = FALSE)

# Graphe : pour chaque colonne, courbe "n communes > seuil"
for (col in num_cols) {
  sub <- counts_over[counts_over$colonne == col, ]

  png(file.path(stats_dir, paste0("seuils_", col, ".png")), width = 1200, height = 800, res = 150)
  plot(sub$seuil, sub$n_communes, type = "b",
       main = paste0("Communes avec ", col, " > seuil"),
       xlab = "Seuil (pixels)", ylab = "Nombre de communes")
  dev.off()
}

cat("\nOK -> Stats dans :", stats_dir, "\n")