suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

options(warn = 1)

# ============================================================
# 0) Chemins
# ============================================================

gpkg_path <- "data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"

clc2018_tif <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/Results/clc_cache/u2018_clc2018_v2020_20u1_raster100m/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"

out_csv <- "data/carte_france_surface_agricole_clc2018_communes.csv"

out_map_binary <- "plots/carte_agricole_binaire_clc2018.png"
out_map_intensity <- "plots/carte_agricole_intensite_clc2018.png"
out_map_type <- "plots/carte_type_culture_annuelle_perenne_clc2018.png"

if (!file.exists(gpkg_path)) stop("GPKG introuvable : ", gpkg_path)
if (!file.exists(clc2018_tif)) stop("Raster introuvable : ", clc2018_tif)

# ============================================================
# 1) Codes CLC
# ============================================================

# Raster CLC codé en classes séquentielles 1..44
# Agriculture = classes 12 à 22
agri_codes <- c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

# Cultures annuelles "pures"
annual_codes <- c(12, 13, 14)

# Cultures pérennes "pures"
perennial_codes <- c(15, 16, 17)

# seuil pour la carte binaire
binary_threshold <- 0.05

# ============================================================
# 2) Lecture communes
# ============================================================

communes_raw <- st_read(gpkg_path, layer = "COMMUNE", quiet = TRUE)

communes_sf <- communes_raw %>%
  mutate(
    code_commune = as.character(code_insee),
    nom_commune  = as.character(nom_officiel),
    code_dept    = as.character(code_insee_du_departement)
  ) %>%
  select(code_commune, nom_commune, code_dept, geometrie) %>%
  filter(!code_dept %in% c("971", "972", "973", "974", "976"))

# ============================================================
# 3) Raster CLC
# ============================================================

clc <- rast(clc2018_tif)

# projection des communes dans le CRS du raster
communes_v <- vect(communes_sf)
communes_v <- project(communes_v, crs(clc))
communes_sf <- st_as_sf(communes_v)

# surface des communes en ha
communes_sf$surf_commune_ha <- as.numeric(st_area(communes_sf)) / 10000

# surface pixel en ha
cell_area_ha <- prod(res(clc)) / 10000

# ============================================================
# 4) Fonction utilitaire
# ============================================================

area_ha_in_poly <- function(bin_raster, poly_vect, cell_area_ha) {
  r <- crop(bin_raster, poly_vect)
  if (is.null(r) || ncell(r) == 0) return(0)

  r <- mask(r, poly_vect)
  n <- global(r == 1, "sum", na.rm = TRUE)[1, 1]
  if (is.na(n)) n <- 0
  n * cell_area_ha
}

# ============================================================
# 5) Boucle par département
# ============================================================

dept_list <- sort(unique(communes_sf$code_dept))
res_list <- vector("list", length(dept_list))

for (i in seq_along(dept_list)) {
  dept <- dept_list[i]
  message(sprintf("[%d/%d] Département %s", i, length(dept_list), dept))

  com_dept_sf <- communes_sf %>% filter(code_dept == dept)
  if (nrow(com_dept_sf) == 0) next

  com_dept_v <- vect(com_dept_sf)

  clc_dept <- try(crop(clc, com_dept_v), silent = TRUE)

  if (inherits(clc_dept, "try-error") || is.null(clc_dept) || ncell(clc_dept) == 0) {
    res_list[[i]] <- data.frame(
      code_commune      = com_dept_sf$code_commune,
      nom_commune       = com_dept_sf$nom_commune,
      code_dept         = com_dept_sf$code_dept,
      surf_commune_ha   = com_dept_sf$surf_commune_ha,
      surf_agri_ha      = 0,
      surf_annual_ha    = 0,
      surf_perennial_ha = 0
    )
    next
  }

  agri_dept <- ifel(clc_dept %in% agri_codes, 1, 0)
  annual_dept <- ifel(clc_dept %in% annual_codes, 1, 0)
  perennial_dept <- ifel(clc_dept %in% perennial_codes, 1, 0)

  dept_res <- vector("list", nrow(com_dept_sf))

  for (j in seq_len(nrow(com_dept_sf))) {
    com_sf <- com_dept_sf[j, ]
    com_v  <- com_dept_v[j]

    surf_agri_ha <- try(area_ha_in_poly(agri_dept, com_v, cell_area_ha), silent = TRUE)
    surf_annual_ha <- try(area_ha_in_poly(annual_dept, com_v, cell_area_ha), silent = TRUE)
    surf_perennial_ha <- try(area_ha_in_poly(perennial_dept, com_v, cell_area_ha), silent = TRUE)

    if (inherits(surf_agri_ha, "try-error")) surf_agri_ha <- 0
    if (inherits(surf_annual_ha, "try-error")) surf_annual_ha <- 0
    if (inherits(surf_perennial_ha, "try-error")) surf_perennial_ha <- 0

    dept_res[[j]] <- data.frame(
      code_commune      = com_sf$code_commune,
      nom_commune       = com_sf$nom_commune,
      code_dept         = com_sf$code_dept,
      surf_commune_ha   = com_sf$surf_commune_ha,
      surf_agri_ha      = surf_agri_ha,
      surf_annual_ha    = surf_annual_ha,
      surf_perennial_ha = surf_perennial_ha
    )
  }

  res_list[[i]] <- bind_rows(dept_res)

  rm(clc_dept, agri_dept, annual_dept, perennial_dept, dept_res)
  gc()
}

stats_all <- bind_rows(res_list)

# ============================================================
# 6) Indicateurs finaux
# ============================================================

carte <- communes_sf %>%
  left_join(
    stats_all,
    by = c("code_commune", "nom_commune", "code_dept", "surf_commune_ha")
  ) %>%
  mutate(
    surf_agri_ha      = coalesce(surf_agri_ha, 0),
    surf_annual_ha    = coalesce(surf_annual_ha, 0),
    surf_perennial_ha = coalesce(surf_perennial_ha, 0),

    part_agri_commune = if_else(
      surf_commune_ha > 0,
      pmin(pmax(surf_agri_ha / surf_commune_ha, 0), 1),
      NA_real_
    ),

    part_agri_sqrt = sqrt(part_agri_commune),

    surf_ann_per_ha = surf_annual_ha + surf_perennial_ha,

    share_annual = if_else(
      surf_ann_per_ha > 0,
      surf_annual_ha / surf_ann_per_ha,
      NA_real_
    ),

    share_perennial = if_else(
      surf_ann_per_ha > 0,
      surf_perennial_ha / surf_ann_per_ha,
      NA_real_
    ),

    score_type = case_when(
      surf_ann_per_ha <= 0 ~ NA_real_,
      TRUE ~ (surf_annual_ha - surf_perennial_ha) / surf_ann_per_ha
    ),

    score_type_enhanced = case_when(
      is.na(score_type) ~ NA_real_,
      TRUE ~ sign(score_type) * abs(score_type)^0.5
    ),

    commune_agricole = part_agri_commune >= binary_threshold
  )

# ============================================================
# 7) Palette rouge / blanc / bleu
# ============================================================

make_type_color <- function(score) {
  if (is.na(score)) return("#BDBDBD")  # gris si pas d'annuelles/pérennes

  s <- max(min(score, 1), -1)

  red_base   <- c(215, 48, 39)
  blue_base  <- c(49, 130, 189)
  white_base <- c(255, 255, 255)

  if (s >= 0) {
    rgb_base <- (1 - s) * white_base + s * red_base
  } else {
    t <- abs(s)
    rgb_base <- (1 - t) * white_base + t * blue_base
  }

  rgb(rgb_base[1], rgb_base[2], rgb_base[3], maxColorValue = 255)
}

carte$type_color <- vapply(carte$score_type_enhanced, make_type_color, character(1))

# ============================================================
# 8) Export CSV
# ============================================================

carte %>%
  st_drop_geometry() %>%
  select(
    code_commune,
    nom_commune,
    code_dept,
    surf_commune_ha,
    surf_agri_ha,
    surf_annual_ha,
    surf_perennial_ha,
    part_agri_commune,
    share_annual,
    share_perennial,
    score_type,
    commune_agricole
  ) %>%
  write.csv(out_csv, row.names = FALSE)

# ============================================================
# 9) Carte 1 : binaire agriculture / pas agriculture
# ============================================================

p_binary <- ggplot(carte) +
  geom_sf(aes(fill = commune_agricole), color = NA) +
  scale_fill_manual(
    values = c("FALSE" = "white", "TRUE" = "#2E7D32"),
    name = NULL,
    labels = c("FALSE" = paste0("< ", binary_threshold * 100, "% surface agricole"),
              "TRUE"  = paste0(">= ", binary_threshold * 100, "% surface agricole"))
  ) +
  labs(
    title = "Communes avec présence significative d'agriculture",
    subtitle = paste0("Commune colorée si au moins ", binary_threshold * 100, "% de sa surface est agricole"),
    caption = "CLC 2018 ; agriculture = classes raster 12 à 22"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9),
    legend.position = "bottom"
  )

# ============================================================
# 10) Carte 2 : heatmap intensité agricole
# ============================================================

p_intensity <- ggplot(carte) +
  geom_sf(aes(fill = part_agri_sqrt), color = NA) +
  scale_fill_gradient(
    low = "white",
    high = "#1B5E20",
    na.value = "grey90",
    labels = percent_format(accuracy = 1),
    name = "Part agricole\n(échelle sqrt)"
  ) +
  labs(
    title = "Part de la surface communale occupée par l'agriculture",
    subtitle = "Plus foncé = plus grande proportion de surface agricole",
    caption = "CLC 2018 ; agriculture = classes raster 12 à 22"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9),
    legend.position = "bottom"
  )

# ============================================================
# 11) Carte 3 : type de culture annuelle vs pérenne
# ============================================================

p_type <- ggplot(carte) +
  geom_sf(aes(fill = type_color), color = NA) +
  scale_fill_identity() +
  labs(
    title = "Dominante annuelle vs pérenne",
    subtitle = "Rouge = plus annuelle ; bleu = plus pérenne ; blanc = équilibre ; gris = absence de ces cultures",
    caption = "CLC 2018 ; annuelles = 12-14 ; pérennes = 15-17"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9)
  )

# ============================================================
# 12) Sauvegarde
# ============================================================

ggsave(out_map_binary, p_binary, width = 10, height = 12, dpi = 300, bg = "white")
ggsave(out_map_intensity, p_intensity, width = 10, height = 12, dpi = 300, bg = "white")
ggsave(out_map_type, p_type, width = 10, height = 12, dpi = 300, bg = "white")

print(p_binary)
print(p_intensity)
print(p_type)

cat("\nOK -> cartes et csv exportés\n")