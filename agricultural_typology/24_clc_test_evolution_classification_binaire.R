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

clc2012_tif <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/Results/u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif"

clc2018_tif <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/Results/u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif"

out_csv <- "data/evolution_type_culture_communes_2012_2018.csv"
out_map_2012 <- "plots/carte_type_culture_2012.png"
out_map_2018 <- "plots/carte_type_culture_2018.png"
out_map_delta <- "plots/carte_evolution_type_culture_2012_2018.png"

if (!file.exists(gpkg_path)) stop("GPKG introuvable : ", gpkg_path)
if (!file.exists(clc2012_tif)) stop("Raster 2012 introuvable : ", clc2012_tif)
if (!file.exists(clc2018_tif)) stop("Raster 2018 introuvable : ", clc2018_tif)

# ============================================================
# 1) Codes CLC
# ============================================================

# Raster CLC codé en classes séquentielles 1..44
# 12 = Non-irrigated arable land
# 13 = Permanently irrigated land
# 14 = Rice fields
# 15 = Vineyards
# 16 = Fruit trees and berry plantations
# 17 = Olive groves
# 18 = Pastures
# 19 = Annual crops associated with permanent crops
# 20 = Complex cultivation patterns
# 21 = Land principally occupied by agriculture...
# 22 = Agro-forestry areas

agri_codes <- c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
annual_codes <- c(12, 13, 14)
perennial_codes <- c(15, 16, 17)

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
# 3) Fonction utilitaire : surface d'un raster binaire dans un polygone
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
# 4) Fonction principale : calcule les indicateurs pour une année
# ============================================================

compute_type_indicators <- function(clc_path, communes_sf, year_label) {
  message("===================================================")
  message("Calcul des indicateurs pour ", year_label)
  message("Raster : ", clc_path)

  clc <- rast(clc_path)

  # Projection des communes dans le CRS du raster
  communes_v <- vect(communes_sf)
  communes_v <- project(communes_v, crs(clc))
  communes_proj_sf <- st_as_sf(communes_v)

  # surface des communes en hectares
  communes_proj_sf$surf_commune_ha <- as.numeric(st_area(communes_proj_sf)) / 10000

  # surface d'un pixel en ha
  cell_area_ha <- prod(res(clc)) / 10000

  message("CRS raster : ", crs(clc))
  message("Résolution raster : ", paste(res(clc), collapse = " x "))
  message("Surface pixel (ha) : ", cell_area_ha)

  dept_list <- sort(unique(communes_proj_sf$code_dept))
  res_list <- vector("list", length(dept_list))

  for (i in seq_along(dept_list)) {
    dept <- dept_list[i]
    message(sprintf("[%d/%d] %s - département %s", i, length(dept_list), year_label, dept))

    com_dept_sf <- communes_proj_sf %>% filter(code_dept == dept)
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

      surf_ann_per_ha <- surf_annual_ha + surf_perennial_ha

      share_annual <- ifelse(surf_ann_per_ha > 0, surf_annual_ha / surf_ann_per_ha, NA_real_)
      share_perennial <- ifelse(surf_ann_per_ha > 0, surf_perennial_ha / surf_ann_per_ha, NA_real_)
      score_type <- ifelse(
        surf_ann_per_ha > 0,
        (surf_annual_ha - surf_perennial_ha) / surf_ann_per_ha,
        NA_real_
      )

      dept_res[[j]] <- data.frame(
        code_commune      = com_sf$code_commune,
        nom_commune       = com_sf$nom_commune,
        code_dept         = com_sf$code_dept,
        surf_commune_ha   = com_sf$surf_commune_ha,
        surf_agri_ha      = surf_agri_ha,
        surf_annual_ha    = surf_annual_ha,
        surf_perennial_ha = surf_perennial_ha,
        share_annual      = share_annual,
        share_perennial   = share_perennial,
        score_type        = score_type
      )
    }

    res_list[[i]] <- bind_rows(dept_res)

    rm(clc_dept, agri_dept, annual_dept, perennial_dept, dept_res)
    gc()
  }

  bind_rows(res_list)
}

# ============================================================
# 5) Calcul 2012 et 2018
# ============================================================

res_2012 <- compute_type_indicators(clc2012_tif, communes_sf, "2012") %>%
  rename(
    surf_agri_ha_2012 = surf_agri_ha,
    surf_annual_ha_2012 = surf_annual_ha,
    surf_perennial_ha_2012 = surf_perennial_ha,
    share_annual_2012 = share_annual,
    share_perennial_2012 = share_perennial,
    score_type_2012 = score_type
  )

res_2018 <- compute_type_indicators(clc2018_tif, communes_sf, "2018") %>%
  rename(
    surf_agri_ha_2018 = surf_agri_ha,
    surf_annual_ha_2018 = surf_annual_ha,
    surf_perennial_ha_2018 = surf_perennial_ha,
    share_annual_2018 = share_annual,
    share_perennial_2018 = share_perennial,
    score_type_2018 = score_type
  )

# ============================================================
# 6) Fusion et évolution
# ============================================================

carte <- communes_sf %>%
  left_join(
    res_2012,
    by = c("code_commune", "nom_commune", "code_dept")
  ) %>%
  left_join(
    res_2018,
    by = c("code_commune", "nom_commune", "code_dept")
  ) %>%
  mutate(
    delta_score_type = score_type_2018 - score_type_2012,
    delta_share_annual = share_annual_2018 - share_annual_2012,
    delta_share_perennial = share_perennial_2018 - share_perennial_2012
  )

# ============================================================
# 7) Export CSV
# ============================================================

carte %>%
  st_drop_geometry() %>%
  select(
    code_commune,
    nom_commune,
    code_dept,
    starts_with("surf_"),
    starts_with("share_"),
    starts_with("score_"),
    starts_with("delta_")
  ) %>%
  write.csv(out_csv, row.names = FALSE)

# ============================================================
# 8) Couleurs pour score de type
# ============================================================

make_type_color <- function(score) {
  if (is.na(score)) return("#BDBDBD")

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

carte$type_color_2012 <- vapply(carte$score_type_2012, make_type_color, character(1))
carte$type_color_2018 <- vapply(carte$score_type_2018, make_type_color, character(1))

# ============================================================
# 9) Carte type 2012
# ============================================================

p_2012 <- ggplot(carte) +
  geom_sf(aes(fill = type_color_2012), color = NA) +
  scale_fill_identity() +
  labs(
    title = "Dominante annuelle vs pérenne en 2012",
    subtitle = "Rouge = plus annuelle ; bleu = plus pérenne ; blanc = équilibre ; gris = absence",
    caption = "CLC 2012 ; annuelles = 12-14 ; pérennes = 15-17"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9)
  )

# ============================================================
# 10) Carte type 2018
# ============================================================

p_2018 <- ggplot(carte) +
  geom_sf(aes(fill = type_color_2018), color = NA) +
  scale_fill_identity() +
  labs(
    title = "Dominante annuelle vs pérenne en 2018",
    subtitle = "Rouge = plus annuelle ; bleu = plus pérenne ; blanc = équilibre ; gris = absence",
    caption = "CLC 2018 ; annuelles = 12-14 ; pérennes = 15-17"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9)
  )

# ============================================================
# 11) Carte évolution 2012 -> 2018
# ============================================================

p_delta <- ggplot(carte) +
  geom_sf(aes(fill = delta_score_type), color = NA) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#D73027",
    midpoint = 0,
    na.value = "#BDBDBD",
    name = "Delta score"
  ) +
  labs(
    title = "Évolution du type de culture entre 2012 et 2018",
    subtitle = "Rouge = évolution vers l'annuel ; bleu = évolution vers le pérenne ; blanc = stabilité",
    caption = "Delta = score 2018 - score 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9),
    legend.position = "bottom"
  )

# ============================================================
# 12) Sauvegarde
# ============================================================

ggsave(out_map_2012, p_2012, width = 10, height = 12, dpi = 300, bg = "white")
ggsave(out_map_2018, p_2018, width = 10, height = 12, dpi = 300, bg = "white")
ggsave(out_map_delta, p_delta, width = 10, height = 12, dpi = 300, bg = "white")

print(p_2012)
print(p_2018)
print(p_delta)

cat("\nOK -> CSV et cartes exportés\n")