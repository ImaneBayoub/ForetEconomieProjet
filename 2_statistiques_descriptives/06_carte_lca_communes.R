# -----------------------------------------------------------------------------
# 06_carte_lca_communes.R
# Cartes des clusters LCA et de l'incertitude de classification par commune
# -----------------------------------------------------------------------------
# Entrées :
#   output/tables/lca_communes_classes.csv
#   data/raw/admin_express/.../ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg
#
# Sorties :
#   output/figures/carte_lca_clusters.png
#   output/figures/carte_lca_incertitude.png
# -----------------------------------------------------------------------------

source("R/packages.R")
source("R/paths.R")
source("R/utils.R")

library(sf)
library(ggplot2)

message_step("Carte des clusters LCA par commune")

# ============================================================
# 1) Chemins
# ============================================================

classes_path <- path("output", "tables", "lca_communes_classes.csv")

gpkg_root <- path(
  "data", "raw", "admin_express",
  "ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20",
  "ADMIN-EXPRESS",
  "1_DONNEES_LIVRAISON_2025-11-00136",
  "ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20",
  "ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"
)

out_map_cluster <- path("output", "figures", "carte_lca_clusters.png")
out_map_uncert  <- path("output", "figures", "carte_lca_incertitude.png")

# ============================================================
# 2) Charger les classes LCA
# ============================================================

df_classes <- readr::read_csv(classes_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    insee = stringr::str_trim(as.character(insee)),
    insee = stringr::str_pad(insee, width = 5, side = "left", pad = "0"),
    classe = factor(classe)
  )

message("Nombre de communes dans le fichier de classes : ", nrow(df_classes))

# ============================================================
# 3) Lire la couche COMMUNE du GPKG
# ============================================================

communes_sf <- sf::st_read(gpkg_root, layer = "commune", quiet = TRUE)

communes_sf <- communes_sf %>%
  dplyr::mutate(
    insee = stringr::str_trim(as.character(code_insee)),
    insee = stringr::str_pad(insee, width = 5, side = "left", pad = "0")
  )

# ============================================================
# 4) Jointure carte + classes
# ============================================================

map_sf <- communes_sf %>%
  dplyr::left_join(
    df_classes %>% dplyr::select(insee, classe, prob_max, uncertainty),
    by = "insee"
  )

message("Total entités dans le fond : ", nrow(map_sf))
message("Communes avec cluster : ", sum(!is.na(map_sf$classe)))
message("Communes sans cluster  : ", sum(is.na(map_sf$classe)))

# ============================================================
# 5) Carte des clusters
# ============================================================

p_cluster <- ggplot2::ggplot(map_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = classe), color = NA, linewidth = 0) +
  ggplot2::scale_fill_brewer(
    palette = "Set2",
    na.value = "grey90",
    drop = FALSE
  ) +
  ggplot2::labs(
    title = "Classification LCA des communes",
    subtitle = "Cluster latent assigné à chaque commune",
    fill = "Classe"
  ) +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 16),
    plot.subtitle = ggplot2::element_text(size = 12),
    legend.position = "right",
    plot.background = ggplot2::element_rect(fill = "white", color = NA)
  )

ggplot2::ggsave(
  filename = out_map_cluster,
  plot = p_cluster,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "white"
)

message("Carte des clusters sauvegardée : ", out_map_cluster)

# ============================================================
# 6) Carte de l'incertitude
# ============================================================

p_uncert <- ggplot2::ggplot(map_sf) +
  ggplot2::geom_sf(ggplot2::aes(fill = uncertainty), color = NA, linewidth = 0) +
  ggplot2::scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey90"
  ) +
  ggplot2::labs(
    title = "Incertitude de classification LCA",
    subtitle = "1 - probabilité postérieure maximale",
    fill = "Incertitude"
  ) +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 16),
    plot.subtitle = ggplot2::element_text(size = 12),
    legend.position = "right",
    plot.background = ggplot2::element_rect(fill = "white", color = NA)
  )

ggplot2::ggsave(
  filename = out_map_uncert,
  plot = p_uncert,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "white"
)

message("Carte d'incertitude sauvegardée : ", out_map_uncert)
