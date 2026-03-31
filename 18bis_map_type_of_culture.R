suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(sf)
  library(ggplot2)
})

# ============================================================
# 1) Chemins
# ============================================================

classes_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_communes_classes.csv"

gpkg_path <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/admin_express/ADMIN-EXPRESS_4-0__GPKG_LAMB93_FXX_2025-11-20/ADMIN-EXPRESS/1_DONNEES_LIVRAISON_2025-11-00136/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20/ADE_4-0_GPKG_LAMB93_FXX-ED2025-11-20.gpkg"
gpkg_layer <- "COMMUNE"

out_map_cluster <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/map_lca_clusters_communes.png"
out_map_uncert  <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/map_lca_uncertainty_communes.png"

# ============================================================
# 2) Charger les classes LCA
# ============================================================

df_classes <- read_csv(classes_path, show_col_types = FALSE) %>%
  mutate(
    insee = str_trim(as.character(insee)),
    insee = str_pad(insee, width = 5, side = "left", pad = "0"),
    classe = factor(classe)
  )

cat("Nombre de communes dans le fichier de classes :", nrow(df_classes), "\n")

# ============================================================
# 3) Lire la couche COMMUNE du GPKG
# ============================================================

communes_sf <- st_read(gpkg_path, layer = gpkg_layer, quiet = TRUE)

cat("Colonnes disponibles dans la couche COMMUNE :\n")
print(names(communes_sf))

# ============================================================
# 4) Construire la clé de jointure
# ============================================================

if (!"code_insee" %in% names(communes_sf)) {
  stop("La colonne 'code_insee' n'existe pas dans la couche COMMUNE.")
}

communes_sf <- communes_sf %>%
  mutate(
    insee = str_trim(as.character(code_insee)),
    insee = str_pad(insee, width = 5, side = "left", pad = "0")
  )

# ============================================================
# 5) Jointure carte + classes
# ============================================================

map_sf <- communes_sf %>%
  left_join(
    df_classes %>% select(insee, classe, prob_max, uncertainty),
    by = "insee"
  )

cat("Nombre total d'entités dans le fond :", nrow(map_sf), "\n")
cat("Nombre de communes avec cluster :", sum(!is.na(map_sf$classe)), "\n")
cat("Nombre de communes sans cluster :", sum(is.na(map_sf$classe)), "\n")

# ============================================================
# 6) Contrôle des communes non appariées
# ============================================================

non_match <- map_sf %>%
  st_drop_geometry() %>%
  filter(is.na(classe)) %>%
  select(insee, nom_officiel)

cat("\nExemples de communes sans cluster :\n")
print(head(non_match, 20))

# ============================================================
# 7) Carte des clusters
# ============================================================

p_cluster <- ggplot(map_sf) +
  geom_sf(aes(fill = classe), color = NA, linewidth = 0) +
  scale_fill_brewer(
    palette = "Set2",
    na.value = "grey90",
    drop = FALSE
  ) +
  labs(
    title = "Classification LCA des communes",
    subtitle = "Cluster latent assigné à chaque commune",
    fill = "Classe"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = out_map_cluster,
  plot = p_cluster,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "white"
)

cat("\nCarte des clusters sauvegardée :", out_map_cluster, "\n")

# ============================================================
# 8) Carte de l'incertitude
# ============================================================

p_uncert <- ggplot(map_sf) +
  geom_sf(aes(fill = uncertainty), color = NA, linewidth = 0) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "grey90"
  ) +
  labs(
    title = "Incertitude de classification LCA",
    subtitle = "1 - probabilité postérieure maximale",
    fill = "Incertitude"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = out_map_uncert,
  plot = p_uncert,
  width = 10,
  height = 12,
  dpi = 300,
  bg = "white"
)

cat("Carte d'incertitude sauvegardée :", out_map_uncert, "\n")