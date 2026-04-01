suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

# ============================================================
# 1) Chemins
# ============================================================

classes_path  <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/lca_communes_classes.csv"
forest_path   <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/commune_cluster.csv"

out_heatmap_n   <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/heatmap_croisee_clusters_n_communes.png"
out_heatmap_pct <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/heatmap_croisee_clusters_pct_ligne.png"
out_table       <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/table_croisee_clusters.csv"

# ============================================================
# 2) Charger les deux clusterings
# ============================================================

# Clustering "type de commune / culture" (LCA)
df_lca <- read_csv(classes_path, show_col_types = FALSE) %>%
  mutate(
    insee = str_trim(as.character(insee)),
    insee = str_pad(insee, width = 5, side = "left", pad = "0")
  )

# Clustering "évolution forêt"
df_forest <- read.csv(forest_path, stringsAsFactors = FALSE) %>%
  mutate(
    insee = str_trim(as.character(insee)),
    insee = str_pad(insee, width = 5, side = "left", pad = "0")
  )

# ============================================================
# 3) Vérifier / renommer les colonnes cluster
# ============================================================

# Dans lca_communes_classes.csv, on suppose que la colonne s'appelle "classe"
if (!"classe" %in% names(df_lca)) {
  stop("La colonne 'classe' est absente de lca_communes_classes.csv")
}

# Dans commune_cluster.csv, on suppose que la colonne s'appelle "cluster"
if (!"cluster" %in% names(df_forest)) {
  stop("La colonne 'cluster' est absente de commune_cluster.csv")
}

df_lca <- df_lca %>%
  transmute(
    insee,
    cluster_culture = factor(classe)
  )

df_forest <- df_forest %>%
  transmute(
    insee,
    cluster_foret = factor(cluster)
  )

# ============================================================
# 4) Garder une seule ligne par commune dans chaque base
# ============================================================

df_lca <- df_lca %>%
  distinct(insee, .keep_all = TRUE)

df_forest <- df_forest %>%
  distinct(insee, .keep_all = TRUE)

# ============================================================
# 5) Jointure
# ============================================================

df_cross <- df_lca %>%
  inner_join(df_forest, by = "insee")

cat("Nombre de communes appariées :", nrow(df_cross), "\n")

# ============================================================
# 6) Table croisée
# ============================================================

tab_cross <- df_cross %>%
  count(cluster_foret, cluster_culture, name = "n_communes") %>%
  group_by(cluster_foret) %>%
  mutate(
    pct_ligne = 100 * n_communes / sum(n_communes)
  ) %>%
  ungroup()

write.csv(tab_cross, out_table, row.names = FALSE)
cat("Table croisée exportée :", out_table, "\n")

# ============================================================
# 7) Heatmap en nombre de communes
# ============================================================

p_n <- ggplot(tab_cross, aes(x = cluster_culture, y = cluster_foret, fill = n_communes)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = n_communes), size = 4) +
  scale_fill_gradient(
    low = "grey95",
    high = "navy"
  ) +
  labs(
    title = "Croisement des clusters culture × forêt",
    subtitle = "Couleur plus foncée = plus de communes dans l'intersection",
    x = "Cluster type de commune / culture",
    y = "Cluster évolution forêt",
    fill = "Nb communes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggsave(out_heatmap_n, p_n, width = 9, height = 6, dpi = 300, bg = "white")
cat("Heatmap effectifs sauvegardée :", out_heatmap_n, "\n")

# ============================================================
# 8) Heatmap en % par ligne
#    (pour voir, dans chaque cluster forêt,
#     comment se répartissent les clusters culture)
# ============================================================

p_pct <- ggplot(tab_cross, aes(x = cluster_culture, y = cluster_foret, fill = pct_ligne)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = paste0(round(pct_ligne, 1), "%")), size = 4) +
  scale_fill_gradient(
    low = "grey95",
    high = "darkred",
    labels = function(x) paste0(round(x), "%")
  ) +
  labs(
    title = "Croisement des clusters culture × forêt",
    subtitle = "Pourcentage au sein de chaque cluster forêt",
    x = "Cluster type de commune / culture",
    y = "Cluster évolution forêt",
    fill = "% ligne"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

ggsave(out_heatmap_pct, p_pct, width = 9, height = 6, dpi = 300, bg = "white")
cat("Heatmap pourcentages sauvegardée :", out_heatmap_pct, "\n")

# ============================================================
# 9) Affichage
# ============================================================

print(p_n)
print(p_pct)