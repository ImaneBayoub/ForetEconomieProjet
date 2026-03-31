suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
})

# ============================================================
# 1) Chemins
# ============================================================

clusters_path   <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/commune_cluster.csv"
cultures_path   <- "/home/imane/Documents/ensae/ForetEconomieProjet/FDS_G_1013/superficies_communes_2010.csv"

out_plot        <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/pie_clusters.png"
out_table       <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/parts_cultures_par_cluster_2010.csv"

out_plot_cycle  <- "/home/imane/Documents/ensae/ForetEconomieProjet/plots/pie_clusters_cycles.png"
out_table_cycle <- "/home/imane/Documents/ensae/ForetEconomieProjet/data/parts_cycles_par_cluster_2010.csv"

# ============================================================
# 2) Charger les données
# ============================================================

df_cluster <- read.csv(clusters_path, stringsAsFactors = FALSE)

if (!all(c("insee", "cluster") %in% names(df_cluster))) {
  stop("Le fichier commune_cluster.csv doit contenir les colonnes 'insee' et 'cluster'.")
}

df_cluster <- df_cluster %>%
  mutate(
    insee = str_trim(as.character(insee)),
    insee = str_pad(insee, width = 5, side = "left", pad = "0"),
    cluster = as.factor(cluster)
  )

df_cult <- read_csv(cultures_path, show_col_types = FALSE) %>%
  mutate(
    com   = str_trim(as.character(com)),
    insee = str_pad(com, width = 5, side = "left", pad = "0")
  )

# ============================================================
# 3) Sélection des grandes catégories
#    (cohérent avec le script Python corrigé)
# ============================================================

culture_cols <- c(
  "Céréales",
  "Oléagineux, protéagineux, plantes à fibres  (Total)",
  "Cultures industrielles",
  "Fourrages et superficies toujours en herbe",
  "Pommes de terre et tubercules",
  "Légumes frais, fraises, melons",
  "Vignes",
  "Cultures permanentes entretenues"
)

culture_cols <- intersect(culture_cols, names(df_cult))

if (length(culture_cols) == 0) {
  stop("Aucune colonne de culture attendue n'a été trouvée dans superficies_communes_2010.csv")
}

cat("Colonnes de culture retenues :\n")
print(culture_cols)

# ============================================================
# 4) Format long
# ============================================================

df_long <- df_cult %>%
  select(insee, all_of(culture_cols)) %>%
  pivot_longer(
    cols = all_of(culture_cols),
    names_to = "culture",
    values_to = "surface"
  ) %>%
  mutate(surface = as.numeric(surface))

# ============================================================
# 5) Diagnostics avant jointure
# ============================================================

cat("Nb communes clusters :", nrow(df_cluster), "\n")
cat("Nb INSEE uniques clusters :", n_distinct(df_cluster$insee), "\n")
cat("Nb communes cultures :", nrow(df_cult), "\n")
cat("Nb INSEE uniques cultures :", n_distinct(df_long$insee), "\n")

insee_match <- intersect(unique(df_cluster$insee), unique(df_long$insee))
cat("Nb INSEE en commun :", length(insee_match), "\n")

if (length(insee_match) == 0) {
  cat("\nExemples INSEE dans clusters :\n")
  print(head(sort(unique(df_cluster$insee)), 20))

  cat("\nExemples INSEE dans cultures :\n")
  print(head(sort(unique(df_long$insee)), 20))

  stop("Aucun code INSEE commun entre les deux fichiers.")
}

# ============================================================
# 6) Jointure avec clusters
# ============================================================

df_merge <- df_cluster %>%
  inner_join(df_long, by = "insee") %>%
  filter(!is.na(cluster), !is.na(surface))

cat("Nb lignes après jointure :", nrow(df_merge), "\n")

if (nrow(df_merge) == 0) {
  stop("La jointure a produit 0 ligne.")
}

# ============================================================
# 7) Calcul des parts par grande catégorie
# ============================================================

df_pie <- df_merge %>%
  group_by(cluster, culture) %>%
  summarise(surface = sum(surface, na.rm = TRUE), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(
    total_surface = sum(surface, na.rm = TRUE),
    pct = ifelse(total_surface > 0, 100 * surface / total_surface, NA_real_)
  ) %>%
  ungroup() %>%
  filter(!is.na(pct), is.finite(pct), pct > 0)

cat("Nb lignes table pie :", nrow(df_pie), "\n")

if (nrow(df_pie) == 0) {
  stop("df_pie est vide après agrégation.")
}

# ============================================================
# 8) Labels
# ============================================================

df_pie <- df_pie %>%
  mutate(label = ifelse(pct >= 3, paste0(round(pct, 1), "%"), ""))

# ============================================================
# 9) Export table
# ============================================================

write.csv(df_pie, out_table, row.names = FALSE)
cat("Table exportée :", out_table, "\n")

# ============================================================
# 10) Plot grandes catégories
# ============================================================

p <- ggplot(df_pie, aes(x = "", y = pct, fill = culture)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ cluster, drop = TRUE) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Composition agricole par cluster (2010)",
    subtitle = "Part de la surface agricole par grande catégorie",
    fill = "Culture"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(out_plot, p, width = 12, height = 8, dpi = 300, bg = "white")
cat("Pie charts sauvegardés :", out_plot, "\n")

# ============================================================
# 11) Agrégation cycle court / long / mixte
#    -> cohérent avec les grandes catégories conservées
# ============================================================

cycle_map <- c(
  "Céréales" = "Cycle court",
  "Oléagineux, protéagineux, plantes à fibres  (Total)" = "Cycle court",
  "Cultures industrielles" = "Cycle court",
  "Pommes de terre et tubercules" = "Cycle court",
  "Légumes frais, fraises, melons" = "Cycle court",
  "Vignes" = "Cycle long",
  "Cultures permanentes entretenues" = "Cycle long",
  "Fourrages et superficies toujours en herbe" = "Mixte"
)

df_cycle <- df_merge %>%
  mutate(cycle = recode(culture, !!!cycle_map, .default = NA_character_)) %>%
  filter(!is.na(cycle)) %>%
  group_by(cluster, cycle) %>%
  summarise(surface = sum(surface, na.rm = TRUE), .groups = "drop") %>%
  group_by(cluster) %>%
  mutate(
    total_surface = sum(surface, na.rm = TRUE),
    pct = ifelse(total_surface > 0, 100 * surface / total_surface, NA_real_),
    label = ifelse(!is.na(pct), paste0(round(pct, 1), "%"), "")
  ) %>%
  ungroup() %>%
  filter(!is.na(pct), is.finite(pct), pct > 0) %>%
  mutate(
    cycle = factor(cycle, levels = c("Cycle court", "Cycle long", "Mixte"))
  )

write.csv(df_cycle, out_table_cycle, row.names = FALSE)
cat("Table cycles exportée :", out_table_cycle, "\n")

# ============================================================
# 12) Plot cycles
# ============================================================

p_cycle <- ggplot(df_cycle, aes(x = "", y = pct, fill = cycle)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ cluster, drop = TRUE) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  labs(
    title = "Part des productions par type de cycle, par cluster (2010)",
    subtitle = "Agrégation en cycle court, cycle long et catégorie mixte",
    fill = "Type de cycle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(out_plot_cycle, p_cycle, width = 10, height = 7, dpi = 300, bg = "white")
cat("Pie charts cycles sauvegardés :", out_plot_cycle, "\n")